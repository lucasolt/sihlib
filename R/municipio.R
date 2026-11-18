#' Count stays by municipality and year
#'
#' @param int Data.frame from [reconstruir_internacoes()].
#' @param por `"residencia"` (municipality of residence, `MUNIC_RES`) or
#'   `"ocorrencia"` (municipality of the hospital, `MUNIC_MOV`).
#' @param anos Years to keep, by the year of admission (`ini`).
#' @return Data.frame with `municipio` (6-digit IBGE code), `ano`, `n` and,
#'   if `int` has it, `grupo`. Municipality-years without stays are not
#'   listed. The attribute `"anos"` keeps `anos`, used by [taxa_municipio()].
#' @export
contar_municipio <- function(int, por = c("residencia", "ocorrencia"), anos) {
  por <- match.arg(por)
  coluna <- if (por == "residencia") "MUNIC_RES" else "MUNIC_MOV"
  exigir_colunas(int, c(coluna, "ini"), "int")

  d <- data.frame(municipio = substr(as.character(int[[coluna]]), 1, 6),
                  ano = as.integer(format(as.Date(int$ini), "%Y")))
  chaves <- c("municipio", "ano")
  if ("grupo" %in% names(int)) {
    d$grupo <- int$grupo
    chaves <- c(chaves, "grupo")
  }

  contagens <- d |>
    dplyr::filter(ano %in% anos) |>
    dplyr::count(dplyr::across(dplyr::all_of(chaves))) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(chaves))) |>
    as.data.frame()
  attr(contagens, "anos") <- sort(unique(as.integer(anos)))
  contagens
}

#' Rate by municipality with observed/expected ratio
#'
#' Sums the counts of all years in the period and divides by person-years.
#' The expected count uses the rate of the whole set of municipalities in
#' `pop` (indirect standardisation without strata). The 95% interval of the
#' O/E ratio is the exact Poisson interval for the observed count divided by
#' the expected count.
#'
#' @param contagens Data.frame from [contar_municipio()] (`municipio`, `ano`,
#'   `n`, optionally `grupo`; results are then computed per `grupo`).
#' @param pop Data.frame with `municipio` and `pop`. With a column `ano`,
#'   person-years are the sum of `pop` over the years of the period; without
#'   it, `pop` is multiplied by the number of years. Municipalities in `pop`
#'   without counts get 0 events; counts of municipalities not in `pop` are
#'   dropped (with a message).
#' @param por Rate multiplier (per 10,000 by default).
#' @return Data.frame with `municipio`, (`grupo`), `obs`, `pessoa_anos`,
#'   `taxa`, `taxa_conjunto`, `esperado`, `razao_oe`, `ic_inf`, `ic_sup`.
#' @examples
#' cont <- data.frame(municipio = c("430010", "430020"), ano = 2024L, n = c(5L, 20L))
#' pop <- data.frame(municipio = c("430010", "430020", "430030"), pop = c(1000, 2000, 500))
#' taxa_municipio(cont, pop)
#' @export
taxa_municipio <- function(contagens, pop, por = 10000) {
  exigir_colunas(contagens, c("municipio", "ano", "n"), "contagens")
  exigir_colunas(pop, c("municipio", "pop"), "pop")
  anos <- attr(contagens, "anos")
  if (is.null(anos)) anos <- sort(unique(contagens$ano))
  contagens$municipio <- substr(as.character(contagens$municipio), 1, 6)
  pop$municipio <- substr(as.character(pop$municipio), 1, 6)

  # Person-years per municipality
  if ("ano" %in% names(pop)) {
    pa <- pop[pop$ano %in% anos, ] |>
      dplyr::summarise(pessoa_anos = sum(pop), .by = municipio)
  } else {
    pa <- pop |>
      dplyr::summarise(pessoa_anos = sum(pop) * length(anos), .by = municipio)
  }

  fora <- !contagens$municipio %in% pa$municipio
  if (any(fora)) message(sum(contagens$n[fora]), " event(s) in municipalities not in `pop` were dropped")

  # Observed per municipality (and group), with zeros for municipalities without events
  grupos <- if ("grupo" %in% names(contagens)) unique(contagens$grupo) else NULL
  obs <- contagens[!fora, ] |>
    dplyr::summarise(obs = sum(n), .by = dplyr::any_of(c("municipio", "grupo")))
  grade <- if (is.null(grupos)) pa else merge(pa, data.frame(grupo = grupos))

  res <- grade |>
    dplyr::left_join(obs, by = intersect(c("municipio", "grupo"), names(grade))) |>
    dplyr::mutate(obs = dplyr::coalesce(as.numeric(obs), 0)) |>
    dplyr::mutate(taxa_conjunto = sum(obs) / sum(pessoa_anos) * por, .by = dplyr::any_of("grupo")) |>
    dplyr::mutate(taxa = obs / pessoa_anos * por,
                  esperado = pessoa_anos * taxa_conjunto / por,
                  razao_oe = obs / esperado,
                  # Exact Poisson interval for the observed count, divided by the expected
                  ic_inf = ifelse(obs == 0, 0, stats::qchisq(0.025, 2 * obs) / 2) / esperado,
                  ic_sup = stats::qchisq(0.975, 2 * (obs + 1)) / 2 / esperado)

  cols <- c("municipio", if (!is.null(grupos)) "grupo",
            "obs", "pessoa_anos", "taxa", "taxa_conjunto", "esperado", "razao_oe", "ic_inf", "ic_sup")
  res <- res[, cols]
  res <- res[do.call(order, res[intersect(c("grupo", "municipio"), names(res))]), ]
  rownames(res) <- NULL
  res
}
