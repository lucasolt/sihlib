#' Reconstruct hospital stays from AIH lines
#'
#' Same logic as the "Internações reconstruídas" section of the CID F notebook:
#'
#' 1. Lines with the same `N_AIH` (main AIH, `IDENT == "1"`, plus continuation
#'    AIHs) become one AIH. AIHs whose main line is not in `base` are dropped.
#' 2. AIHs of the same patient in the same hospital that start up to `gap`
#'    day(s) after the end of the previous one are chained into one stay
#'    ("emendadas"). The patient key is hospital + birth date + sex +
#'    municipality of residence; AIHs without birth date are never chained.
#'
#' @param base Data.frame of AIH lines (all character, as from
#'   [sih_carregar()]), usually after [sih_filtrar()]. Needs `N_AIH`, `IDENT`,
#'   `CNES`, `NASC`, `SEXO`, `MUNIC_RES`, `MUNIC_MOV`, `ESPEC`, `DIAG_PRINC`,
#'   `DT_INTER`, `DT_SAIDA`, `COBRANCA`, `MORTE` and `VAL_TOT`. A column
#'   `grupo` (from [sih_filtrar()] with ranges) is carried along if present.
#' @param gap Maximum days between the end of one AIH and the start of the next
#'   for them to be chained.
#' @return One row per stay: `chave`, `id`, `CNES`, `ESPEC`, `MUNIC_RES`,
#'   `MUNIC_MOV`, `NASC`, `SEXO`, `diag` (main diagnosis of the first AIH),
#'   `ini`, `fim`, `n_aih_principais`, `n_linhas`, `aihs` (AIH numbers joined
#'   by `" > "`), `diags` (ICD of each AIH, consecutive repeats removed),
#'   `motivos` (`COBRANCA` at the end of each AIH), `obito`, `valor`, `dias`,
#'   `emendado` and, if present, `grupo`.
#' @export
reconstruir_internacoes <- function(base, gap = 1) {
  exigir_colunas(base, c("N_AIH", "IDENT", "CNES", "NASC", "SEXO", "MUNIC_RES", "MUNIC_MOV", "ESPEC",
                         "DIAG_PRINC", "DT_INTER", "DT_SAIDA", "COBRANCA", "MORTE", "VAL_TOT"))
  tem_grupo <- "grupo" %in% names(base)
  if (!tem_grupo) base$grupo <- NA_character_

  # Step 1: one row per N_AIH
  aih <- base |>
    dplyr::mutate(ini_l = as.Date(DT_INTER, "%Y%m%d"),
                  fim_l = as.Date(DT_SAIDA, "%Y%m%d"),
                  principal = IDENT == "1") |>
    dplyr::group_by(N_AIH) |>
    dplyr::summarise(CNES = dplyr::first(CNES), NASC = dplyr::first(NASC), SEXO = dplyr::first(SEXO),
                     MUNIC_RES = dplyr::first(MUNIC_RES), MUNIC_MOV = dplyr::first(MUNIC_MOV),
                     ESPEC = dplyr::first(ESPEC),
                     diag = dplyr::first(DIAG_PRINC[principal], default = dplyr::first(DIAG_PRINC)),
                     grupo = dplyr::first(grupo[principal], default = dplyr::first(grupo)),
                     ini = suppressWarnings(min(ini_l, na.rm = TRUE)),
                     fim = suppressWarnings(max(fim_l, na.rm = TRUE)),
                     tem_principal = any(principal),
                     n_linhas = dplyr::n(),
                     cobranca_final = if (all(is.na(fim_l))) NA_character_ else COBRANCA[which.max(fim_l)],
                     obito = any(MORTE == "1"),
                     valor = sum(suppressWarnings(as.numeric(VAL_TOT)), na.rm = TRUE),
                     .groups = "drop") |>
    dplyr::filter(tem_principal) |>   # only AIHs whose main line is in base
    dplyr::mutate(chave = ifelse(is.na(NASC) | NASC == "", paste0("semchave_", N_AIH),
                                 paste(CNES, NASC, SEXO, MUNIC_RES)))

  # Step 2: chain AIHs that start up to `gap` day(s) after the previous end
  int <- aih |>
    dplyr::arrange(chave, ini, fim) |>
    dplyr::group_by(chave) |>
    dplyr::mutate(fim_ant = dplyr::lag(cummax(as.numeric(fim))),   # latest end up to the previous AIH
                  novo = is.na(fim_ant) | as.numeric(ini) - fim_ant > gap,
                  id = cumsum(novo)) |>
    dplyr::group_by(chave, id) |>
    dplyr::summarise(# diags must come before `diag` is overwritten by its first value
                     diags = paste(rle(diag)$values, collapse = " > "),
                     CNES = dplyr::first(CNES), ESPEC = dplyr::first(ESPEC),
                     MUNIC_RES = dplyr::first(MUNIC_RES), MUNIC_MOV = dplyr::first(MUNIC_MOV),
                     NASC = dplyr::first(NASC), SEXO = dplyr::first(SEXO),
                     diag = dplyr::first(diag), grupo = dplyr::first(grupo),
                     ini = min(ini), fim = max(fim),
                     n_aih_principais = dplyr::n(), n_linhas = sum(n_linhas),
                     aihs = paste(N_AIH, collapse = " > "),
                     motivos = paste(cobranca_final, collapse = " > "),
                     obito = any(obito), valor = sum(valor),
                     .groups = "drop") |>
    dplyr::mutate(dias = as.numeric(fim - ini),
                  emendado = n_aih_principais > 1) |>
    dplyr::relocate(diags, .after = aihs)

  if (!tem_grupo) int$grupo <- NULL
  as.data.frame(int)
}
