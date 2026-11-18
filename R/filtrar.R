#' Filter SIH-RD lines by ICD-10, specialty and admission date
#'
#' All filters work on single AIH lines. `NULL` means "do not filter".
#'
#' @param base Data.frame from [sih_carregar()].
#' @param cid Main diagnosis (`DIAG_PRINC`) filter. Either a character vector
#'   of prefixes (e.g. `"F"` or `c("I10", "E1")`), or a data.frame of
#'   inclusive ranges with columns `grupo`, `de`, `ate` on the first 3
#'   characters (e.g. `de = "I20", ate = "I25"`). With ranges, a column
#'   `grupo` is added and lines outside every range are dropped; when ranges
#'   overlap, the last matching row wins.
#' @param espec Bed specialty codes (`ESPEC`) to keep, e.g. `c("05", "87")`.
#' @param data_ini,data_fim Admission date (`DT_INTER`) limits, inclusive
#'   (`Date` or `"YYYY-MM-DD"`).
#' @return The filtered data.frame.
#' @details Filtering by date before [reconstruir_internacoes()] can cut a
#'   chain of AIHs at the period limits. To avoid that, reconstruct with a
#'   wider period and filter the result by `ini` afterwards.
#' @examples
#' base <- data.frame(DIAG_PRINC = c("F200", "I10", "F32"), ESPEC = c("05", "03", "87"),
#'                    DT_INTER = c("20250105", "20250210", "20250320"))
#' sih_filtrar(base, cid = "F", espec = c("05", "87"))
#' sih_filtrar(base, cid = data.frame(grupo = "has", de = "I10", ate = "I15"))
#' @export
sih_filtrar <- function(base, cid = NULL, espec = NULL, data_ini = NULL, data_fim = NULL) {
  manter <- rep(TRUE, nrow(base))

  if (!is.null(cid)) {
    exigir_colunas(base, "DIAG_PRINC")
    diag <- toupper(base$DIAG_PRINC)
    if (is.data.frame(cid)) {
      exigir_colunas(cid, c("grupo", "de", "ate"), "cid")
      base$grupo <- classificar_cid(diag, cid)
      manter <- manter & !is.na(base$grupo)
    } else {
      tem_prefixo <- Reduce(`|`, lapply(toupper(cid), function(p) startsWith(diag, p)))
      manter <- manter & !is.na(tem_prefixo) & tem_prefixo
    }
  }

  if (!is.null(espec)) {
    exigir_colunas(base, "ESPEC")
    manter <- manter & base$ESPEC %in% espec
  }

  if (!is.null(data_ini) || !is.null(data_fim)) {
    exigir_colunas(base, "DT_INTER")
    dt <- as.Date(base$DT_INTER, "%Y%m%d")
    if (!is.null(data_ini)) manter <- manter & !is.na(dt) & dt >= as.Date(data_ini)
    if (!is.null(data_fim)) manter <- manter & !is.na(dt) & dt <= as.Date(data_fim)
  }

  base[manter, , drop = FALSE]
}

# Group of each ICD-10 code (first 3 characters) from a table of ranges; NA if none
classificar_cid <- function(cid, faixas) {
  cid3 <- toupper(substr(cid, 1, 3))
  grupo <- rep(NA_character_, length(cid3))
  for (i in seq_len(nrow(faixas))) {
    dentro <- !is.na(cid3) & cid3 >= toupper(faixas$de[i]) & cid3 <= toupper(faixas$ate[i])
    grupo[dentro] <- as.character(faixas$grupo[i])
  }
  grupo
}
