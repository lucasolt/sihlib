#' Load SIH-RD files with a local / Drive / DATASUS cache
#'
#' For each state (UF) and month (competência) it looks for the raw file in
#' `dir_cache`; if missing, in the Drive folder `pasta_drive`; if still
#' missing, downloads it from DATASUS with [microdatasus::fetch_datasus()],
#' saves it locally and uploads it to Drive. Months not yet published are
#' skipped.
#'
#' The cache is RAW: the whole month, all columns as text, one file per UF and
#' month named `sih_rd_<uf>_<yyyy_mm>.rds`. The only exception: SIH-RD is
#' published by the state of the HOSPITAL, so for a UF other than
#' `uf_residencia` only residents of `uf_residencia` are kept (e.g. RS
#' residents admitted in SC).
#'
#' @param ufs State abbreviations, e.g. `c("RS", "SC")`.
#' @param comp_ini,comp_fim First and last month to load (`Date` or
#'   `"YYYY-MM-DD"`; only year and month are used). An AIH may be billed months
#'   after the admission, so `comp_fim` should go beyond the analysis period.
#' @param uf_residencia IBGE code (2 digits) of the state of residence to keep
#'   in files of other states. `NULL` keeps everyone.
#' @param dir_cache Local folder for the `.rds` files (created if needed).
#' @param pasta_drive Drive folder for the cache: a name (created at the root
#'   of My Drive if needed) or a `dribble`. `NULL` (default) disables Drive.
#' @param rebaixar Months to download again even if cached, as `"yyyy_mm"`,
#'   e.g. `c("2026_03", "2026_04")`.
#' @return A data.frame with all rows stacked (all columns character). The
#'   attribute `"log"` has one row per UF and month: `uf`, `competencia`,
#'   `origem` (`"local"`, `"drive"`, `"datasus"` or `"indisponivel"`) and
#'   `linhas`.
#' @export
sih_carregar <- function(ufs, comp_ini, comp_fim, uf_residencia = "43", dir_cache,
                         pasta_drive = NULL, rebaixar = character()) {
  ufs <- toupper(ufs)
  if (any(!ufs %in% names(codigos_uf))) stop("Unknown UF: ", paste(setdiff(ufs, names(codigos_uf)), collapse = ", "))
  competencias <- seq(inicio_mes(comp_ini), inicio_mes(comp_fim), by = "month")
  dir.create(dir_cache, showWarnings = FALSE, recursive = TRUE)

  # Drive folder and its file list are read once
  usar_drive <- !is.null(pasta_drive)
  if (usar_drive) {
    precisa_drive()
    if (is.character(pasta_drive)) pasta_drive <- garantir_pasta(pasta_drive)
    arquivos_drive <- googledrive::drive_ls(pasta_drive)
  }

  dados <- list()
  log <- list()
  for (uf in ufs) {
    for (i in seq_along(competencias)) {
      comp <- competencias[i]
      rotulo <- format(comp, "%Y_%m")
      arq <- sprintf("sih_rd_%s_%s.rds", tolower(uf), rotulo)
      local <- file.path(dir_cache, arq)
      forcar <- rotulo %in% rebaixar

      if (file.exists(local) && !forcar) {
        origem <- "local"
      } else if (usar_drive && arq %in% arquivos_drive$name && !forcar) {
        googledrive::drive_download(arquivos_drive[arquivos_drive$name == arq, ][1, ],
                                    path = local, overwrite = TRUE)
        origem <- "drive"
      } else {
        # Residents filter only applies to files of other states
        filtro_res <- if (!is.null(uf_residencia) && codigos_uf[[uf]] != uf_residencia) uf_residencia else NULL
        d <- baixar_competencia(uf, comp, filtro_res)
        if (is.null(d)) {
          origem <- "indisponivel"
        } else {
          saveRDS(d, local)
          if (usar_drive) googledrive::drive_upload(local, path = pasta_drive, name = arq, overwrite = TRUE)
          origem <- "datasus"
        }
      }

      linhas <- 0L
      if (origem != "indisponivel") {
        d <- readRDS(local)
        linhas <- nrow(d)
        dados[[length(dados) + 1]] <- d
      }
      log[[length(log) + 1]] <- data.frame(uf = uf, competencia = rotulo, origem = origem, linhas = linhas)
    }
  }

  base <- if (length(dados) > 0) as.data.frame(dplyr::bind_rows(dados)) else data.frame()
  attr(base, "log") <- do.call(rbind, log)
  base
}

# Downloads one month of SIH-RD for one UF. Returns NULL when not published / failed.
baixar_competencia <- function(uf, comp, filtro_res = NULL) {
  ano <- as.integer(format(comp, "%Y"))
  mes <- as.integer(format(comp, "%m"))
  rd <- tryCatch(
    microdatasus::fetch_datasus(year_start = ano, month_start = mes, year_end = ano, month_end = mes,
                                uf = uf, information_system = "SIH-RD"),
    error = function(e) NULL
  )
  if (is.null(rd) || nrow(rd) == 0) return(NULL)
  rd <- as.data.frame(rd)
  rd[] <- lapply(rd, as.character)   # all text: avoids type clashes between months
  if (!is.null(filtro_res)) rd <- rd[substr(rd$MUNIC_RES, 1, 2) %in% filtro_res, ]
  rd
}
