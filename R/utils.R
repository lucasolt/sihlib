# IBGE codes of the Brazilian states (first 2 digits of the municipality code)
codigos_uf <- c(
  RO = "11", AC = "12", AM = "13", RR = "14", PA = "15", AP = "16", TO = "17",
  MA = "21", PI = "22", CE = "23", RN = "24", PB = "25", PE = "26", AL = "27",
  SE = "28", BA = "29", MG = "31", ES = "32", RJ = "33", SP = "35", PR = "41",
  SC = "42", RS = "43", MS = "50", MT = "51", GO = "52", DF = "53"
)

# First day of the month of a date (accepts Date or "YYYY-MM-DD" text)
inicio_mes <- function(x) as.Date(format(as.Date(x), "%Y-%m-01"))

# Stops with a clear message when the googledrive package is missing
precisa_drive <- function() {
  if (!requireNamespace("googledrive", quietly = TRUE)) {
    stop("Package 'googledrive' is needed for Drive access: install.packages(\"googledrive\")", call. = FALSE)
  }
}

# Stops if a data.frame lacks any of the required columns
exigir_colunas <- function(df, colunas, nome = "base") {
  faltam <- setdiff(colunas, names(df))
  if (length(faltam) > 0) {
    stop(nome, " is missing column(s): ", paste(faltam, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}
