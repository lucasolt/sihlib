# Synthetic AIH lines (invented values, never real SIH rows).
# Builds one line per call; every field is text, like the raw cache.
linha <- function(n_aih, ident = "1", ini, fim, nasc = "19800101", sexo = "1",
                  cnes = "9999991", res = "430001", mov = "430002", espec = "05",
                  diag = "F200", cobranca = "12", morte = "0", valor = "100") {
  data.frame(N_AIH = n_aih, IDENT = ident, CNES = cnes, NASC = nasc, SEXO = sexo,
             MUNIC_RES = res, MUNIC_MOV = mov, ESPEC = espec, DIAG_PRINC = diag,
             DT_INTER = ini, DT_SAIDA = fim, COBRANCA = cobranca, MORTE = morte, VAL_TOT = valor)
}

base_sintetica <- function() {
  rbind(
    # Patient A: main AIH + continuation AIH (same N_AIH) -> one AIH, then a
    # new AIH starting 1 day after -> chained ("emendada")
    linha("A1", "1", "20250101", "20250130", cobranca = "21"),
    linha("A1", "5", "20250101", "20250210", cobranca = "12", valor = "50"),
    linha("A2", "1", "20250211", "20250220", diag = "F310"),
    # Patient B: two AIHs 10 days apart -> NOT chained (gap > 1)
    linha("B1", "1", "20250301", "20250310", nasc = "19900505", sexo = "3"),
    linha("B2", "1", "20250320", "20250325", nasc = "19900505", sexo = "3"),
    # Patient C: no birth date -> never chained, even with gap 0
    linha("C1", "1", "20250401", "20250405", nasc = ""),
    linha("C2", "1", "20250405", "20250410", nasc = ""),
    # Orphan continuation (main AIH not in base) -> dropped
    linha("D1", "5", "20250501", "20250510", nasc = "19700101")
  )
}

# Fresh empty temp folder (base R only)
withr_tempdir <- function() {
  d <- tempfile("sihlib_")
  dir.create(d)
  d
}
