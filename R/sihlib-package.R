#' @keywords internal
"_PACKAGE"

# Column names used inside dplyr verbs (silences R CMD check notes)
utils::globalVariables(c(
  "N_AIH", "IDENT", "DT_INTER", "DT_SAIDA", "DIAG_PRINC", "COBRANCA", "MORTE", "VAL_TOT",
  "CNES", "NASC", "SEXO", "MUNIC_RES", "MUNIC_MOV", "ESPEC", "grupo",
  "ini_l", "fim_l", "principal", "tem_principal", "cobranca_final", "chave",
  "ini", "fim", "fim_ant", "novo", "id", "diag", "obito", "valor", "n_linhas",
  "n_aih_principais", "diags", "aihs", "municipio", "ano", "n", "obs", "pop", "pessoa_anos", "esperado",
  "taxa_conjunto"
))
