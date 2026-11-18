test_that("sih_filtrar works with prefixes, ranges, specialty and dates", {
  b <- base_sintetica()
  b$DIAG_PRINC[1] <- "I10"
  expect_equal(nrow(sih_filtrar(b, cid = "F")), nrow(b) - 1)
  r <- sih_filtrar(b, cid = data.frame(grupo = c("x", "y"), de = c("I10", "F30"), ate = c("I15", "F39")))
  expect_equal(sort(r$grupo), c("x", "y"))
  expect_equal(nrow(sih_filtrar(b, espec = "87")), 0)
  expect_equal(nrow(sih_filtrar(b, data_ini = "2025-03-01", data_fim = "2025-03-31")), 2)
})

test_that("contar_municipio and taxa_municipio", {
  int <- reconstruir_internacoes(base_sintetica())
  cont <- contar_municipio(int, "residencia", anos = 2025)
  expect_equal(cont$n, 5L)
  expect_equal(cont$municipio, "430001")
  cont_oc <- contar_municipio(int, "ocorrencia", anos = 2024)
  expect_equal(nrow(cont_oc), 0)

  pop <- data.frame(municipio = c("430001", "430003"), pop = c(1000, 1000))
  tx <- taxa_municipio(cont, pop, por = 1000)
  expect_equal(tx$obs, c(5, 0))
  expect_equal(tx$taxa, c(5, 0))
  expect_equal(tx$taxa_conjunto, c(2.5, 2.5))
  expect_equal(tx$esperado, c(2.5, 2.5))
  expect_equal(tx$razao_oe, c(2, 0))
  expect_equal(tx$ic_inf[1], stats::poisson.test(5)$conf.int[1] / 2.5)
  expect_equal(tx$ic_sup[2], stats::poisson.test(0)$conf.int[2] / 2.5)
})
