test_that("continuation lines with the same N_AIH become one AIH", {
  int <- reconstruir_internacoes(base_sintetica())
  a <- int[grepl("A1", int$aihs), ]
  expect_equal(nrow(a), 1)
  expect_equal(a$n_linhas, 3)                       # 2 lines of A1 + 1 of A2
  expect_equal(a$motivos, "12 > 12")                # COBRANCA of the latest exit of each AIH
  expect_equal(a$valor, 250)
})

test_that("AIHs starting up to `gap` days after are chained", {
  int <- reconstruir_internacoes(base_sintetica(), gap = 1)
  a <- int[grepl("A1", int$aihs), ]
  expect_equal(a$aihs, "A1 > A2")
  expect_true(a$emendado)
  expect_equal(a$n_aih_principais, 2)
  expect_equal(a$ini, as.Date("2025-01-01"))
  expect_equal(a$fim, as.Date("2025-02-20"))
  expect_equal(a$dias, 50)
  expect_equal(a$diags, "F200 > F310")
})

test_that("AIHs with gap > `gap` are NOT chained", {
  int <- reconstruir_internacoes(base_sintetica(), gap = 1)
  b <- int[grepl("^B", int$aihs), ]
  expect_equal(nrow(b), 2)
  expect_false(any(b$emendado))
  # A larger gap chains them
  int10 <- reconstruir_internacoes(base_sintetica(), gap = 10)
  expect_equal(int10$aihs[grepl("^B", int10$aihs)], "B1 > B2")
})

test_that("AIHs without birth date are never chained; orphan continuations are dropped", {
  int <- reconstruir_internacoes(base_sintetica(), gap = 30)
  expect_equal(sum(grepl("^C", int$aihs)), 2)
  expect_false(any(grepl("D1", int$aihs)))
})

test_that("output keeps municipality, hospital and specialty columns", {
  int <- reconstruir_internacoes(base_sintetica())
  expect_true(all(c("MUNIC_RES", "MUNIC_MOV", "CNES", "ESPEC") %in% names(int)))
  expect_equal(nrow(int), 5)   # A, B1, B2, C1, C2
})

test_that("grupo from sih_filtrar is carried along", {
  base <- sih_filtrar(base_sintetica(), cid = data.frame(grupo = "psicoses", de = "F20", ate = "F29"))
  int <- reconstruir_internacoes(base)
  expect_true("grupo" %in% names(int))
  expect_true(all(int$grupo == "psicoses"))
})

test_that("emendar = FALSE returns one row per AIH", {
  aih <- reconstruir_internacoes(base_sintetica(), emendar = FALSE)
  expect_equal(sort(aih$aihs), c("A1", "A2", "B1", "B2", "C1", "C2"))
  expect_false(any(aih$emendado))
  expect_equal(aih$n_linhas[aih$aihs == "A1"], 2)
  expect_equal(aih$motivos[aih$aihs == "A1"], "12")
})
