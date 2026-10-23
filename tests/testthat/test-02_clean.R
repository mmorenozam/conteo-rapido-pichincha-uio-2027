# Tests for R/02_clean.R.

mk <- function() tibble::tibble(
  parroquia_codigo = 1L, parroquia = "P1",
  zona_codigo = rep(c(0L, 0L, 61L), each = 2), zona = rep(c("Z", "Z", "CRS NÂº 2"), each = 2),
  recinto_codigo = rep(c(10L, 10L, NA), each = 2), recinto = rep(c("A", "A", NA), each = 2),
  junta_id = rep(1:3, each = 2), junta = rep(c("1F", "1M", "1M"), each = 2),
  junta_num = 1L, sexo = rep(c("F", "M", "M"), each = 2),
  sufragantes = rep(c(10L, 9L, 3L), each = 2), ausentismo = 2L,
  blancos = 1L, nulos = 1L, candidato = rep(c("X", "Y"), 3), votos = c(5L, 3L, 4L, 4L, 1L, 0L)
)
# junta 1: 5+3+1+1 = 10 = sufragantes (consistent); junta 2: 4+4+1+1 = 10 vs 9 (gap +1); junta 3: special

test_that("encoding rule repairs the doubly encoded text", {
  r <- rule_fix_encoding(mk(), "t")
  expect_false(any(grepl("Â", r$data$zona)))
  expect_equal(r$log$juntas_afectadas, 1L)
})

test_that("special juntas are excluded and returned", {
  r <- rule_exclude_special(mk(), "t")
  expect_equal(sort(unique(r$data$junta_id)), 1:2)
  expect_equal(unique(r$excluidas$junta_id), 3L)
  expect_equal(r$log$juntas_afectadas, 1L)
})

test_that("inconsistent actas are flagged, not changed", {
  r <- rule_flag_inconsistent(rule_exclude_special(mk(), "t")$data, "t")
  h <- juntas_header(r$data)
  expect_equal(h$acta_inconsistente, c(FALSE, TRUE))
  expect_equal(h$brecha_acta, c(0L, 1L))
  expect_equal(sum(r$data$votos), 16L)
  expect_equal(r$log$juntas_afectadas, 1L)
})

test_that("clean_juntas returns consistent tables and a log per rule", {
  cl <- assert_clean_ok(clean_juntas(mk(), "t"), "t")
  expect_equal(nrow(cl$juntas), 2)
  expect_equal(nrow(cl$votos), 4)
  expect_equal(nrow(cl$excluidas), 1)
  expect_equal(cl$log$regla, names(RULES_2023))
  expect_equal(cl$juntas$electores, c(12L, 11L))
})

test_that("2023: cleaning gives the expected counts", {
  f <- raw_path("cne2023_prefecto_pichincha_juntas.csv")
  skip_if_not(file.exists(f), "2023 raw files not found")
  j <- read_juntas_web(f)
  cl <- assert_clean_ok(clean_juntas(j, "Prefecto 2023"), "Prefecto 2023")
  expect_equal(nrow(cl$juntas), 6892)
  expect_equal(nrow(cl$votos), 82704)
  expect_equal(nrow(cl$excluidas), 11)
  expect_equal(sum(cl$excluidas$sufragantes), 84)
  expect_equal(sum(cl$juntas$acta_inconsistente), 910)
  expect_equal(sum(cl$juntas$brecha_acta), -557)
  expect_equal(sum(cl$juntas$electores), 2332694)
  expect_lt(max(abs(exclusion_effect(j, cl)$diferencia_pp)), 0.001)

  a <- read_juntas_web(raw_path("cne2023_alcalde_quito_juntas.csv"))
  ca <- assert_clean_ok(clean_juntas(a, "Alcalde Quito 2023"), "Alcalde Quito 2023")
  expect_equal(nrow(ca$juntas), 5939)
  expect_equal(sum(ca$juntas$acta_inconsistente), 829)
  expect_equal(sum(ca$juntas$brecha_acta), -581)
})
