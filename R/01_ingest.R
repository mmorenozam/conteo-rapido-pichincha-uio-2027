# Phase 1: ingest.
#
# Reads the raw inputs into tidy tibbles and checks them against the official CNE
# files. No cleaning rule is applied here: anything that changes or drops data
# belongs in R/02_clean.R. Every check that can fail stops the pipeline with a
# message that says what differed.
#
# Inputs (all in data-raw/, read-only):
#   cne2023_prefecto_pichincha_juntas.csv, cne2023_alcalde_quito_juntas.csv
#       junta-level results scraped from the CNE site (one row per junta x candidate)
#   seccionales-2023/Resultados-electorales-2023.sav
#       official results by parroquia x sex x candidate (the reference for validation)
#   seccionales-2023/parroquias.sav
#       parroquia catalog with the urban / rural flag (PARROQUIA_ESTADO)
#   seccionales-2023/Distributivo Nacional de Recintos Electorales 08-08-2022.xlsx
#   seccionales-2026/Distirbutivo-Recintos-Elecciones-Seccionales-y-CPCCS-2027-110826.xlsx
#       recinto distributivos with the junta ranges by sex

PROVINCIA_PICHINCHA <- 17L
CANTON_QUITO <- 60L

# Columns that describe the junta (one value per junta), as opposed to the
# candidate columns (one value per junta x candidate).
COLS_CANDIDATO <- c("op_codigo", "op_lista", "op_orden", "op_nombre", "candidato", "votos")

# ---- junta-level results scraped from the CNE site ------------------------------

#' Read a consolidated junta-level results file.
#'
#' @param path Path to `data-raw/cne20XX_<dignidad>_juntas.csv`.
#' @return Tibble, one row per junta x candidate. Codes are integers, `sexo` is
#'   "F" or "M", `recinto_codigo` is NA for the special juntas that have no recinto.
read_juntas_web <- function(path) {
  x <- readr::read_csv(
    path,
    col_types = readr::cols(
      .default = readr::col_integer(),
      parroquia = readr::col_character(),
      zona = readr::col_character(),
      recinto_codigo = readr::col_double(), # written as 1402.0 by the consolidation script
      recinto = readr::col_character(),
      recinto_u_r = readr::col_character(),
      junta = readr::col_character(),
      sexo = readr::col_character(),
      op_lista = readr::col_character(),
      op_orden = readr::col_character(),
      op_nombre = readr::col_character(),
      candidato = readr::col_character()
    ),
    locale = readr::locale(encoding = "UTF-8"),
    progress = FALSE
  )
  probs <- readr::problems(x)
  if (nrow(probs) > 0) {
    cli::cli_abort("{.file {path}}: {nrow(probs)} parsing problem{?s}, first in row {probs$row[1]} column {probs$col[1]}.")
  }
  x |>
    dplyr::mutate(
      recinto_codigo = as.integer(recinto_codigo),
      candidato = stringr::str_squish(candidato)
    )
}

#' One row per junta: the junta columns of a junta x candidate table.
juntas_header <- function(juntas) {
  juntas |>
    dplyr::select(-dplyr::any_of(COLS_CANDIDATO)) |>
    dplyr::distinct()
}

#' Structural checks on a junta-level table. Stops on failure, returns a summary.
#'
#' Checks: one set of junta values per junta_id (no conflicting duplicates), no
#' duplicated junta x candidate, sex is F or M, no missing votes, every junta has
#' the same number of candidates, no negative counts.
assert_juntas_ok <- function(juntas, label) {
  hdr <- juntas_header(juntas)
  n_cand <- dplyr::count(juntas, junta_id, name = "k")
  fails <- c(
    "junta_id with conflicting junta values" = anyDuplicated(hdr$junta_id) > 0,
    "duplicated junta x candidate" = anyDuplicated(juntas[c("junta_id", "candidato")]) > 0,
    "sexo not F/M" = !all(hdr$sexo %in% c("F", "M")),
    "missing votos" = anyNA(juntas$votos),
    "juntas with different number of candidates" = dplyr::n_distinct(n_cand$k) != 1,
    "negative counts" = any(c(juntas$votos, hdr$sufragantes, hdr$blancos, hdr$nulos) < 0, na.rm = TRUE)
  )
  if (any(fails)) {
    cli::cli_abort(c("{label}: junta table failed structural checks.", stats::setNames(names(fails)[fails], rep("x", sum(fails)))))
  }
  # Values are computed before tibble(): inside tibble() a new column called
  # `juntas` would mask the argument `juntas` for the columns that follow it.
  n_filas <- nrow(juntas)
  votos_validos <- sum(juntas$votos)
  tibble::tibble(
    fuente = label,
    filas = n_filas,
    juntas = nrow(hdr),
    parroquias = dplyr::n_distinct(hdr$parroquia_codigo),
    candidatos = n_cand$k[1],
    votos_validos = votos_validos,
    sufragantes = sum(hdr$sufragantes),
    blancos = sum(hdr$blancos),
    nulos = sum(hdr$nulos)
  )
}

# ---- official SPSS results ---------------------------------------------------------

#' Read the official CNE results file (SPSS) with labels removed (codes stay numeric).
read_resultados_spss <- function(path) {
  haven::read_sav(path) |>
    haven::zap_labels() |>
    tibble::as_tibble()
}

#' Official results for one dignidad, in the same units as the junta table.
#'
#' @param spss Output of [read_resultados_spss()].
#' @param dignidad CNE dignidad code (2023: 11 Prefecto, 13 Alcalde).
#' @param canton Optional canton code (60 for Alcalde Quito).
#' @return Tibble, one row per parroquia x sex x candidate.
spss_dignidad <- function(spss, dignidad, provincia = PROVINCIA_PICHINCHA, canton = NULL) {
  out <- dplyr::filter(spss, DIGNIDAD_CODIGO == dignidad, PROVINCIA_CODIGO == provincia)
  if (!is.null(canton)) out <- dplyr::filter(out, CANTON_CODIGO == canton)
  if (nrow(out) == 0) cli::cli_abort("No SPSS rows for dignidad {dignidad}, provincia {provincia}.")
  out |>
    dplyr::transmute(
      canton = as.integer(CANTON_CODIGO),
      parroquia_codigo = as.integer(PARROQUIA_CODIGO),
      sexo = substr(toupper(as.character(JUNTA_SEXO)), 1, 1), # FEMENINO / MASCULINO
      candidato = stringr::str_squish(as.character(CANDIDATO_NOMBRE)),
      sufragantes = as.integer(SUFRAGANTES),
      blancos = as.integer(BLANCOS),
      nulos = as.integer(NULOS),
      votos = as.integer(VOTOS)
    )
}

#' Compare a junta-level table with the official SPSS file.
#'
#' Votes are compared at parroquia x sex x candidate; sufragantes, blancos and
#' nulos at parroquia x sex (in the SPSS file they repeat on every candidate row).
#' @return List with the cell table, the header table and counts of differences.
compare_with_spss <- function(juntas, spss) {
  keys <- c("parroquia_codigo", "sexo", "candidato")
  cells <- dplyr::full_join(
    dplyr::summarise(spss, spss = sum(votos), .by = dplyr::all_of(keys)),
    dplyr::summarise(juntas, web = sum(votos), .by = dplyr::all_of(keys)),
    by = keys
  ) |>
    dplyr::mutate(igual = dplyr::coalesce(spss == web, FALSE))

  spss_hdr <- spss |>
    dplyr::summarise(
      dplyr::across(c(sufragantes, blancos, nulos), list(spss = dplyr::first, n = dplyr::n_distinct)),
      .by = c(parroquia_codigo, sexo)
    )
  if (any(spss_hdr$sufragantes_n > 1 | spss_hdr$blancos_n > 1 | spss_hdr$nulos_n > 1)) {
    cli::cli_abort("SPSS file: sufragantes, blancos or nulos vary within a parroquia x sex.")
  }
  hdr <- dplyr::full_join(
    dplyr::select(spss_hdr, parroquia_codigo, sexo, dplyr::ends_with("_spss")),
    juntas_header(juntas) |>
      dplyr::summarise(dplyr::across(c(sufragantes, blancos, nulos), sum, .names = "{.col}_web"),
                       .by = c(parroquia_codigo, sexo)),
    by = c("parroquia_codigo", "sexo")
  ) |>
    dplyr::mutate(igual = dplyr::coalesce(
      sufragantes_spss == sufragantes_web & blancos_spss == blancos_web & nulos_spss == nulos_web, FALSE))

  list(
    celdas = cells,
    cabecera = hdr,
    n_celdas = nrow(cells),
    n_celdas_distintas = sum(!cells$igual),
    n_cabecera_distintas = sum(!hdr$igual),
    votos_validos_spss = sum(cells$spss, na.rm = TRUE),
    votos_validos_web = sum(cells$web, na.rm = TRUE)
  )
}

#' Stop the pipeline unless the junta table equals the SPSS file exactly.
assert_matches_spss <- function(cmp, label) {
  if (cmp$n_celdas_distintas > 0 || cmp$n_cabecera_distintas > 0) {
    bad <- dplyr::filter(cmp$celdas, !igual) |> utils::head(5)
    cli::cli_abort(c(
      "{label}: scraped data differ from the SPSS file.",
      "x" = "{cmp$n_celdas_distintas} of {cmp$n_celdas} parroquia x sex x candidate cells differ.",
      "x" = "{cmp$n_cabecera_distintas} parroquia x sex rows differ in sufragantes, blancos or nulos.",
      "i" = "First differing cells: {paste(bad$parroquia_codigo, bad$sexo, bad$candidato, collapse = '; ')}"
    ))
  }
  cmp
}

# ---- parroquia catalog --------------------------------------------------------------

#' Parroquias of Pichincha with the CNE urban / rural flag.
#' @return Tibble: canton, parroquia_codigo, parroquia, estado ("U" or "R").
read_parroquias <- function(path, provincia = PROVINCIA_PICHINCHA) {
  x <- haven::read_sav(path)
  tibble::tibble(
    provincia = as.integer(haven::zap_labels(x$PROVINCIA_CODIGO)),
    canton = as.integer(haven::zap_labels(x$CANTON_CODIGO)),
    parroquia_codigo = as.integer(haven::zap_labels(x$PARROQUIA_CODIGO)),
    parroquia = as.character(haven::as_factor(x$PARROQUIA_NOMBRE, levels = "labels")),
    estado = as.character(haven::zap_labels(x$PARROQUIA_ESTADO))
  ) |>
    dplyr::filter(provincia == !!provincia) |>
    dplyr::select(-provincia)
}

# ---- recinto distributivos ------------------------------------------------------------

#' Read a CNE recinto distributivo (2023 or 2027 layout) for one province.
#'
#' Column names are normalized with janitor, so "JUN INIF" (2027) and "JUN_INIF"
#' (2023) both become `jun_inif`. Columns missing in a given year (U/R in 2027;
#' CDA and access flags in 2023) are returned as NA.
#'
#' @param sheet Sheet name or position. 2023: 3 (" Distributivo Nacional"); 2027: "Consolidado".
#' @param skip Rows above the header. Both files: 7.
#' @return Tibble, one row per recinto.
read_distributivo <- function(path, sheet, skip, anio, provincia = PROVINCIA_PICHINCHA) {
  x <- readxl::read_excel(path, sheet = sheet, skip = skip, guess_max = 50000, .name_repair = "minimal") |>
    janitor::clean_names()
  for (nm in c("u_r", "cda", "dificil_acceso", "sin_conectividad", "numero_electores", "lat", "long")) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }
  need <- c("codigo_provincia", "codigo_canton", "nombre_canton", "codigo_parroquia", "nombre_parroquia",
            "codigo_zona", "nombre_zona", "codigo_recinto", "nombre_recinto",
            "jun_fem", "jun_mas", "num_junr", "jun_inif", "jun_finf", "jun_inim", "jun_finm")
  miss <- setdiff(need, names(x))
  if (length(miss)) cli::cli_abort("{.file {path}}: missing column{?s} {miss}.")

  x |>
    dplyr::filter(as.integer(codigo_provincia) == !!provincia) |>
    dplyr::transmute(
      anio = as.integer(anio),
      canton = as.integer(codigo_canton), canton_nombre = nombre_canton,
      parroquia_codigo = as.integer(codigo_parroquia), parroquia = nombre_parroquia,
      zona_codigo = as.integer(codigo_zona), zona = nombre_zona,
      recinto_codigo = as.integer(codigo_recinto), recinto = nombre_recinto,
      u_r = as.character(u_r),
      juntas_f = as.integer(jun_fem), juntas_m = as.integer(jun_mas), juntas = as.integer(num_junr),
      jun_inif = as.integer(jun_inif), jun_finf = as.integer(jun_finf),
      jun_inim = as.integer(jun_inim), jun_finm = as.integer(jun_finm),
      electores = as.integer(numero_electores),
      cda = as.character(cda),
      dificil_acceso = as.character(dificil_acceso),
      sin_conectividad = as.character(sin_conectividad),
      lat = as.numeric(lat), long = as.numeric(long)
    )
}

#' Expand the junta ranges of a distributivo to one row per junta.
#'
#' Ranges with a start of 0 or missing are skipped (one such record in 2023:
#' recinto 6208, Chillogallo, women's range 0 to 0); they are reported by
#' [invalid_junta_ranges()].
#' @return Tibble: parroquia_codigo, zona_codigo, recinto_codigo, sexo, junta_num.
expand_junta_ranges <- function(dist) {
  dplyr::bind_rows(
    dplyr::transmute(dist, parroquia_codigo, zona_codigo, recinto_codigo, sexo = "F", ini = jun_inif, fin = jun_finf),
    dplyr::transmute(dist, parroquia_codigo, zona_codigo, recinto_codigo, sexo = "M", ini = jun_inim, fin = jun_finm)
  ) |>
    dplyr::filter(!is.na(ini), !is.na(fin), ini > 0, fin >= ini) |>
    dplyr::mutate(junta_num = purrr::map2(ini, fin, seq.int)) |>
    tidyr::unnest(junta_num) |>
    dplyr::mutate(junta_num = as.integer(junta_num)) |>
    dplyr::select(parroquia_codigo, zona_codigo, recinto_codigo, sexo, junta_num)
}

#' Recintos whose declared number of juntas does not match their junta ranges.
invalid_junta_ranges <- function(dist) {
  n_rng <- function(ini, fin) dplyr::if_else(!is.na(ini) & ini > 0 & !is.na(fin) & fin >= ini, fin - ini + 1L, 0L)
  dist |>
    dplyr::mutate(rango_f = n_rng(jun_inif, jun_finf), rango_m = n_rng(jun_inim, jun_finm)) |>
    dplyr::filter(rango_f != dplyr::coalesce(juntas_f, 0L) | rango_m != dplyr::coalesce(juntas_m, 0L)) |>
    dplyr::select(recinto_codigo, recinto, parroquia, juntas_f, jun_inif, jun_finf, juntas_m, jun_inim, jun_finm)
}

#' Link juntas to recintos through the distributivo ranges and compare with the
#' recinto already in the consolidated file.
#'
#' The key is parroquia x zona x sex x junta number, which is how the CNE site
#' numbers juntas. Keys that appear in more than one recinto are not used.
check_recinto_link <- function(juntas, rangos) {
  keys <- c("parroquia_codigo", "zona_codigo", "sexo", "junta_num")
  dup <- dplyr::count(rangos, dplyr::across(dplyr::all_of(keys))) |> dplyr::filter(n > 1)
  r1 <- dplyr::anti_join(rangos, dup, by = keys)
  hdr <- juntas_header(juntas)
  linked <- dplyr::left_join(hdr, dplyr::rename(r1, recinto_rango = recinto_codigo), by = keys)
  differs <- is.na(linked$recinto_rango) != is.na(linked$recinto_codigo) |
    dplyr::coalesce(linked$recinto_rango != linked$recinto_codigo, FALSE)
  list(
    juntas = nrow(hdr),
    juntas_con_recinto = sum(!is.na(linked$recinto_rango)),
    recintos = dplyr::n_distinct(linked$recinto_rango, na.rm = TRUE),
    distinto_del_archivo = sum(differs),
    rangos_duplicados = nrow(dup),
    sin_recinto = linked |>
      dplyr::filter(is.na(recinto_rango)) |>
      dplyr::select(parroquia_codigo, parroquia, zona_codigo, zona, junta, sufragantes),
    sin_resultado = r1 |>
      dplyr::filter(parroquia_codigo %in% hdr$parroquia_codigo) |>
      dplyr::anti_join(hdr, by = keys)
  )
}

#' Stop unless every junta of the distributivo has a result, the link agrees with
#' the consolidated file, and the only juntas without recinto are special juntas
#' (home voting and prisons, which the CNE puts in their own zonas).
assert_recinto_link <- function(link, label, zonas_especiales = c(61L, 89L)) {
  otros <- dplyr::filter(link$sin_recinto, !zona_codigo %in% zonas_especiales)
  fails <- c(
    "link differs from the consolidated file" = link$distinto_del_archivo > 0,
    "distributivo juntas without result" = nrow(link$sin_resultado) > 0,
    "juntas without recinto outside the special zonas" = nrow(otros) > 0,
    "junta ranges shared by two recintos" = link$rangos_duplicados > 0
  )
  if (any(fails)) {
    cli::cli_abort(c("{label}: recinto link failed.", stats::setNames(names(fails)[fails], rep("x", sum(fails)))))
  }
  link
}

# ---- summary for the report ---------------------------------------------------------

#' One row per junta-level source: size, totals, SPSS comparison and recinto link.
ingest_summary <- function(checks, spss_cmps, links) {
  purrr::pmap_dfr(list(checks, spss_cmps, links), function(ck, cmp, lk) {
    dplyr::mutate(ck,
      celdas_spss = cmp$n_celdas,
      celdas_distintas = cmp$n_celdas_distintas,
      juntas_con_recinto = lk$juntas_con_recinto,
      recintos = lk$recintos,
      juntas_sin_recinto = nrow(lk$sin_recinto)
    )
  })
}

#' Recintos, juntas and electores of a distributivo, by canton and in total.
#' (The count of large recintos is computed first, before `juntas` is summed.)
distributivo_summary <- function(dist) {
  by_canton <- dist |>
    dplyr::summarise(recintos_mas_32_juntas = sum(juntas > 32), recintos = dplyr::n(),
                     juntas = sum(juntas), electores = sum(electores), .by = c(anio, canton, canton_nombre))
  dplyr::bind_rows(
    by_canton,
    dist |>
      dplyr::summarise(recintos_mas_32_juntas = sum(juntas > 32), recintos = dplyr::n(),
                       juntas = sum(juntas), electores = sum(electores), .by = anio) |>
      dplyr::mutate(canton_nombre = "TOTAL PICHINCHA")
  )
}
