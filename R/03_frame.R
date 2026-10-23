# Phase 3: sampling frame.
#
# The frame is the 2027 recinto distributivo (cut-off 11 Aug 2026), one row per
# recinto, with its stratum, its link to the 2023 recinto of the same code, and
# (for a given allocation) the certainty flag for recintos larger than the
# systematic PPS interval of their stratum.
#
# Strata (approved proposal, October 2026):
#   Quito urbano, Quito rural (by the CNE urban / rural flag of the parroquia),
#   Rumiñahui, Mejía, Cayambe y Pedro Moncayo, Noroccidente (S. M. de los Bancos,
#   P. V. Maldonado, Puerto Quito).

# Non-ASCII letters are written as \u escapes so the file sources the same on any system.
ESTRATOS <- c("Quito urbano", "Quito rural", "Rumiñahui", "Mejía",
              "Cayambe y Pedro Moncayo", "Noroccidente")
ESTRATO_CANTON <- c(
  "80" = "Rumiñahui", "70" = "Mejía",
  "65" = "Cayambe y Pedro Moncayo", "75" = "Cayambe y Pedro Moncayo",
  "770" = "Noroccidente", "895" = "Noroccidente", "896" = "Noroccidente"
)

# ---- parroquia codes ---------------------------------------------------------------------

#' Parroquias whose CNE code changed between 2023 and 2027.
#'
#' Juan Montalvo (Cayambe) is 5495 in the 2023 catalog, results and distributivo
#' and 4765 in the 2025 and 2027 distributivos (the 2025 results site still lists
#' it as 5495 but stores its juntas under 4765). Same recinto (816) in both years.
parroquia_crosswalk <- function() {
  tibble::tibble(
    parroquia_codigo_2023 = 5495L,
    parroquia_codigo_2027 = 4765L,
    nombre = "JUAN MONTALVO",
    nota = "Código CNE cambia entre 2023 (5495) y 2025/2027 (4765); mismo recinto 816"
  )
}

#' Parroquia catalog with the 2027 codes added for parroquias whose code changed.
parroquias_frame <- function(parroquias, crosswalk = parroquia_crosswalk()) {
  extra <- crosswalk |>
    dplyr::inner_join(parroquias, by = c("parroquia_codigo_2023" = "parroquia_codigo")) |>
    dplyr::transmute(canton, parroquia_codigo = parroquia_codigo_2027, parroquia, estado)
  dplyr::bind_rows(parroquias, dplyr::anti_join(extra, parroquias, by = "parroquia_codigo"))
}

# ---- frame -----------------------------------------------------------------------------------

#' Assign each recinto of a distributivo to its stratum.
#'
#' Quito recintos are urban or rural by the CNE flag of their parroquia (in 2023
#' this flag agrees with the recinto-level U/R flag of the distributivo for all
#' 366 Quito recintos). The other strata are groups of cantons.
assign_strata <- function(dist, parroquias) {
  x <- dist |>
    dplyr::left_join(dplyr::select(parroquias, parroquia_codigo, estado_parroquia = estado), by = "parroquia_codigo") |>
    dplyr::mutate(estrato = dplyr::case_when(
      canton == CANTON_QUITO & estado_parroquia == "U" ~ ESTRATOS[1],
      canton == CANTON_QUITO & estado_parroquia == "R" ~ ESTRATOS[2],
      canton != CANTON_QUITO ~ unname(ESTRATO_CANTON[as.character(canton)]),
      .default = NA_character_
    ))
  if (anyNA(x$estrato)) {
    bad <- dplyr::filter(x, is.na(estrato)) |> dplyr::distinct(canton, parroquia_codigo, parroquia)
    cli::cli_abort(c("Recintos without stratum (unknown canton or Quito parroquia without U/R flag):",
                     "x" = "{paste(bad$canton, bad$parroquia_codigo, bad$parroquia, collapse = '; ')}"))
  }
  dplyr::mutate(x, estrato = factor(estrato, levels = ESTRATOS))
}

#' Link each 2027 recinto to the 2023 recinto with the same code.
#'
#' `historia`: "igual" (same code, parroquia and number of juntas), "cambia_juntas"
#' (same code and parroquia, different number of juntas), "cambia_parroquia",
#' "nuevo" (code not in 2023). Phase 4 uses stratum-level parameters, so recintos
#' without history are not a problem for the sample size; they matter for the
#' simulation, which is run on the 2023 recintos.
link_history <- function(frame, dist_2023, crosswalk = parroquia_crosswalk()) {
  h <- dplyr::transmute(dist_2023, recinto_codigo, parroquia_codigo_2023 = parroquia_codigo,
                        juntas_2023 = juntas, recinto_2023 = recinto)
  frame |>
    dplyr::left_join(h, by = "recinto_codigo") |>
    dplyr::mutate(
      parroquia_2023_en_2027 = dplyr::coalesce(
        crosswalk$parroquia_codigo_2027[match(parroquia_codigo_2023, crosswalk$parroquia_codigo_2023)],
        parroquia_codigo_2023),
      historia = dplyr::case_when(
        is.na(juntas_2023) ~ "nuevo",
        parroquia_2023_en_2027 != parroquia_codigo ~ "cambia_parroquia",
        juntas_2023 == juntas ~ "igual",
        .default = "cambia_juntas"
      ),
      cambio_juntas = juntas - juntas_2023
    ) |>
    dplyr::select(-parroquia_2023_en_2027)
}

#' 2023 recintos that no longer exist in the 2027 frame.
recintos_retirados <- function(frame, dist_2023) {
  dist_2023 |>
    dplyr::anti_join(frame, by = "recinto_codigo") |>
    dplyr::select(canton, parroquia_codigo, parroquia, recinto_codigo, recinto, juntas, electores)
}

#' Build the 2027 sampling frame: strata, history, sort order for systematic PPS.
#'
#' Within each stratum the frame is sorted by parroquia, zona and recinto, so the
#' systematic draw spreads the sample across parroquias (implicit stratification).
build_frame <- function(dist_2027, dist_2023, parroquias) {
  dist_2027 |>
    assign_strata(parroquias_frame(parroquias)) |>
    link_history(dist_2023) |>
    dplyr::arrange(estrato, canton, parroquia_codigo, zona_codigo, recinto_codigo) |>
    dplyr::mutate(orden = dplyr::row_number(), .by = estrato) |>
    dplyr::relocate(estrato, orden, .before = 1)
}

#' Stop unless every recinto is in exactly one stratum and the frame keeps every
#' recinto, junta and elector of the distributivo.
assert_frame_ok <- function(frame, dist) {
  fails <- c(
    "duplicated recinto" = anyDuplicated(frame$recinto_codigo) > 0,
    "recintos lost or added" = nrow(frame) != nrow(dist),
    "juntas lost or added" = sum(frame$juntas) != sum(dist$juntas),
    "electores lost or added" = sum(frame$electores, na.rm = TRUE) != sum(dist$electores, na.rm = TRUE),
    "recinto without stratum" = anyNA(frame$estrato),
    "recinto without juntas" = any(frame$juntas <= 0),
    "juntas F + M differ from total" = any(frame$juntas_f + frame$juntas_m != frame$juntas)
  )
  if (any(fails)) {
    cli::cli_abort(c("Sampling frame failed checks.", stats::setNames(names(fails)[fails], rep("x", sum(fails)))))
  }
  frame
}

# ---- allocation and certainty ------------------------------------------------------------

#' Allocate a number of recintos across strata in proportion to their juntas.
#'
#' Largest-remainder rounding with a minimum per stratum. This is the provisional
#' allocation used to size the certainty stratum; phase 5 compares it with Neyman.
allocate_proportional <- function(frame, n_total, min_por_estrato = 2L) {
  w <- frame |>
    dplyr::summarise(juntas = sum(juntas), .by = estrato) |>
    dplyr::arrange(estrato)
  x <- n_total * w$juntas / sum(w$juntas)
  n <- pmax(floor(x), min_por_estrato)
  while (sum(n) < n_total) {
    i <- which.max(x - n)
    n[i] <- n[i] + 1
  }
  while (sum(n) > n_total) {
    ok <- which(n > min_por_estrato)
    i <- ok[which.min((x - n)[ok])]
    n[i] <- n[i] - 1
  }
  dplyr::mutate(w, n_recintos = as.integer(n))
}

#' Flag the certainty recintos of each stratum for a given allocation.
#'
#' With systematic PPS on juntas, a recinto whose size is at least the sampling
#' interval (juntas of the stratum / recintos to draw) would be hit with
#' probability 1 or more. Those recintos are taken with certainty; the interval is
#' recomputed on the rest until no new recinto crosses it.
#' @param asignacion Tibble with `estrato` and `n_recintos`.
#' @return The frame with `certeza` (logical) and `intervalo` (final interval of the stratum).
flag_certainty <- function(frame, asignacion) {
  parts <- lapply(split(frame, frame$estrato, drop = TRUE), function(g) {
    n_h <- asignacion$n_recintos[match(as.character(g$estrato[1]), as.character(asignacion$estrato))]
    if (is.na(n_h)) cli::cli_abort("No allocation for stratum {g$estrato[1]}.")
    cert <- rep(FALSE, nrow(g))
    repeat {
      quedan <- n_h - sum(cert)
      if (quedan <= 0) cli::cli_abort("{g$estrato[1]}: all {n_h} recintos to draw would be certainty recintos.")
      intervalo <- sum(g$juntas[!cert]) / quedan
      nuevo <- cert | g$juntas >= intervalo
      if (identical(nuevo, cert)) break
      cert <- nuevo
    }
    dplyr::mutate(g, certeza = cert, intervalo = intervalo)
  })
  dplyr::bind_rows(parts) |> dplyr::arrange(estrato, orden)
}

# ---- summaries for the report ----------------------------------------------------------------

#' Recintos, juntas and electores by stratum (with a total row).
frame_summary <- function(frame) {
  summ <- function(d) dplyr::summarise(d,
    # max_juntas comes before `juntas` is replaced by its sum
    max_juntas = max(juntas), recintos = dplyr::n(),
    juntas = sum(juntas), juntas_f = sum(juntas_f), juntas_m = sum(juntas_m),
    electores = sum(electores, na.rm = TRUE),
    juntas_por_recinto = juntas / recintos,
    recintos_cda = sum(cda == "SI", na.rm = TRUE),
    recintos_dificil_acceso = sum(dificil_acceso == "SI", na.rm = TRUE),
    recintos_sin_conectividad = sum(sin_conectividad == "SI", na.rm = TRUE),
    recintos_nuevos = sum(historia == "nuevo"),
    recintos_cambia_juntas = sum(historia == "cambia_juntas"))
  dplyr::bind_rows(
    summ(dplyr::group_by(frame, estrato)) |> dplyr::mutate(estrato = as.character(estrato)),
    summ(frame) |> dplyr::mutate(estrato = "Total")
  )
}

#' Certainty recintos and intervals by stratum for an allocation.
certainty_summary <- function(frame_cert, asignacion) {
  frame_cert |>
    dplyr::summarise(intervalo = dplyr::first(intervalo), recintos_certeza = sum(certeza),
                     juntas_certeza = sum(juntas[certeza]), .by = estrato) |>
    dplyr::left_join(dplyr::select(asignacion, estrato, n_recintos), by = "estrato") |>
    dplyr::mutate(recintos_pps = n_recintos - recintos_certeza) |>
    dplyr::arrange(estrato)
}
