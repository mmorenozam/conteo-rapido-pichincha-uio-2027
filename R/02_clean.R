# Phase 2: clean.
#
# Every rule is a named function that takes the junta x candidate table and
# returns list(data = <table after the rule>, log = <one-row tibble>). The logs
# are stacked into the cleaning log that feeds the "Depuración de datos" chapter.
# Decisions taken with the owner (20 Oct 2026):
#   - special juntas (home voting, prisons) are excluded from the sampling frame;
#   - actas where valid + blancos + nulos != sufragantes are kept as recorded and flagged.

# ---- helpers ---------------------------------------------------------------------------

#' One-row log entry for a cleaning rule.
log_rule <- function(regla, fuente, descripcion, juntas, votos = NA_real_, detalle = NA_character_) {
  tibble::tibble(regla = regla, fuente = fuente, descripcion = descripcion,
                 juntas_afectadas = as.integer(juntas), votos_afectados = votos, detalle = detalle)
}

#' Apply a list of rules in order and collect their logs.
#' @param rules Named list of functions(data, fuente) returning list(data, log)
#'   and, for rules that drop rows, `excluidas` (the dropped rows).
apply_rules <- function(data, rules, fuente) {
  logs <- vector("list", length(rules))
  excl <- data[0, , drop = FALSE]
  for (i in seq_along(rules)) {
    res <- rules[[i]](data, fuente)
    data <- res$data
    logs[[i]] <- res$log
    if (!is.null(res$excluidas)) excl <- dplyr::bind_rows(excl, res$excluidas)
  }
  list(data = data, log = dplyr::bind_rows(logs), excluidas = excl)
}

# ---- rules -----------------------------------------------------------------------------

#' Rule: repair mis-encoded text from the CNE site.
#'
#' Why: some zona names arrive as UTF-8 read twice ("NÂº 2" for "Nº 2"). In 2023
#' this only affects the prison zonas. The text is repaired, nothing else changes.
rule_fix_encoding <- function(data, fuente) {
  fix <- function(s) stringr::str_replace_all(s, c("Âº" = "º", "Âª" = "ª"))
  text_cols <- intersect(c("parroquia", "zona", "recinto", "op_nombre", "candidato"), names(data))
  changed <- data |>
    dplyr::filter(dplyr::if_any(dplyr::all_of(text_cols), \(x) !is.na(x) & fix(x) != x)) |>
    dplyr::distinct(junta_id, zona)
  data <- dplyr::mutate(data, dplyr::across(dplyr::all_of(text_cols), fix))
  list(
    data = data,
    log = log_rule("fix_encoding", fuente, "Texto con codificación doble (Âº) reparado",
                   nrow(changed), detalle = paste(unique(fix(changed$zona)), collapse = "; "))
  )
}

#' Rule: exclude the special juntas from the sampling frame.
#'
#' Why: home-voting juntas (zona 89) and prison juntas (zona 61) belong to no
#' recinto, have 1 to 24 voters and cannot be reached by an observer. Their votes
#' stay in the official result; leaving them out of the frame changes the
#' estimates by a negligible amount (reported in the log). Decided with the owner.
#' The excluded rows are returned in `excluidas`.
rule_exclude_special <- function(data, fuente, zonas_especiales = c(61L, 89L)) {
  is_special <- is.na(data$recinto_codigo) | data$zona_codigo %in% zonas_especiales
  excl <- data[is_special, , drop = FALSE]
  hdr <- juntas_header(excl)
  list(
    data = data[!is_special, , drop = FALSE],
    excluidas = excl,
    log = log_rule("exclude_special", fuente,
                   "Juntas especiales (voto en casa, centros de privación de libertad) excluidas del marco",
                   nrow(hdr), votos = sum(excl$votos),
                   detalle = sprintf("%d sufragantes (%.4f%% del total)", sum(hdr$sufragantes),
                                     100 * sum(hdr$sufragantes) / sum(juntas_header(data)$sufragantes)))
  )
}

#' Rule: flag actas whose components do not add up to sufragantes.
#'
#' Why: in about 13% of the 2023 actas, valid + blancos + nulos differs from
#' sufragantes (by a few votes, in both directions). The same values are in the
#' official totals, so they are kept as recorded. The estimator uses valid votes
#' as the denominator, which does not depend on sufragantes. The flag allows a
#' sensitivity check without these actas.
rule_flag_inconsistent <- function(data, fuente) {
  validos <- dplyr::summarise(data, votos_validos = sum(votos), .by = junta_id)
  data <- data |>
    dplyr::select(-dplyr::any_of(c("votos_validos", "brecha_acta", "acta_inconsistente"))) |>
    dplyr::left_join(validos, by = "junta_id") |>
    dplyr::mutate(
      brecha_acta = votos_validos + blancos + nulos - sufragantes,
      acta_inconsistente = brecha_acta != 0
    )
  hdr <- juntas_header(data)
  list(
    data = data,
    log = log_rule("flag_inconsistent", fuente,
                   "Actas con válidos + blancos + nulos distinto de sufragantes: se conservan y se marcan",
                   sum(hdr$acta_inconsistente), votos = sum(hdr$brecha_acta),
                   detalle = sprintf("brecha neta %+d votos; mediana de la brecha absoluta en actas marcadas: %s",
                                     sum(hdr$brecha_acta),
                                     stats::median(abs(hdr$brecha_acta[hdr$acta_inconsistente]))))
  )
}

#' Rule: derived junta variables.
#'
#' Why: later phases need electores per junta (sufragantes + ausentismo, as the
#' CNE site reports them) and the turnout. Nothing is changed or dropped.
rule_derive <- function(data, fuente) {
  data <- dplyr::mutate(data, electores = sufragantes + ausentismo,
                        participacion = dplyr::if_else(electores > 0, sufragantes / electores, NA_real_))
  list(data = data, log = log_rule("derive", fuente, "Variables derivadas: electores y participación", 0L))
}

RULES_2023 <- list(
  fix_encoding = rule_fix_encoding,
  exclude_special = rule_exclude_special,
  flag_inconsistent = rule_flag_inconsistent,
  derive = rule_derive
)

# ---- outputs -----------------------------------------------------------------------------

#' Clean one junta-level source.
#' @return list(juntas = one row per junta, votos = one row per junta x candidate,
#'   excluidas = excluded juntas (one row per junta), log = cleaning log).
clean_juntas <- function(juntas, fuente, rules = RULES_2023) {
  res <- apply_rules(juntas, rules, fuente)
  d <- res$data
  list(
    juntas = juntas_header(d),
    votos = dplyr::select(d, junta_id, dplyr::any_of(COLS_CANDIDATO)),
    excluidas = juntas_header(res$excluidas),
    log = res$log
  )
}

#' Checks on the cleaned tables. Stops on failure.
assert_clean_ok <- function(cl, label) {
  fails <- c(
    "duplicated junta" = anyDuplicated(cl$juntas$junta_id) > 0,
    "junta without recinto" = anyNA(cl$juntas$recinto_codigo),
    "votes for unknown junta" = !all(cl$votos$junta_id %in% cl$juntas$junta_id),
    "junta without votes" = !all(cl$juntas$junta_id %in% cl$votos$junta_id),
    "electores below sufragantes" = any(cl$juntas$electores < cl$juntas$sufragantes),
    "mis-encoded text left" = any(stringr::str_detect(cl$juntas$zona, "Â"), na.rm = TRUE)
  )
  if (any(fails)) {
    cli::cli_abort(c("{label}: cleaned tables failed checks.", stats::setNames(names(fails)[fails], rep("x", sum(fails)))))
  }
  cl
}

#' Effect of the exclusion on the vote shares (percentage points), for the log.
exclusion_effect <- function(juntas_raw, cl) {
  share <- function(v) v |>
    dplyr::summarise(votos = sum(votos), .by = candidato) |>
    dplyr::mutate(pct = 100 * votos / sum(votos))
  dplyr::full_join(share(juntas_raw), share(cl$votos), by = "candidato", suffix = c("_todas", "_marco")) |>
    dplyr::mutate(diferencia_pp = pct_marco - pct_todas) |>
    dplyr::arrange(dplyr::desc(pct_todas))
}

#' Write the cleaned tables to data/ (parquet) and return the paths.
write_clean <- function(cl, name, dir = "data") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  paths <- file.path(dir, paste0(name, c("_juntas", "_votos", "_excluidas"), ".parquet"))
  arrow::write_parquet(cl$juntas, paths[1])
  arrow::write_parquet(cl$votos, paths[2])
  arrow::write_parquet(cl$excluidas, paths[3])
  paths
}
