# Pipeline of the quick count project. Run with targets::tar_make().
# Phase 1 (ingest): read the raw inputs and check them against the official files.
# Phase 2 (clean): named, logged cleaning rules; cleaned tables written to data/.
# Phase 3 (frame): 2027 recintos with strata, 2023 history and certainty flags.
# Phase 4 (parameters): S and rho by stratum and candidate from the 2023 juntas.
# Phase 5 (sample size): grid of scenarios, integrated Prefecto + Alcalde sample.
# Phase 6 (simulation): the full design drawn many times on the 2023 juntas.
# Phase 5b/6b (junta design): juntas drawn directly (the sampled junta's own delegate reports).
# Phase 7 (stability): S across five races (2019, 2023, 2025) and Bayesian assurance.
# Later phases (selection, estimation) are
# added below as they are built, one phase at a time.

library(targets)
library(tarchetypes)

tar_option_set(
  packages = c("dplyr", "tidyr", "readr", "readxl", "haven", "janitor", "stringr", "purrr", "tibble", "cli", "arrow"),
  format = "rds",
  # Free each result from memory once its dependants have used it: the long runs
  # (several 20 MB simulation tables) crashed the background R process twice.
  memory = "transient",
  garbage_collection = TRUE
)

tar_source("R")

list(
  # ---- raw files (tracked: a change in a file re-runs what depends on it) ----------
  tar_target(f_juntas_prefecto_2023, "data-raw/cne2023_prefecto_pichincha_juntas.csv", format = "file"),
  tar_target(f_juntas_alcalde_2023, "data-raw/cne2023_alcalde_quito_juntas.csv", format = "file"),
  tar_target(f_spss_2023, "data-raw/seccionales-2023/Resultados-electorales-2023.sav", format = "file"),
  tar_target(f_parroquias_2023, "data-raw/seccionales-2023/parroquias.sav", format = "file"),
  tar_target(f_distributivo_2023,
             "data-raw/seccionales-2023/Distributivo Nacional de Recintos Electorales 08-08-2022.xlsx",
             format = "file"),
  tar_target(f_distributivo_2027,
             "data-raw/seccionales-2026/Distirbutivo-Recintos-Elecciones-Seccionales-y-CPCCS-2027-110826.xlsx",
             format = "file"),

  # ---- read ------------------------------------------------------------------------
  tar_target(juntas_prefecto_2023, read_juntas_web(f_juntas_prefecto_2023)),
  tar_target(juntas_alcalde_2023, read_juntas_web(f_juntas_alcalde_2023)),
  tar_target(spss_2023, read_resultados_spss(f_spss_2023)),
  tar_target(spss_prefecto_2023, spss_dignidad(spss_2023, dignidad = 11L)),
  tar_target(spss_alcalde_2023, spss_dignidad(spss_2023, dignidad = 13L, canton = CANTON_QUITO)),
  tar_target(parroquias_2023, read_parroquias(f_parroquias_2023)),
  tar_target(distributivo_2023, read_distributivo(f_distributivo_2023, sheet = 3, skip = 7, anio = 2023L)),
  tar_target(distributivo_2027, read_distributivo(f_distributivo_2027, sheet = "Consolidado", skip = 7, anio = 2027L)),
  tar_target(rangos_2023, expand_junta_ranges(distributivo_2023)),

  # ---- checks (each one stops the pipeline if it fails) -----------------------------
  tar_target(check_juntas_prefecto_2023, assert_juntas_ok(juntas_prefecto_2023, "Prefecto 2023")),
  tar_target(check_juntas_alcalde_2023, assert_juntas_ok(juntas_alcalde_2023, "Alcalde Quito 2023")),
  tar_target(spss_cmp_prefecto_2023,
             assert_matches_spss(compare_with_spss(juntas_prefecto_2023, spss_prefecto_2023), "Prefecto 2023")),
  tar_target(spss_cmp_alcalde_2023,
             assert_matches_spss(compare_with_spss(juntas_alcalde_2023, spss_alcalde_2023), "Alcalde Quito 2023")),
  tar_target(link_prefecto_2023,
             assert_recinto_link(check_recinto_link(juntas_prefecto_2023, rangos_2023), "Prefecto 2023")),
  tar_target(link_alcalde_2023,
             assert_recinto_link(check_recinto_link(juntas_alcalde_2023, rangos_2023), "Alcalde Quito 2023")),
  tar_target(rangos_invalidos_2023, invalid_junta_ranges(distributivo_2023)),
  tar_target(rangos_invalidos_2027, invalid_junta_ranges(distributivo_2027)),

  # ---- summaries for the report ("Fuentes de datos" and "Depuración") --------------
  tar_target(ingest_resumen, ingest_summary(
    checks = list(check_juntas_prefecto_2023, check_juntas_alcalde_2023),
    spss_cmps = list(spss_cmp_prefecto_2023, spss_cmp_alcalde_2023),
    links = list(link_prefecto_2023, link_alcalde_2023)
  )),
  tar_target(distributivos_resumen, dplyr::bind_rows(
    distributivo_summary(distributivo_2023), distributivo_summary(distributivo_2027)
  )),

  # ---- phase 2: clean ------------------------------------------------------------------
  # The checks above must pass first: cleaning depends on them explicitly.
  tar_target(limpio_prefecto_2023, {
    spss_cmp_prefecto_2023; link_prefecto_2023
    assert_clean_ok(clean_juntas(juntas_prefecto_2023, "Prefecto 2023"), "Prefecto 2023")
  }),
  tar_target(limpio_alcalde_2023, {
    spss_cmp_alcalde_2023; link_alcalde_2023
    assert_clean_ok(clean_juntas(juntas_alcalde_2023, "Alcalde Quito 2023"), "Alcalde Quito 2023")
  }),
  tar_target(depuracion_log, dplyr::bind_rows(limpio_prefecto_2023$log, limpio_alcalde_2023$log)),
  tar_target(efecto_exclusion, dplyr::bind_rows(
    dplyr::mutate(exclusion_effect(juntas_prefecto_2023, limpio_prefecto_2023), fuente = "Prefecto 2023"),
    dplyr::mutate(exclusion_effect(juntas_alcalde_2023, limpio_alcalde_2023), fuente = "Alcalde Quito 2023")
  )),
  tar_target(archivos_limpio_prefecto_2023, write_clean(limpio_prefecto_2023, "prefecto_2023"), format = "file"),
  tar_target(archivos_limpio_alcalde_2023, write_clean(limpio_alcalde_2023, "alcalde_2023"), format = "file"),

  # ---- phase 3: frame ------------------------------------------------------------------
  tar_target(marco_2027, assert_frame_ok(build_frame(distributivo_2027, distributivo_2023, parroquias_2023),
                                         distributivo_2027)),
  tar_target(recintos_retirados_2023, recintos_retirados(marco_2027, distributivo_2023)),
  # Provisional allocation (proposal: 188 recintos, proportional to juntas) to size the
  # certainty stratum; phase 5 recomputes it for every scenario of the sample size grid.
  tar_target(n_recintos_provisional, 188L),
  tar_target(asignacion_provisional, allocate_proportional(marco_2027, n_recintos_provisional)),
  tar_target(marco_2027_certeza, flag_certainty(marco_2027, asignacion_provisional)),
  tar_target(marco_resumen, frame_summary(marco_2027)),
  tar_target(certeza_resumen, certainty_summary(marco_2027_certeza, asignacion_provisional)),
  tar_target(archivo_marco_2027, {
    dir.create("data", showWarnings = FALSE)
    arrow::write_parquet(marco_2027, "data/marco_2027.parquet")
    "data/marco_2027.parquet"
  }, format = "file"),

  # ---- phase 4: parameters -------------------------------------------------------------
  tar_target(n_boot, 500L),
  tar_target(semilla_boot, 20270214L), # fixed seed: the bootstrap intervals are reproducible
  tar_target(matriz_prefecto_2023, junta_matrix(limpio_prefecto_2023, parroquias_2023)),
  tar_target(matriz_alcalde_2023, junta_matrix(limpio_alcalde_2023, parroquias_2023)),
  tar_target(parametros_2023, dplyr::bind_rows(
    estimate_parameters(matriz_prefecto_2023, "Prefecto 2023", B = n_boot, seed = semilla_boot),
    estimate_parameters(matriz_alcalde_2023, "Alcalde Quito 2023", B = n_boot, seed = semilla_boot)
  )),
  tar_target(parametros_2023_sin_inconsistentes, dplyr::bind_rows(
    estimate_parameters(matriz_prefecto_2023, "Prefecto 2023", B = 0L,
                        filtro = !matriz_prefecto_2023$juntas$acta_inconsistente),
    estimate_parameters(matriz_alcalde_2023, "Alcalde Quito 2023", B = 0L,
                        filtro = !matriz_alcalde_2023$juntas$acta_inconsistente)
  )),
  tar_target(sensibilidad_inconsistentes, sensitivity_inconsistent(parametros_2023, parametros_2023_sin_inconsistentes)),
  tar_target(insumos_diseno, design_inputs(parametros_2023)),

  # ---- phase 5: sample size ------------------------------------------------------------
  # Cost = recintos (an observer costs the same whatever the number of actas).
  tar_target(escenarios_tamano, sample_size_scenarios()),
  tar_target(minimo_recintos_estrato, 4L),
  tar_target(actas_max_observador, 2L), # juntas per recinto one observer can record (owner, 20 Oct 2026)
  tar_target(grilla_tamano, sample_size_grid(parametros_2023, marco_2027, escenarios_tamano,
                                             minimo = minimo_recintos_estrato, reserva = 0.10,
                                             actas_max = actas_max_observador)),
  tar_target(escenarios_cliente, client_scenarios(grilla_tamano)),

  # ---- phase 6: simulation -------------------------------------------------------------
  # 500 replicates per design take a few minutes in total; raise n_sim for the final report.
  tar_target(n_sim, 500L),
  tar_target(n_sim_final, 2000L), # junta design (main design): 2000 replicates, 99% quantiles stable to about 0.05 pp
  tar_target(semilla_sim, 20270215L),
  tar_target(poblacion_sim, sim_population(matriz_prefecto_2023, matriz_alcalde_2023)),
  tar_target(sim_escenarios, simulate_scenarios(poblacion_sim, escenarios_cliente, n_sim, semilla_sim)),
  tar_target(sim_resumen, summarise_simulation(sim_escenarios)),
  # How far can the sample shrink? Scenario B (+/-1 pp, 99%, 2 actas) scaled down.
  tar_target(factores_escala, seq(0.4, 1, by = 0.1)),
  tar_target(sim_escalado, {
    e <- dplyr::filter(escenarios_cliente, startsWith(escenario, "B."))
    simulate_scaling(poblacion_sim, e$n_h[[1]], e$m, factores_escala, n_sim, semilla_sim, conf = e$conf)
  }),
  tar_target(sim_escalado_resumen, summarise_simulation(sim_escalado)),
  tar_target(tamano_minimo_simulado, smallest_scale(sim_escalado_resumen, E = 1)),

  # ---- phase 5b / 6b: junta design -------------------------------------------------------
  # One delegate per junta, the quick-count reporter is the sampled junta's own delegate
  # (client, 20 Oct 2026): no extra staff per junta, so juntas are drawn directly.
  tar_target(poblacion_juntas, junta_population(matriz_prefecto_2023, matriz_alcalde_2023)),
  tar_target(objetivos_juntas, tibble::tribble(~E, ~conf, 1, 0.99, 1, 0.95, 1.5, 0.99)),
  tar_target(formula_juntas, purrr::pmap(objetivos_juntas, function(E, conf)
    junta_formula_integrated(parametros_2023, marco_2027, E, conf))),
  tar_target(factores_juntas, seq(0.3, 1, by = 0.05)),
  tar_target(sim_juntas, purrr::map_dfr(seq_len(nrow(objetivos_juntas)), function(i)
    simulate_junta_scaling(poblacion_juntas, formula_juntas[[i]]$n_h, factores_juntas, n_sim_final,
                           semilla_sim + 1000L * i, conf = objetivos_juntas$conf[i], E = objetivos_juntas$E[i]))),
  tar_target(sim_juntas_resumen, summarise_junta_sim(sim_juntas)),
  tar_target(eleccion_juntas, junta_size_choice(sim_juntas, margen = regla_margen)),
  tar_target(diseno_juntas_1_99, junta_design_chosen(sim_juntas, eleccion_juntas, E = 1, conf = 0.99)),
  # The client's question: one sample for both dignidades, or two separate samples?
  tar_target(sim_una_vs_dos, {
    d <- diseno_juntas_1_99
    if (is.null(d)) d <- list(n_h = formula_juntas[[1]]$n_h) # no share of the formula size complied: use it whole
    quito <- c("Quito urbano", "Quito rural")
    dplyr::bind_rows(
      simulate_junta_design(poblacion_juntas, d$n_h, n_sim_final, semilla_sim + 5001L, 0.99, "1. Una muestra (diseno elegido)"),
      simulate_junta_design(poblacion_juntas, d$n_h, n_sim_final, semilla_sim + 5002L, 0.99,
                            "2. Dos muestras, mismas actas", n_h_alcalde = d$n_h[quito])
    )
  }),
  tar_target(una_vs_dos_resumen, summarise_junta_sim(sim_una_vs_dos)),

  # ---- options for the client: best case and with a buffer ---------------------------------
  # Assumed field losses (to replace with the client's experience): a junta fails on its
  # own with these probabilities, and a whole recinto fails (no signal) with 3%.
  tar_target(perdidas_campo, list(
    junta = stats::setNames(c(0.05, 0.10, 0.05, 0.10, 0.10, 0.15), ESTRATOS),
    recinto = 0.03
  )),
  tar_target(sim_campo, {
    d <- diseno_juntas_1_99
    if (is.null(d)) d <- list(n_h = formula_juntas[[1]]$n_h)
    field_scenarios(poblacion_juntas, d$n_h, perdidas_campo$junta, perdidas_campo$recinto, n_sim_final, semilla_sim + 6000L, conf = 0.99)
  }),
  tar_target(opciones_campo, field_options(sim_campo, E = 1, conf = 0.99, margen = regla_margen)),
  # Same options at 95%: the chosen +/-1 pp, 95% design.
  tar_target(diseno_juntas_1_95, junta_design_chosen(sim_juntas, eleccion_juntas, E = 1, conf = 0.95)),
  tar_target(sim_campo_95, {
    d <- diseno_juntas_1_95
    if (is.null(d)) d <- list(n_h = formula_juntas[[2]]$n_h)
    field_scenarios(poblacion_juntas, d$n_h, perdidas_campo$junta, perdidas_campo$recinto, n_sim_final, semilla_sim + 7000L, conf = 0.95)
  }),
  tar_target(opciones_campo_95, field_options(sim_campo_95, E = 1, conf = 0.95, margen = regla_margen)),
  tar_target(asignacion_opciones, {
    alloc <- function(d, f, nivel) {
      if (is.null(d)) d <- list(n_h = f$n_h)
      tibble::tibble(conf = nivel, estrato = names(d$n_h), optimista = unname(d$n_h),
                     con_colchon = unname(buffer_allocation(d$n_h, perdidas_campo$junta, perdidas_campo$recinto,
                                                            N = lengths(poblacion_juntas$filas_estrato))))
    }
    dplyr::bind_rows(alloc(diseno_juntas_1_99, formula_juntas[[1]], 0.99),
                     alloc(diseno_juntas_1_95, formula_juntas[[2]], 0.95))
  }),

  # ---- phase 7: stability across elections and Bayesian assurance -------------------------
  # Junta files of 2019 and 2025, validated against the CNE SPSS files when they were
  # consolidated (scripts/extraccion/consolidar_juntas_20XX.py; reports in data-raw/).
  tar_target(f_juntas_prefecto_2019, "data-raw/cne2019_prefecto_pichincha_juntas.csv", format = "file"),
  tar_target(f_juntas_alcalde_2019, "data-raw/cne2019_alcalde_quito_juntas.csv", format = "file"),
  tar_target(f_juntas_presidente_2025, "data-raw/cne2025_presidente_pichincha_juntas.csv", format = "file"),
  tar_target(matriz_prefecto_2019, historic_matrix(f_juntas_prefecto_2019, "Prefecto 2019", parroquias_2023)),
  tar_target(matriz_alcalde_2019, historic_matrix(f_juntas_alcalde_2019, "Alcalde Quito 2019", parroquias_2023)),
  tar_target(matriz_presidente_2025, historic_matrix(f_juntas_presidente_2025, "Presidente 2025", parroquias_2023)),
  tar_target(parametros_hist, dplyr::bind_rows(
    estimate_parameters(matriz_prefecto_2019, "Prefecto 2019", B = 0L),
    estimate_parameters(matriz_alcalde_2019, "Alcalde Quito 2019", B = 0L),
    estimate_parameters(matriz_presidente_2025, "Presidente 2025", B = 0L)
  )),
  tar_target(parametros_todas, dplyr::bind_rows(parametros_2023, parametros_hist)),
  tar_target(estabilidad_S, stability_table(parametros_todas)),
  # The Bayesian assurance (fit_s_model, assurance_curve, ...) was set aside by the owner
  # (21 Oct 2026); its functions stay in R/07_stability.R but it is no longer part of the pipeline.

  # ---- phase 7b: design weighted 2/3 on 2023, 1/3 on 2019 (owner, 21 Oct 2026) ------------
  # The Bayesian assurance above is set aside (hard to explain). The design is sized for
  # the weighted variability of the last two seccionales and must pass the simulation on
  # the 2023 and the 2019 juntas. Presidente 2025 is only a stress test.
  tar_target(peso_2023, 2 / 3),
  tar_target(parametros_ponderados, dplyr::bind_rows(
    pool_parameters(parametros_2023 |> dplyr::filter(dignidad == "Prefecto 2023"),
                    parametros_hist |> dplyr::filter(dignidad == "Prefecto 2019"), "Prefecto ponderado", w = peso_2023),
    pool_parameters(parametros_2023 |> dplyr::filter(dignidad == "Alcalde Quito 2023"),
                    parametros_hist |> dplyr::filter(dignidad == "Alcalde Quito 2019"), "Alcalde Quito ponderado", w = peso_2023)
  )),
  tar_target(comparacion_ponderada, design_inputs(dplyr::bind_rows(
    dplyr::filter(parametros_todas, dignidad != "Presidente 2025"), parametros_ponderados)) |>
      dplyr::select(dignidad, estrato, S_max, candidato_S_max) |>
      tidyr::pivot_wider(id_cols = estrato, names_from = dignidad, values_from = S_max)),
  tar_target(poblacion_juntas_2019, junta_population(matriz_prefecto_2019, matriz_alcalde_2019)),
  tar_target(poblacion_juntas_2025, junta_population(matriz_presidente_2025, quito_subset(matriz_presidente_2025))),
  tar_target(objetivos_ponderados, tibble::tribble(~E, ~conf, 1, 0.99, 1, 0.95)),
  tar_target(formula_ponderada, purrr::pmap(objetivos_ponderados, function(E, conf)
    junta_formula_integrated(parametros_ponderados, marco_2027, E, conf, n_cand = 2L,
                             dig_prov = "Prefecto ponderado", dig_quito = "Alcalde Quito ponderado"))),
  # Targets (owner, 21 Oct 2026): shares of the first two candidates and, for option A, the
  # margin between them; the 3rd candidate is reported with its own margin (with_reported_margin()).
  # Reported margin: "focalizado" = sd for the two leaders and the margin between them, the
  # larger of sd and SRS for the other candidates.
  # One-line switch: "sd", "max" or "focalizado".
  tar_target(regla_margen, "focalizado"),
  # At most 4 sampled juntas per recinto (owner, 22 Oct 2026), so that a recinto keeps
  # delegates for the rest of the observation. Applied to the field options (campo_*,
  # estres_2025), which check the chosen designs with the cap; the size search is uncapped.
  tar_target(max_juntas_recinto, 4L),
  tar_target(factores_ponderados, seq(0.6, 1.3, by = 0.05)),
  tar_target(sim_ponderada, purrr::map_dfr(seq_len(nrow(objetivos_ponderados)), function(i)
    simulate_scaling_elections(list("2023" = poblacion_juntas, "2019" = poblacion_juntas_2019),
                               formula_ponderada[[i]]$n_h, factores_ponderados, n_sim_final,
                               semilla_sim + 8000L + 1000L * i, conf = objetivos_ponderados$conf[i],
                               E = objetivos_ponderados$E[i]))),
  tar_target(eleccion_ponderada, size_choice_elections(sim_ponderada, margen = regla_margen)),
  # For comparison: the same choice under the other two rules.
  tar_target(eleccion_ponderada_sd, size_choice_elections(sim_ponderada, margen = "sd")),
  tar_target(eleccion_ponderada_max, size_choice_elections(sim_ponderada, margen = "max")),
  tar_target(comparacion_reglas, dplyr::bind_rows(
    dplyr::mutate(eleccion_ponderada_sd, regla = "sd"), dplyr::mutate(eleccion_ponderada_max, regla = "max"),
    dplyr::mutate(size_choice_elections(sim_ponderada, margen = "focalizado"), regla = "focalizado")) |>
      dplyr::filter(cumple) |> dplyr::slice_min(factor, n = 1, by = c(regla, conf)) |>
      dplyr::select(regla, conf, factor, juntas, actas, peor_error, peor_semi, cobertura_min)),
  tar_target(diseno_ponderado_99, {
    d <- junta_design_chosen(sim_ponderada, eleccion_ponderada, E = 1, conf = 0.99)
    if (is.null(d)) list(n_h = formula_ponderada[[1]]$n_h, factor = NA) else d
  }),
  tar_target(diseno_ponderado_95, {
    d <- junta_design_chosen(sim_ponderada, eleccion_ponderada, E = 1, conf = 0.95)
    if (is.null(d)) list(n_h = formula_ponderada[[2]]$n_h, factor = NA) else d
  }),
  # Best case and with buffer, checked on both elections (same scenarios as phase 6b).
  tar_target(campo_ponderado, field_options_elections(
    list("2023" = poblacion_juntas, "2019" = poblacion_juntas_2019),
    list(list(d = diseno_ponderado_99, conf = 0.99), list(d = diseno_ponderado_95, conf = 0.95)),
    marco_2027, perdidas_campo, n_sim_final, semilla_sim + 9000L, margen = regla_margen,
    max_recinto = max_juntas_recinto)),
  tar_target(asignacion_ponderada, {
    N <- lengths(poblacion_juntas$filas_estrato)
    f <- function(d, nivel) tibble::tibble(conf = nivel, estrato = names(d$n_h), optimista = unname(d$n_h),
      con_colchon = unname(buffer_allocation(d$n_h, perdidas_campo$junta, perdidas_campo$recinto, N = N)))
    dplyr::bind_rows(f(diseno_ponderado_99, 0.99), f(diseno_ponderado_95, 0.95))
  }),

  # ---- option B: shares only, same method ---------------------------------
  # Sized so that each candidate's share is within +/-1 pp; the margin between first and
  # second is reported (semi_margen) but not required.
  tar_target(formula_cuotas, purrr::pmap(objetivos_ponderados, function(E, conf)
    junta_formula_integrated(parametros_ponderados, marco_2027, E, conf, objetivo = "cuotas", n_cand = 2L,
                             dig_prov = "Prefecto ponderado", dig_quito = "Alcalde Quito ponderado"))),
  tar_target(factores_cuotas, seq(0.6, 1.3, by = 0.05)),
  tar_target(sim_cuotas, purrr::map_dfr(seq_len(nrow(objetivos_ponderados)), function(i)
    simulate_scaling_elections(list("2023" = poblacion_juntas, "2019" = poblacion_juntas_2019),
                               formula_cuotas[[i]]$n_h, factores_cuotas, n_sim_final,
                               semilla_sim + 12000L + 1000L * i, conf = objetivos_ponderados$conf[i],
                               E = objetivos_ponderados$E[i]))),
  tar_target(eleccion_cuotas, size_choice_elections(sim_cuotas, margen = regla_margen, cantidades = "cuotas")),
  tar_target(diseno_cuotas_99, {
    d <- junta_design_chosen(sim_cuotas, eleccion_cuotas, E = 1, conf = 0.99)
    if (is.null(d)) list(n_h = formula_cuotas[[1]]$n_h, factor = NA) else d
  }),
  tar_target(diseno_cuotas_95, {
    d <- junta_design_chosen(sim_cuotas, eleccion_cuotas, E = 1, conf = 0.95)
    if (is.null(d)) list(n_h = formula_cuotas[[2]]$n_h, factor = NA) else d
  }),
  tar_target(campo_cuotas, field_options_elections(
    list("2023" = poblacion_juntas, "2019" = poblacion_juntas_2019),
    list(list(d = diseno_cuotas_99, conf = 0.99), list(d = diseno_cuotas_95, conf = 0.95)),
    marco_2027, perdidas_campo, n_sim_final, semilla_sim + 13000L, cantidades = "cuotas", margen = regla_margen,
    max_recinto = max_juntas_recinto)),
  tar_target(asignacion_cuotas, {
    N <- lengths(poblacion_juntas$filas_estrato)
    f <- function(d, nivel) tibble::tibble(conf = nivel, estrato = names(d$n_h), optimista = unname(d$n_h),
      con_colchon = unname(buffer_allocation(d$n_h, perdidas_campo$junta, perdidas_campo$recinto, N = N)))
    dplyr::bind_rows(f(diseno_cuotas_99, 0.99), f(diseno_cuotas_95, 0.95))
  }),
  # The two options side by side, for the client.
  tar_target(resumen_opciones, options_summary(campo_ponderado, campo_cuotas)),

  # ---- +/-1.5 pp: only as wave 1 of option A (owner, 22 Oct 2026) ---------------------------
  # Option B at +/-1.5 pp and the stand-alone +/-1.5 pp field scenarios were dropped; this
  # design only sets the size of wave 1 in each stratum (see `olas`).
  tar_target(objetivos_15, tibble::tribble(~E, ~conf, 1.5, 0.99, 1.5, 0.95)),
  tar_target(formula_ponderada_15, purrr::pmap(objetivos_15, function(E, conf)
    junta_formula_integrated(parametros_ponderados, marco_2027, E, conf, n_cand = 2L,
                             dig_prov = "Prefecto ponderado", dig_quito = "Alcalde Quito ponderado"))),
  tar_target(sim_ponderada_15, purrr::map_dfr(seq_len(nrow(objetivos_15)), function(i)
    simulate_scaling_elections(list("2023" = poblacion_juntas, "2019" = poblacion_juntas_2019),
                               formula_ponderada_15[[i]]$n_h, factores_ponderados, n_sim_final,
                               semilla_sim + 20000L + 1000L * i, conf = objetivos_15$conf[i], E = objetivos_15$E[i]))),
  tar_target(eleccion_ponderada_15, size_choice_elections(sim_ponderada_15, margen = regla_margen)),
  tar_target(disenos_15, {
    elegir <- function(sim, el, f, conf, i) {
      d <- junta_design_chosen(sim, el, E = 1.5, conf = conf)
      if (is.null(d)) list(n_h = f[[i]]$n_h, factor = NA) else d
    }
    list(A_99 = elegir(sim_ponderada_15, eleccion_ponderada_15, formula_ponderada_15, 0.99, 1),
         A_95 = elegir(sim_ponderada_15, eleccion_ponderada_15, formula_ponderada_15, 0.95, 2))
  }),
  tar_target(asignacion_15, {
    N <- lengths(poblacion_juntas$filas_estrato)
    purrr::imap_dfr(disenos_15, function(d, nombre) tibble::tibble(
      opcion = substr(nombre, 1, 1), conf = as.numeric(substr(nombre, 3, 4)) / 100,
      estrato = names(d$n_h), optimista = unname(d$n_h),
      con_colchon = unname(buffer_allocation(d$n_h, perdidas_campo$junta, perdidas_campo$recinto, N = N))))
  }),
  # ---- nested waves: wave 1 (+/-1.5 pp) inside the full sample (+/-1 pp), option A ---------
  tar_target(olas, wave_check(
    list("2023" = poblacion_juntas, "2019" = poblacion_juntas_2019),
    list(list(conf = 0.99, full = diseno_ponderado_99$n_h, w1 = disenos_15$A_99$n_h),
         list(conf = 0.95, full = diseno_ponderado_95$n_h, w1 = disenos_15$A_95$n_h)),
    perdidas_campo, n_sim_final, semilla_sim + 30000L, margen = regla_margen, max_recinto = max_juntas_recinto)),
  tar_target(resumen_olas, olas |>
    dplyr::select(eleccion, conf, escenario, objetivo_pp, juntas_muestra, juntas_reportan, actas_reportan,
                  peor_semi, semi_margen, semi_otros, cobertura_min, max_por_recinto, cumple) |>
    dplyr::arrange(dplyr::desc(conf), escenario, eleccion)),

  # ---- phase 8: lists of drawn juntas for the client (owner, 23 Oct 2026) -------------------
  # The juntas of options A and B (99% and 95%, with the buffer) drawn from the 2026 frame,
  # with a fixed seed, and one Excel workbook per option in reportes/sorteo/ (confidential,
  # in .gitignore). Option A also marks each junta's wave. Needs the openxlsx package.
  tar_target(semilla_sorteo, 20261129L),
  tar_target(juntas_marco_2027, frame_juntas(marco_2027)),
  tar_target(listas_sorteo, draw_lists(
    juntas_marco_2027,
    list(A_99 = diseno_ponderado_99, A_95 = diseno_ponderado_95, B_99 = diseno_cuotas_99, B_95 = diseno_cuotas_95),
    disenos_15, perdidas_campo, semilla_sorteo, max_recinto = max_juntas_recinto)),
  tar_target(resumen_sorteo, summarise_lists(listas_sorteo, juntas_marco_2027)),
  tar_target(archivos_sorteo, write_list_workbooks(listas_sorteo, resumen_sorteo, "reportes/sorteo",
                                                   semilla_sorteo, perdidas_campo),
             format = "file"),

  # Stress test: the chosen design on the 2025 presidential juntas ("Alcalde" rows = Presidente in Quito).
  tar_target(estres_2025, dplyr::bind_rows(
    dplyr::mutate(field_options(simulate_junta_design(poblacion_juntas_2025, diseno_ponderado_99$n_h, n_sim_final,
                                                      semilla_sim + 9901L, 0.99, "Diseno ponderado, 99%",
                                                      max_recinto = max_juntas_recinto), E = 1, conf = 0.99, margen = regla_margen), conf = 0.99),
    dplyr::mutate(field_options(simulate_junta_design(poblacion_juntas_2025, diseno_ponderado_95$n_h, n_sim_final,
                                                      semilla_sim + 9902L, 0.95, "Diseno ponderado, 95%",
                                                      max_recinto = max_juntas_recinto), E = 1, conf = 0.95, margen = regla_margen), conf = 0.95)
  ))
)
