# Data sources (data-raw)

Every raw file is listed here with its origin, download date and SHA-256 hash. Files in `data-raw/` are never edited by hand; corrections happen in code.

## seccionales-2023 (CNE, Elecciones Seccionales, CPCCS y Referéndum, 5 Feb 2023)

Downloaded by Mauricio Moreno on 19 Oct 2026. Source URL: to be completed.

| File | Content | Bytes | SHA-256 |
| --- | --- | --- | --- |
| Resultados-electorales-2023.sav | Official results, all dignidades, aggregated by parroquia × sex of the junta × candidate (no junta or recinto level) | 39679983 | dacea6ed259a97a302aed2ed577c2b856b531463247968be72a45e1d00d31e00 |
| Candidatos-2023.sav | Candidates | 6806430 | d44cacb201dc35e2ba9c27e31463cc934af8980241deeca7ea3f132f980464eb |
| Organizaciones-politicas-2023.sav | Political organizations | 125475 | e89cc88bb591169945a18b09e4b6ba1d8160c96f901f6db903f5847339d6ab32 |
| Registro-electoral-a-nivel-parroquial-2023.sav | Electores by parroquia × sex × age group | 430867 | 01e09721c6511b1ce378e97ec9c4d662cd74742b9fce1edc8a121931bf2c9ccf |
| parroquias.sav | Parroquia catalog, includes PARROQUIA_ESTADO (U urban / R rural) | 94151 | 85fbd175f6b3e792dee00994b78fb547980471108fd599e83ff2e5d24b148b4e |
| cantones.sav | Canton catalog | 11467 | 6b6d177fb1d32113d4f55c081d47f5e0ed5d8892e0f819a4e27300e31adcf4a0 |
| provincias.sav | Province catalog | 1267 | 9a263865402d34a721fcf9b0ad5d9e101dd566709a50e4f014b2736cdb938316 |
| dignidades.sav | Dignidad catalog | 1533 | b2b6621266111a3ff12cb10d83d29207f2faf0c9cea7768468d2f54dc1fba38e |
| Distributivo Nacional de Recintos Electorales 08-08-2022.xlsx | Recintos 2023 with junta ranges by sex (JUN_INIF to JUN_FINF, JUN_INIM to JUN_FINM), coordinates, U/R | 1171651 | 7d12d6dd5fed28c418fa59f67439ee7ab583cbb36ea190b5b99b097de9c78541 |
| DISTRIBUTIVO ELECTORES 2023.xls | Electores and juntas by zona and sex | 746496 | a80aee3e97722453be5d1e75dde45a943494679e355e4cf8abf67df92b85ca7e |
| 02_Tabla-Circunscripciones Electorales_DEFINITIVO ELECTORES.xlsx | Population and electores by circunscripción | 32460 | 3fb07e95505f257cb175d80f0de5c000ddde1a211feee33896588a644d0d4e26 |

## seccionales-2026 (CNE, Elecciones Seccionales y CPCCS, 14 Feb 2027)

| File | Content | Bytes | SHA-256 |
| --- | --- | --- | --- |
| Distirbutivo-Recintos-Elecciones-Seccionales-y-CPCCS-2027-110826.xlsx | Recintos 2027 (cut-off 11 Aug 2026) with junta ranges, electores, CDA, hard-to-reach and no-connectivity flags | 974614 | 35d74e9a269dc239976c87078d7f0758f65bc84547be9302c117710a5605036a |
| RESOLUCION-PLE-CNE-2-2-7-2026-RVMR1-signed.pdf | CNE resolution (not yet reviewed) | 405710 | to be computed |

## seccionales-2019 (CNE, Elecciones Seccionales y CPCCS, 24 Mar 2019)

Downloaded 19 Oct 2026. Source URL: to be completed. Catalog and register files (candidates, organizations, cantons, parroquias, provinces, dignidades, voter register by parroquia) are also in the folder.

| File | Content | Bytes | SHA-256 |
| --- | --- | --- | --- |
| Resultados-a-nivel-parroquial-1.sav | Results by parroquia × sex × candidate (no junta level) | 16978653 | 4facf2c35d299e548e3cf69243580ae80675c072908865d77832f65b0a55b636 |
| distrivutivo_recinto.xls | Recintos 2019 (cut-off 29 Jan 2019) with junta ranges by sex; names only, no codes | 1101824 | d447bf4e3337d6e52b425f3a022d5e089977bb34412ecaa8f83377f6f3da4759 |
| CANDIDATOS.sav | Candidates (CANDIDATO_CODIGO to CANDIDATO_NOMBRE), used to name the candidates of the results file | 8941675 | 8f380b5020da8af56bf49d28521a82523bca6fa4a7cf532696aed853790e062d |

## generales-2025 (CNE, Elecciones Generales 2025, first round)

Downloaded 19 Oct 2026. Source URL: to be completed. Catalog and register files are also in the folder.

| File | Content | Bytes | SHA-256 |
| --- | --- | --- | --- |
| Primera-Vuelta.sav | Results by parroquia × sex × candidate (no junta level) | 18998397 | 0fd0f520982e25e7261b7680c833a15554fce7c7a1e906d1deb8a247ffb7dbbd |
| 30oct2024_DistributivoRecintos-Coord1.xls | Recintos 2025 (cut-off 30 Oct 2024) with codes, junta ranges by sex, CDA flag, coordinates; sheet `DISTRIBUTIVO al 30102024` (also a `MATRIZ DE CAMBIOS` sheet with recinto changes since 13 Sep 2024) | 4704256 | 3972230ccddba89ea043f1712e523bfccbc0160ceec6f99fce296363bee67010 |

## Junta-level results 2023 (extracted from the CNE results site)

Source: https://app01.cne.gob.ec/Resultados2023/, one request per junta, extracted 19 Oct 2026 with `scripts/extraccion/scraper_cne2023.js`. Hashes of every checkpoint file and of the consolidated file are in each folder's `*_extraccion_log.json`. The consolidated files are in `data-raw/`: `cne2023_prefecto_pichincha_juntas.csv` and `cne2023_alcalde_quito_juntas.csv`.

| Folder | Content |
| --- | --- |
| pre-consolidacion-prefecto/ | 26 checkpoint files (per parroquia and per canton), extraction log and validation report |
| pre-consolidacion-alcalde/ | 1 checkpoint file (whole canton), extraction log and validation report |

## Junta-level results 2019 and 2025 (extracted from the CNE results sites)

| Folder / file | Content |
| --- | --- |
| `cne2025_presidente_pichincha_juntas.csv` | 2025 Presidente, first round, all Pichincha: 7,061 juntas, 112,976 rows. SHA-256 bfcc5a889726d9f57dbe9d61986a5b70e981bee7cc5a9578df0454c4e562fbd9 |
| `pre-consolidacion-2025-presidente/` | 8 canton files + Juan Montalvo file, extraction log (hashes of every input), validation report. Source https://app01.cne.gob.ec/Resultados2025/, extracted 20 Oct 2026 with `scripts/extraccion/scraper_cne2025.js` |
| `cne2019_prefecto_pichincha_juntas.csv` | 2019 Prefecto, all Pichincha: 6,846 juntas, 123,228 rows, matches the SPSS file. SHA-256 254aa455887fc028e2b3e515789381789fcfd3df0b67513bca7fc7cab56ad9da |
| `cne2019_alcalde_quito_juntas.csv` | 2019 Alcalde Quito: 5,952 juntas, 107,136 rows, matches the SPSS file. SHA-256 5c5b5412990ea8558cf41fee3ad086283bf4048afb7e6c15a6f97fb0572e6c6e |
| `pre-consolidacion-2019-alcalde/` | Canton file, extraction log, validation report. Extracted 21 Oct 2026 with `scripts/extraccion/scraper_cne2019.js` |
| `cne2019_prefecto_pichincha_juntas_parcial.csv` | Superseded by the file above (first run, 5 of 8 cantons); can be deleted |
| `pre-consolidacion-2019-prefecto/` | 8 canton files, extraction log, validation report. Source https://app01.cne.gob.ec/Resultados2019/, extracted 19 to 21 Oct 2026 with `scripts/extraccion/scraper_cne2019.js` |
