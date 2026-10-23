# Conteo rápido por actas: Prefectura de Pichincha y Alcaldía de Quito 2026

Diseño muestral de un conteo rápido por actas para las elecciones seccionales del **29 de noviembre de 2026** en Pichincha: Prefecto/a de Pichincha y Alcalde/sa de Quito. El repositorio contiene los datos públicos utilizados, el código del análisis en R y sus pruebas, de modo que cualquier resultado del diseño puede reproducirse.

Autor: Mauricio Moreno, PhD.

## Qué hace

1. **Reúne los resultados junta por junta** de elecciones anteriores en Pichincha (seccionales 2019 y 2023, primera vuelta presidencial 2025), extraídos de los sitios de consulta de resultados del CNE y validados contra los archivos oficiales del CNE.
2. **Construye el marco muestral** con el distributivo de recintos del CNE para 2026 (corte del 11 de agosto de 2026): todas las juntas de la provincia, en seis estratos geográficos (Quito urbano, Quito rural, Rumiñahui, Mejía, Cayambe y Pedro Moncayo, Noroccidente).
3. **Mide la variabilidad del voto entre juntas** en cada estrato, con las dos últimas seccionales ponderadas (2023 con dos tercios del peso y 2019 con un tercio).
4. **Calcula el tamaño de la muestra** de juntas con una fórmula de muestreo estratificado (asignación de Neyman) y lo ajusta con simulaciones: el sorteo se repite 2000 veces sobre los resultados reales por junta de 2019 y de 2023, y se elige el menor tamaño que cumple el objetivo de precisión en las dos elecciones.
5. **Sortea las juntas** de la muestra con un sorteo sistemático con arranque aleatorio dentro de cada estrato.

## El diseño, en breve

- **Unidad de muestreo:** la junta receptora del voto. Una sola muestra sirve para las dos dignidades: en Quito cada junta seleccionada aporta el acta de Prefectura y la de Alcaldía; fuera de Quito, la de Prefectura.
- **Selección:** en cada estrato, sorteo sistemático sobre la lista de juntas ordenada por cantón, parroquia, zona, recinto, sexo y número de junta, de modo que la muestra se reparte por toda la geografía del estrato. Ningún recinto aporta más de 4 juntas.
- **Estimador:** estimador de razón combinado (votos del candidato sobre votos válidos, con los pesos del diseño). El margen de error se calcula con diferencias sucesivas, el estimador habitual para muestras sistemáticas.
- **Objetivo de precisión:** la cuota del 1ro y del 2do candidato (y, en una de las opciones, la diferencia entre ellos) con ±1 punto porcentual, al 99 % o al 95 % de confianza.
- **Validación:** el diseño se prueba con simulaciones sobre elecciones reales, con pérdidas de juntas en campo y una reserva para cubrirlas. La primera vuelta presidencial de 2025 se usa como prueba de estrés.

## Estructura

```
.
├── _targets.R               # flujo de trabajo del análisis (paquete targets)
├── R/                       # funciones, un archivo por fase
│   ├── 01_ingest.R          #   lectura de los archivos del CNE
│   ├── 02_clean.R           #   depuración de los resultados por junta
│   ├── 03_frame.R           #   marco muestral y estratos
│   ├── 04_parameters.R      #   variabilidad entre juntas
│   ├── 05_sample_size.R     #   tamaño de muestra por fórmula
│   ├── 06_simulation.R      #   simulación del diseño por recintos (de referencia)
│   ├── 06b_junta_design.R   #   diseño por juntas: sorteo, estimación, pérdidas y reserva
│   ├── 07_stability.R       #   ponderación 2019 y 2023, opciones y olas de resultados
│   └── 08_draw.R            #   sorteo de las juntas y listas en Excel
├── data-raw/                # datos originales, sin modificar (ver data-raw/SOURCES.md)
├── scripts/
│   ├── extraccion/          # programas que extrajeron y consolidaron los resultados por junta
│   └── setup/               # instalación inicial de renv
├── tests/testthat/          # pruebas automáticas de cada fase
└── renv.lock                # versiones fijadas de los paquetes de R
```

## Datos

Todos los datos son públicos y provienen del Consejo Nacional Electoral (CNE) del Ecuador:

- **Archivos oficiales del CNE** (SPSS y Excel): resultados por parroquia y sexo, catálogos de cantones y parroquias, registro electoral y distributivos de recintos de 2019, 2023, 2025 y 2026.
- **Resultados por junta** (`data-raw/cne20*_juntas.csv`): los archivos oficiales descargables llegan solo al nivel de parroquia y sexo, de modo que los resultados por junta se extrajeron de los sitios de consulta del CNE, **una consulta a la vez y con pausas entre consultas**. En cada extracción, la suma de los resultados por junta coincide exactamente con el archivo oficial en cada parroquia, sexo y candidato. Los registros de extracción (con la huella SHA-256 de cada archivo) y los informes de validación están en las carpetas `data-raw/pre-consolidacion-*`.

El origen, la fecha y la huella de cada archivo están en `data-raw/SOURCES.md`.

## Cómo reproducir el análisis

Requisitos: R 4.6 o posterior y, solo para volver a extraer los datos, Python 3 y un navegador.

```r
# la primera vez: instala los paquetes en las versiones de renv.lock
renv::restore()

# ejecuta el análisis completo (las simulaciones tardan alrededor de una hora)
targets::tar_make()

# pruebas automáticas
testthat::test_dir("tests/testthat")

# resultados: por ejemplo, el resumen de las opciones de tamaño
targets::tar_read(resumen_opciones)
```

El flujo de trabajo solo vuelve a calcular lo que cambió. Las simulaciones y el sorteo usan semillas fijas, de modo que dan los mismos números en cada ejecución.

## Qué no está en este repositorio

El informe técnico y las listas de juntas sorteadas son entregables para la organización que encargó el diseño y no se publican. Se generan a partir de este mismo código.

## Nota

Este repositorio documenta un diseño muestral. No es un resultado electoral ni una proyección: no contiene estimaciones de la elección de 2026.
