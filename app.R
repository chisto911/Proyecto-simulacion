library(shiny)
library(bslib)
library(plotly)
library(DT)
library(writexl)
library(readxl)

source("R/algoritmos_rng.R", encoding = "UTF-8")
source("R/montecarlo.R", encoding = "UTF-8")
source("R/flujo_uniforme.R", encoding = "UTF-8")
source("R/discretas.R", encoding = "UTF-8")
source("R/continuas.R", encoding = "UTF-8")
source("R/bondad_ajuste.R", encoding = "UTF-8")
source("R/ui_inicio.R", encoding = "UTF-8")

canvas_placeholder <- function(badge, title, description) {
  div(
    class = "canvas-preview",
    span(class = "canvas-badge", badge),
    h4(class = "canvas-title", title),
    p(class = "canvas-desc", description)
  )
}

epn_module_header <- function(title, subtitle, formula_tex) {
  div(
    class = "module-header",
    div(
      class = "module-title-wrap",
      div(class = "module-accent-bar"),
      div(
        h2(class = "module-title", title),
        p(class = "module-subtitle", subtitle)
      )
    ),
    div(class = "formula-pill", formula_tex)
  )
}

epn_footer_bar <- function() {
  div(
    class = "epn-footer",
    div(
      strong("Escuela Politécnica Nacional"),
      span(" · Simulación (2026-B)")
    ),
    div(
      class = "epn-footer-motto",
      "Noviembre - 2026"
    )
  )
}

# Tarjeta de indicador (KPI) con el mismo estilo de las Pestañas 1 y 2.
# estado = "pass" o "fail" colorea la tarjeta según la decisión de una prueba.
kpi_tarjeta <- function(etiqueta, pastilla, numero, subtexto, estado = NULL) {
  estilo_tarjeta <- switch(
    if (is.null(estado)) "ninguno" else estado,
    pass = "border-top-color: #059669;",
    fail = "border-top-color: #D31929;",
    NULL
  )
  estilo_pastilla <- switch(
    if (is.null(estado)) "ninguno" else estado,
    pass = "color: #059669; background-color: #ECFDF5; border-color: rgba(5, 150, 105, 0.3);",
    fail = "color: #D31929; background-color: #FDF2F3; border-color: rgba(211, 25, 41, 0.3);",
    NULL
  )
  div(
    class = "kpi-card",
    style = estilo_tarjeta,
    div(class = "kpi-label", span(etiqueta), span(class = "status-pill-pass", style = estilo_pastilla, pastilla)),
    div(class = "kpi-number", numero),
    div(class = "kpi-subtext", subtexto)
  )
}

ui <- navbarPage(
  title = div(
    class = "epn-brand-wrap",
    div(
      class = "epn-logo-frame",
      tags$img(src = "logo_epn.png", alt = "Escudo Escuela Politécnica Nacional", class = "epn-logo-img")
    ),
    div(
      class = "epn-brand-text",
      span(class = "epn-institution-name", "Escuela Politécnica Nacional"),
      span(class = "epn-app-title", "Facultad de Ciencias"),
      span(class = "epn-motto-sub", "SIMULACIÓN · 2026-B")
    )
  ),
  windowTitle = "Simulación Estocástica | E.P.N. 2026-B",
  id = "main_navbar",
  theme = bs_theme(version = 5),
  header = tagList(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
      tags$link(rel = "icon", type = "image/png", href = "logo_epn.png")
    ),
    withMathJax()
  ),
  footer = epn_footer_bar(),
  selected = "tab_home",

  pestana_inicio(),

  tabPanel(
    title = "1. Pseudoaleatorios",
    value = "tab_rng",
    icon = icon("dice"),
    div(
      class = "module-shell",
      epn_module_header(
        title = "Generación de Números Pseudoaleatorios",
        subtitle = "Implementación desde sus ecuaciones recurrentes sin utilizar generadores nativos como base.",
        formula_tex = "Objetivo: $$U_i \\sim \\text{Uniforme}(0, 1) \\quad \\mathbb{E}[U] = 0.5, \\; \\mathbb{V}(U) = \\frac{1}{12} \\approx 0.08333$$"
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "control-panel",
            div(class = "control-section-title", span("Configuración del Generador"), span("Pestaña 1")),

            selectInput(
              inputId = "rng_method",
              label = "Algoritmo Recurrente",
              choices = c(
                "1. Método Congruencial Lineal (MCL)" = "mcl",
                "2. Método de Lehmer" = "lehmer",
                "3. Método de los Cuadrados Medios" = "midsq"
              ),
              selected = "lehmer"
            ),

            div(class = "control-section-title", span("Ejemplos Rápidos")),
            div(
              class = "preset-group",
              actionButton("preset_lehmer_ok", "Lehmer (x0=4122, c=76)", class = "btn btn-preset"),
              actionButton("preset_lehmer_neg", "Lehmer Negativo (x0=2000)", class = "btn btn-preset"),
              actionButton("preset_ms_ok", "Cuadrados (x0=4122)", class = "btn btn-preset"),
              actionButton("preset_ms_cycle", "Ciclo Corto (x0=3708)", class = "btn btn-preset"),
              actionButton("preset_mcl_std", "MCL Estándar", class = "btn btn-preset")
            ),

            numericInput(
              inputId = "rng_x0",
              label = "Semilla inicial (x0)",
              value = 4122,
              min = 1,
              step = 1
            ),
            div(class = "input-helper", "Valor entero inicial que determina toda la secuencia."),

            conditionalPanel(
              condition = "input.rng_method == 'mcl'",
              fluidRow(
                column(
                  6,
                  numericInput("rng_a", "Multiplicador (a)", value = 1664525, min = 1, step = 1)
                ),
                column(
                  6,
                  numericInput("rng_c_mcl", "Incremento (c)", value = 1013904223, min = 0, step = 1)
                )
              ),
              numericInput("rng_m", "Módulo (m)", value = 4294967296, min = 2, step = 1),
              div(class = "input-helper", "Ecuación: x_{n+1} = (a * x_n + c) mod m")
            ),

            conditionalPanel(
              condition = "input.rng_method == 'lehmer'",
              fluidRow(
                column(
                  6,
                  numericInput("rng_c_lehmer", "Multiplicador c (k cifras)", value = 76, min = 1, step = 1)
                ),
                column(
                  6,
                  numericInput("rng_n_lehmer", "Cifras semilla (n)", value = 4, min = 2, max = 10, step = 1)
                )
              ),
              div(class = "input-helper", "Separa las k cifras izquierdas de x_i * c y las resta a las n derechas.")
            ),

            conditionalPanel(
              condition = "input.rng_method == 'midsq'",
              numericInput("rng_ndigits", "Número de cifras (n par)", value = 4, min = 2, max = 8, step = 2),
              div(class = "input-helper", "Eleva x_i^2 a 2n cifras y extrae las n cifras centrales.")
            ),

            numericInput(
              inputId = "rng_n",
              label = "Cantidad de números a generar (N)",
              value = 50,
              min = 5,
              max = 5000,
              step = 10
            ),

            div(
              class = "d-grid gap-2 mt-3",
              actionButton("btn_run_rng", "Generar Secuencia", icon = icon("play"), class = "btn btn-primary-sim")
            ),

            hr(),
            div(class = "control-section-title", span("Exportación de Datos")),
            div(
              class = "d-flex gap-2",
              downloadButton("dl_rng_xlsx", "Exportar .xlsx", class = "btn btn-secondary-sim flex-fill"),
              downloadButton("dl_rng_csv", "Exportar .csv / .txt", class = "btn btn-secondary-sim flex-fill")
            )
          )
        ),

        column(
          width = 8,
          uiOutput("rng_kpi_cards"),
          uiOutput("rng_xray_panel"),

          fluidRow(
            column(
              width = 7,
              div(
                class = "surface-card",
                div(
                  class = "surface-card-header",
                  h3(class = "surface-card-title", "Histograma Ajustado a la Densidad U(0, 1)"),
                  span(class = "surface-card-meta", "Empírico vs. f(u) = 1")
                ),
                plotlyOutput("rng_hist_plot", height = "320px")
              )
            ),
            column(
              width = 5,
              div(
                class = "surface-card",
                div(
                  class = "surface-card-header",
                  h3(class = "surface-card-title", "Rezagos (u_i vs u_{i+1})"),
                  span(class = "surface-card-meta", "Pares Consecutivos")
                ),
                plotlyOutput("rng_lag_plot", height = "320px")
              )
            )
          ),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Tabla de Datos Generados y Traza Aritmética"),
              span(class = "surface-card-meta", "Desglose de Recurrencia")
            ),
            DTOutput("rng_table")
          )
        )
      )
    )
  ),

  tabPanel(
    title = "2. Montecarlo",
    value = "tab_mc",
    icon = icon("chart-area"),
    div(
      class = "module-shell",
      epn_module_header(
        title = "Aproximación de Integrales por el Método de Montecarlo",
        subtitle = "Estimación estocástica de integrales definidas, evaluación geométrica en la región y análisis de convergencia.",
        formula_tex = "$$\\hat{I}_N = \\frac{b - a}{N} \\sum_{i=1}^{N} f(X_i), \\quad X_i \\sim U(a, b)$$"
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "control-panel",
            div(class = "control-section-title", span("Definición de la Integral"), span("Pestaña 2")),
            textInput(
              inputId = "mc_func",
              label = "Expresión de la función f(x)",
              value = "exp(-x^2)",
              placeholder = "Ej: sin(x), exp(-x^2), x^3 + 2*x"
            ),
            div(class = "input-helper", "Escriba f(x) usando sintaxis de R (ej. exp(-x^2), sin(pi*x), sqrt(1 - x^2))."),

            div(class = "control-section-title", span("Funciones de Prueba")),
            div(
              class = "preset-group",
              actionButton("preset_mc_gauss", "exp(-x^2) en [0, 1]", class = "btn btn-preset"),
              actionButton("preset_mc_trig", "sin(pi*x) en [0, 1]", class = "btn btn-preset"),
              actionButton("preset_mc_poly", "x^2 + 1 en [0, 2]", class = "btn btn-preset"),
              actionButton("preset_mc_osc", "cos(2*pi*x) en [0, 1]", class = "btn btn-preset")
            ),

            fluidRow(
              column(
                6,
                numericInput("mc_a", "Límite inferior (a)", value = 0, step = 0.5)
              ),
              column(
                6,
                numericInput("mc_b", "Límite superior (b)", value = 1, step = 0.5)
              )
            ),

            fluidRow(
              column(
                7,
                numericInput(
                  inputId = "mc_n",
                  label = "Simulaciones (N)",
                  value = 1000,
                  min = 10,
                  max = 50000,
                  step = 100
                )
              ),
              column(
                5,
                numericInput(
                  inputId = "mc_seed",
                  label = "Semilla",
                  value = 2026,
                  min = 1,
                  step = 1
                )
              )
            ),

            selectInput(
              inputId = "mc_rng_source",
              label = "Fuente pseudoaleatoria",
              choices = c(
                "Generador MCL interno" = "internal_mcl",
                "Usar secuencia activa de Pestaña 1" = "tab1_seq"
              ),
              selected = "internal_mcl"
            ),

            div(
              class = "d-grid gap-2 mt-3",
              actionButton("btn_run_mc", "Ejecutar Simulación Montecarlo", icon = icon("play"), class = "btn btn-primary-sim")
            ),

            hr(),
            div(class = "control-section-title", span("Exportación de Simulaciones")),
            div(
              class = "d-flex gap-2",
              downloadButton("dl_mc_xlsx", "Exportar .xlsx", class = "btn btn-secondary-sim flex-fill"),
              downloadButton("dl_mc_csv", "Exportar .csv", class = "btn btn-secondary-sim flex-fill")
            )
          )
        ),

        column(
          width = 8,
          uiOutput("mc_kpi_cards"),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "1. Gráfico Geométrico: Evaluación de Puntos Aleatorios en [a, b]"),
              span(class = "surface-card-meta", "Muestreo en Región")
            ),
            plotlyOutput("mc_geom_plot", height = "350px")
          ),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "2. Gráfico de Convergencia según el Número de Simulaciones (N)"),
              span(class = "surface-card-meta", "IC 95% vs. Valor Exacto")
            ),
            plotlyOutput("mc_conv_plot", height = "330px")
          ),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Comparación Numérica Detallada"),
              span(class = "surface-card-meta", "Resumen de Precisión")
            ),
            DTOutput("mc_summary_table")
          )
        )
      )
    )
  ),

  tabPanel(
    title = "3. V.A. Discretas",
    value = "tab_disc",
    icon = icon("chart-column"),
    div(
      class = "module-shell",
      epn_module_header(
        title = "Generación de Variables Aleatorias Discretas",
        subtitle = "Simulación de modelos discretos clásicos y leyes arbitrarias mediante el Método de la Transformación Inversa Discreta.",
        formula_tex = "$$X = x_j \\iff \\sum_{i=1}^{j-1} p_i < U \\le \\sum_{i=1}^{j} p_i, \\quad \\sum_{i=1}^{k} p_i = 1$$"
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "control-panel",
            div(class = "control-section-title", span("Ley de Probabilidad Discreta"), span("Pestaña 3")),
            selectInput(
              inputId = "disc_type",
              label = "Seleccione la Distribución",
              choices = c(
                "Binomial(n, p)" = "binom",
                "Poisson(lambda)" = "pois",
                "Geométrica(p)" = "geom",
                "Personalizada (Transformación Inversa)" = "custom"
              ),
              selected = "binom"
            ),

            conditionalPanel(
              condition = "input.disc_type == 'binom'",
              numericInput("disc_binom_n", "Número de ensayos (n)", value = 10, min = 1, max = 1000, step = 1),
              numericInput("disc_binom_p", "Probabilidad de éxito (p)", value = 0.4, min = 0.01, max = 0.99, step = 0.05)
            ),

            conditionalPanel(
              condition = "input.disc_type == 'pois'",
              numericInput("disc_pois_lambda", "Tasa media (lambda > 0)", value = 3.5, min = 0.1, max = 100, step = 0.5)
            ),

            conditionalPanel(
              condition = "input.disc_type == 'geom'",
              numericInput("disc_geom_p", "Probabilidad de éxito (p)", value = 0.3, min = 0.01, max = 0.99, step = 0.05)
            ),

            conditionalPanel(
              condition = "input.disc_type == 'custom'",
              textInput("disc_custom_x", "Valores del soporte X = {x1, ..., xk}", value = "1, 2, 3, 4"),
              textInput("disc_custom_p", "Probabilidades P(X = xi)", value = "0.15, 0.35, 0.30, 0.20"),
              div(class = "input-helper", "La suma de probabilidades debe ser igual a 1."),
              div(
                class = "preset-group",
                actionButton("preset_disc_ross", "Ross 4.3: P = (0.30, 0.20, 0.35, 0.15)", class = "btn btn-preset"),
                actionButton("preset_disc_dado", "Dado justo (1, ..., 6)", class = "btn btn-preset")
              )
            ),

            fluidRow(
              column(
                7,
                numericInput("disc_n_sim", "Tamaño de muestra (N)", value = 1000, min = 10, max = 20000, step = 100)
              ),
              column(
                5,
                numericInput("disc_seed", "Semilla", value = 2026, min = 1, step = 1)
              )
            ),
            div(class = "input-helper", "Los uniformes provienen del generador MCL propio (no se usa runif)."),

            div(
              class = "d-grid gap-2 mt-3",
              actionButton("btn_run_disc", "Simular Variable Discreta", icon = icon("play"), class = "btn btn-primary-sim")
            ),

            hr(),
            div(class = "control-section-title", span("Exportación")),
            div(
              class = "d-flex gap-2",
              downloadButton("dl_disc_xlsx", "Descargar .xlsx", class = "btn btn-secondary-sim flex-fill"),
              downloadButton("dl_disc_txt", "Descargar .txt", class = "btn btn-secondary-sim flex-fill")
            )
          )
        ),

        column(
          width = 8,
          uiOutput("disc_kpi_cards"),
          uiOutput("disc_method_banner"),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Gráfico de Barras Comparativo: Frecuencia Empírica vs. Probabilidad Teórica"),
              span(class = "surface-card-meta", "Transformación Inversa Discreta")
            ),
            plotlyOutput("disc_bar_plot", height = "360px")
          ),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Resumen Descriptivo y Tabla de Frecuencias"),
              span(class = "surface-card-meta", "Momentos Muestrales vs. Teóricos")
            ),
            DTOutput("disc_freq_table")
          )
        )
      )
    )
  ),

  tabPanel(
    title = "4. V.A. Continuas",
    value = "tab_cont",
    icon = icon("wave-square"),
    div(
      class = "module-shell",
      epn_module_header(
        title = "Generación de Variables Aleatorias Continuas",
        subtitle = "Simulación de distribuciones continuas clásicas y densidades personalizadas mediante Transformación Inversa o Aceptación y Rechazo.",
        formula_tex = "$$X = F^{-1}(U) \\quad \\text{o} \\quad U_2 \\le \\frac{f(Y)}{M \\cdot g(Y)}$$"
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "control-panel",
            div(class = "control-section-title", span("Modelo Continuo"), span("Pestaña 4")),
            selectInput(
              inputId = "cont_type",
              label = "Seleccione la Distribución",
              choices = c(
                "Uniforme(a, b)" = "unif",
                "Exponencial(lambda)" = "exp",
                "Normal(mu, sigma)" = "norm",
                "Personalizada f(x)" = "custom"
              ),
              selected = "exp"
            ),

            conditionalPanel(
              condition = "input.cont_type == 'unif'",
              fluidRow(
                column(6, numericInput("cont_unif_a", "Mínimo (a)", value = 0)),
                column(6, numericInput("cont_unif_b", "Máximo (b)", value = 10))
              )
            ),

            conditionalPanel(
              condition = "input.cont_type == 'exp'",
              numericInput("cont_exp_rate", "Tasa (lambda > 0)", value = 1.5, min = 0.05, step = 0.25)
            ),

            conditionalPanel(
              condition = "input.cont_type == 'norm'",
              fluidRow(
                column(6, numericInput("cont_norm_mu", "Media (mu)", value = 0)),
                column(6, numericInput("cont_norm_sd", "Desv. Est. (sigma)", value = 1, min = 0.01))
              )
            ),

            conditionalPanel(
              condition = "input.cont_type == 'custom'",
              selectInput(
                inputId = "cont_custom_method",
                label = "Método de Simulación",
                choices = c(
                  "Aceptación y Rechazo" = "ar",
                  "Transformación Inversa" = "inv"
                )
              ),
              textInput("cont_custom_fx", "Función de densidad f(x)", value = "20 * x * (1 - x)^3"),
              fluidRow(
                column(6, numericInput("cont_custom_a", "Soporte mín (a)", value = 0)),
                column(6, numericInput("cont_custom_b", "Soporte máx (b)", value = 1))
              ),
              div(class = "input-helper", "Si f(x) no integra 1 en [a, b], se normaliza automáticamente."),
              div(
                class = "preset-group",
                actionButton("preset_cont_beta", "Ross 5d: 20x(1−x)³ en [0, 1]", class = "btn btn-preset"),
                actionButton("preset_cont_tri", "Ross 5.2: densidad por tramos en [2, 6]", class = "btn btn-preset"),
                actionButton("preset_cont_cuad", "3x² en [0, 1]", class = "btn btn-preset")
              )
            ),

            fluidRow(
              column(
                7,
                numericInput("cont_n_sim", "Tamaño de muestra (N)", value = 2000, min = 20, max = 50000, step = 100)
              ),
              column(
                5,
                numericInput("cont_seed", "Semilla", value = 2026, min = 1, step = 1)
              )
            ),
            div(class = "input-helper", "Los uniformes provienen del generador MCL propio (no se usa runif)."),

            div(
              class = "d-grid gap-2 mt-3",
              actionButton("btn_run_cont", "Simular Variable Continua", icon = icon("play"), class = "btn btn-primary-sim")
            ),

            hr(),
            div(class = "control-section-title", span("Exportación")),
            div(
              class = "d-flex gap-2",
              downloadButton("dl_cont_xlsx", "Descargar .xlsx", class = "btn btn-secondary-sim flex-fill"),
              downloadButton("dl_cont_txt", "Descargar .txt", class = "btn btn-secondary-sim flex-fill")
            )
          )
        ),

        column(
          width = 8,
          uiOutput("cont_kpi_cards"),
          uiOutput("cont_method_banner"),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Histograma con Curva de Densidad Empírica frente a la Teórica"),
              span(class = "surface-card-meta", "Ajuste de Densidad f(x)")
            ),
            plotlyOutput("cont_hist_plot", height = "360px")
          ),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Función de Distribución Acumulada (CDF Empírica vs. Teórica)"),
              span(class = "surface-card-meta", "F_n(x) vs F(x)")
            ),
            plotlyOutput("cont_cdf_plot", height = "330px")
          )
        )
      )
    )
  ),

  tabPanel(
    title = "5. Bondad de Ajuste",
    value = "tab_gof",
    icon = icon("square-check"),
    div(
      class = "module-shell",
      epn_module_header(
        title = "Pruebas de Bondad de Ajuste y Diagnóstico Estocástico",
        subtitle = "Verificación formal de uniformidad/ajuste (Kolmogorov-Smirnov) e independencia secuencial (Prueba de Rachas).",
        formula_tex = "$$D_{KS} = \\sup_x |F_n(x) - F(x)| \\quad \\cdot \\quad Z_{Rachas} = \\frac{R - \\mathbb{E}[R]}{\\sqrt{\\mathbb{V}(R)}}$$"
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "control-panel",
            div(class = "control-section-title", span("Origen de la Muestra"), span("Pestaña 5")),
            radioButtons(
              inputId = "gof_source",
              label = "Seleccione los datos a evaluar",
              choices = c(
                "Usar números generados en Pestaña 1" = "internal",
                "Usar muestra simulada en Pestaña 4" = "cont",
                "Subir archivo externo (.csv o .xlsx)" = "upload"
              ),
              selected = "internal"
            ),

            conditionalPanel(
              condition = "input.gof_source == 'upload'",
              fileInput(
                inputId = "gof_file",
                label = "Cargar archivo de datos (.csv / .xlsx)",
                accept = c(".csv", ".xlsx", ".txt"),
                buttonLabel = "Explorar...",
                placeholder = "Ningún archivo seleccionado"
              ),
              uiOutput("gof_col_ui")
            ),

            div(class = "control-section-title", span("Distribución bajo H0 (prueba KS)")),
            selectInput(
              inputId = "gof_dist",
              label = "Distribución teórica F0",
              choices = c(
                "Uniforme(0, 1)" = "unif01",
                "Uniforme(a, b)" = "unif",
                "Normal(mu, sigma)" = "norm",
                "Exponencial(lambda)" = "exp"
              ),
              selected = "unif01"
            ),
            conditionalPanel(
              condition = "input.gof_dist != 'unif01'",
              checkboxInput("gof_estimar", "Estimar los parámetros desde la muestra", value = TRUE),
              conditionalPanel(
                condition = "!input.gof_estimar && input.gof_dist == 'unif'",
                fluidRow(
                  column(6, numericInput("gof_unif_a", "a", value = 0)),
                  column(6, numericInput("gof_unif_b", "b", value = 1))
                )
              ),
              conditionalPanel(
                condition = "!input.gof_estimar && input.gof_dist == 'norm'",
                fluidRow(
                  column(6, numericInput("gof_norm_mu", "mu", value = 0)),
                  column(6, numericInput("gof_norm_sd", "sigma", value = 1, min = 0.0001))
                )
              ),
              conditionalPanel(
                condition = "!input.gof_estimar && input.gof_dist == 'exp'",
                numericInput("gof_exp_rate", "lambda", value = 1, min = 0.0001)
              ),
              div(class = "input-helper", "Con parámetros estimados, el valor p de KS es aproximado (conservador).")
            ),

            div(class = "control-section-title", span("Prueba de independencia")),
            selectInput(
              inputId = "gof_runs_type",
              label = "Tipo de prueba de rachas",
              choices = c(
                "Rachas sobre / bajo la mediana" = "mediana",
                "Rachas ascendentes / descendentes" = "ascendente"
              ),
              selected = "mediana"
            ),

            selectInput(
              inputId = "gof_alpha",
              label = "Nivel de significancia (alpha)",
              choices = c(
                "alpha = 0.01 (99% confianza)" = "0.01",
                "alpha = 0.05 (95% confianza)" = "0.05",
                "alpha = 0.10 (90% confianza)" = "0.10"
              ),
              selected = "0.05"
            ),

            div(
              class = "d-grid gap-2 mt-3",
              actionButton("btn_run_gof", "Ejecutar Pruebas Estadísticas", icon = icon("play"), class = "btn btn-primary-sim")
            )
          )
        ),

        column(
          width = 8,
          uiOutput("gof_kpi_cards"),
          uiOutput("gof_banner"),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Tabla de Contraste de Hipótesis (Estadístico, Valor p y Decisión)"),
              span(class = "surface-card-meta", "Nivel alpha configurable")
            ),
            DTOutput("gof_table")
          ),

          div(
            class = "surface-card",
            div(
              class = "surface-card-header",
              h3(class = "surface-card-title", "Gráficos Diagnósticos de Ajuste e Independencia"),
              span(class = "surface-card-meta", "ECDF + Secuencia de Rachas")
            ),
            fluidRow(
              column(6, plotlyOutput("gof_ecdf_plot", height = "340px")),
              column(6, plotlyOutput("gof_runs_plot", height = "340px"))
            )
          )
        )
      )
    )
  ),

  # Logo de la Facultad de Ciencias en el extremo derecho de la barra
  nav_spacer(),
  nav_item(
    if (file.exists("www/logo_fc.png")) {
      div(
        class = "epn-logo-frame fc-logo-frame",
        tags$img(src = "logo_fc.png", alt = "Facultad de Ciencias", class = "epn-logo-img")
      )
    }
  )
)

server <- function(input, output, session) {

  # Navegación desde la pestaña de inicio
  ir_a <- function(pestana) updateNavbarPage(session, "main_navbar", selected = pestana)
  observeEvent(input$go_rng_hero, ir_a("tab_rng"))
  observeEvent(input$go_rng, ir_a("tab_rng"))
  observeEvent(input$go_mc, ir_a("tab_mc"))
  observeEvent(input$go_disc, ir_a("tab_disc"))
  observeEvent(input$go_cont, ir_a("tab_cont"))
  observeEvent(input$go_gof, ir_a("tab_gof"))

  rng_trigger <- reactiveVal(1L)

  observeEvent(input$btn_run_rng, {
    rng_trigger(rng_trigger() + 1L)
  })

  observeEvent(input$preset_lehmer_ok, {
    updateSelectInput(session, "rng_method", selected = "lehmer")
    updateNumericInput(session, "rng_x0", value = 4122)
    updateNumericInput(session, "rng_c_lehmer", value = 76)
    updateNumericInput(session, "rng_n_lehmer", value = 4)
    updateNumericInput(session, "rng_n", value = 50)
    rng_trigger(rng_trigger() + 1L)
  })

  observeEvent(input$preset_lehmer_neg, {
    updateSelectInput(session, "rng_method", selected = "lehmer")
    updateNumericInput(session, "rng_x0", value = 2000)
    updateNumericInput(session, "rng_c_lehmer", value = 50)
    updateNumericInput(session, "rng_n_lehmer", value = 4)
    updateNumericInput(session, "rng_n", value = 10)
    rng_trigger(rng_trigger() + 1L)
  })

  observeEvent(input$preset_ms_ok, {
    updateSelectInput(session, "rng_method", selected = "midsq")
    updateNumericInput(session, "rng_x0", value = 4122)
    updateNumericInput(session, "rng_ndigits", value = 4)
    updateNumericInput(session, "rng_n", value = 50)
    rng_trigger(rng_trigger() + 1L)
  })

  observeEvent(input$preset_ms_cycle, {
    updateSelectInput(session, "rng_method", selected = "midsq")
    updateNumericInput(session, "rng_x0", value = 3708)
    updateNumericInput(session, "rng_ndigits", value = 4)
    updateNumericInput(session, "rng_n", value = 15)
    rng_trigger(rng_trigger() + 1L)
  })

  observeEvent(input$preset_mcl_std, {
    updateSelectInput(session, "rng_method", selected = "mcl")
    updateNumericInput(session, "rng_x0", value = 12345)
    updateNumericInput(session, "rng_a", value = 1664525)
    updateNumericInput(session, "rng_c_mcl", value = 1013904223)
    updateNumericInput(session, "rng_m", value = 4294967296)
    updateNumericInput(session, "rng_n", value = 200)
    rng_trigger(rng_trigger() + 1L)
  })

  rng_resultado <- reactive({
    rng_trigger()
    metodo <- isolate(input$rng_method)
    x0 <- isolate(req(input$rng_x0))
    N <- min(max(as.integer(isolate(req(input$rng_n))), 2L), 10000L)

    tryCatch({
      if (metodo == "mcl") {
        generar_mcl(
          x0 = x0,
          a = isolate(req(input$rng_a)),
          c_inc = isolate(req(input$rng_c_mcl)),
          m = isolate(req(input$rng_m)),
          N = N
        )
      } else if (metodo == "lehmer") {
        generar_lehmer(
          x0 = x0,
          c_mult = isolate(req(input$rng_c_lehmer)),
          n_cifras = isolate(req(input$rng_n_lehmer)),
          N = N
        )
      } else {
        generar_cuadrados_medios(
          x0 = x0,
          n_cifras = isolate(req(input$rng_ndigits)),
          N = N
        )
      }
    }, error = function(e) {
      showNotification(paste("Error en parámetros:", e$message), type = "error", duration = 5)
      NULL
    })
  })

  output$rng_kpi_cards <- renderUI({
    res <- req(rng_resultado())
    u <- res$df$u_i
    diag <- res$diagnostico

    media_emp <- mean(u, na.rm = TRUE)
    var_emp <- if (length(u) > 1) var(u, na.rm = TRUE) else NA_real_
    err_media <- abs(media_emp - 0.5)
    err_var <- if (!is.na(var_emp)) abs(var_emp - (1 / 12)) else NA_real_

    div(
      class = "kpi-grid",
      div(
        class = "kpi-card",
        div(
          class = "kpi-label",
          span("Media Empírica E[X]"),
          span(class = "status-pill-pass", "Teórica: 0.5000")
        ),
        div(class = "kpi-number", sprintf("%.5f", media_emp)),
        div(class = "kpi-subtext", sprintf("Error absoluto |E[X] - 0.5| = %.5f", err_media))
      ),
      div(
        class = "kpi-card",
        div(
          class = "kpi-label",
          span("Varianza Empírica V(X)"),
          span(class = "status-pill-pass", "Teórica: 0.08333")
        ),
        div(class = "kpi-number", ifelse(is.na(var_emp), "N/A", sprintf("%.5f", var_emp))),
        div(
          class = "kpi-subtext",
          ifelse(is.na(err_var), "Se requieren al menos 2 valores",
                 sprintf("Error absoluto |V(X) - 1/12| = %.5f", err_var))
        )
      ),
      div(
        class = "kpi-card",
        div(
          class = "kpi-label",
          span("Estado de la Secuencia"),
          span(class = "status-pill-pass", sprintf("n = %d datos", length(u)))
        ),
        div(class = "kpi-number", style = "font-size: 1.15rem;", diag$badge_kpi),
        div(class = "kpi-subtext", diag$titulo)
      )
    )
  })

  output$rng_xray_panel <- renderUI({
    res <- req(rng_resultado())
    df <- res$df
    diag <- res$diagnostico

    n_mostrar <- min(nrow(df), 6L)
    filas_xray <- df[seq_len(n_mostrar), , drop = FALSE]

    cards_ui <- lapply(seq_len(nrow(filas_xray)), function(k) {
      row <- filas_xray[k, ]
      card_cls <- "xray-step-card"
      if (row$Estado %in% c("Inicio de Ciclo", "En Ciclo")) card_cls <- paste(card_cls, "is-cycle")
      if (row$Estado %in% c("Iterante Negativo", "Colapso a 0")) card_cls <- paste(card_cls, "is-error")

      div(
        class = card_cls,
        div(
          class = "xray-step-head",
          strong(sprintf("i = %d (x_%d = %.0f)", row$Iteracion, row$Iteracion, row$Semilla_xi)),
          span(sprintf("u_%d = %.4f", row$Iteracion, row$u_i))
        ),
        HTML(row$RayosX_HTML)
      )
    })

    div(
      class = paste0("cycle-banner status-", diag$estado_tipo),
      div(
        class = "cycle-banner-title",
        span(diag$titulo),
        span(class = "surface-card-meta", "Desglose Aritmético y Ciclos")
      ),
      p(class = "cycle-banner-desc", diag$mensaje),
      div(
        class = "xray-legend mt-2",
        HTML("<span><span class='xray-cut'>Rojo</span> = Cifras separadas / descartadas</span>"),
        HTML("<span><span class='xray-core'>Dorado / Azul</span> = Cifras centrales o retenidas</span>")
      ),
      div(class = "xray-grid", cards_ui)
    )
  })

  output$rng_hist_plot <- renderPlotly({
    res <- req(rng_resultado())
    u <- res$df$u_i
    nbins_opt <- max(5L, min(20L, as.integer(ceiling(sqrt(length(u))))))

    plot_ly() %>%
      add_histogram(
        x = u,
        histnorm = "probability density",
        nbinsx = nbins_opt,
        name = "Densidad Empírica",
        marker = list(
          color = "rgba(15, 31, 56, 0.82)",
          line = list(color = "#FFFFFF", width = 1.2)
        ),
        hovertemplate = "Intervalo: %{x}<br>Densidad empírica: %{y:.3f}<extra></extra>"
      ) %>%
      add_lines(
        x = c(0, 1),
        y = c(1, 1),
        name = "Teórica U(0,1): f(u) = 1",
        line = list(color = "#D31929", width = 3, dash = "dash"),
        hovertemplate = "u = %{x:.2f}<br>Densidad teórica f(u) = 1.000<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "Valor pseudoaleatorio u_i", range = c(-0.02, 1.02), zeroline = FALSE),
        yaxis = list(title = "Densidad de probabilidad", rangemode = "tozero"),
        legend = list(orientation = "h", y = 1.12, x = 0),
        margin = list(t = 30, r = 15, b = 45, l = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$rng_lag_plot <- renderPlotly({
    res <- req(rng_resultado())
    df <- res$df
    u <- df$u_i

    if (length(u) < 2) {
      return(
        plot_ly() %>%
          add_annotations(
            text = "Se requieren al menos 2 iteraciones válidas para graficar rezagos (u_i, u_{i+1}).",
            x = 0.5, y = 0.5, showarrow = FALSE
          ) %>%
          layout(paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "#F8FAFC")
      )
    }

    u_curr <- u[-length(u)]
    u_next <- u[-1]
    estados_pares <- df$Estado[-1]
    es_ciclo <- estados_pares %in% c("Inicio de Ciclo", "En Ciclo", "Colapso a 0")

    p_lag <- plot_ly()
    if (any(!es_ciclo)) p_lag <- p_lag %>%
      add_markers(
        x = u_curr[!es_ciclo],
        y = u_next[!es_ciclo],
        name = "Pares Activos",
        marker = list(color = "#0F1F38", size = 8, opacity = 0.85),
        hovertemplate = "u_i = %{x:.4f}<br>u_{i+1} = %{y:.4f}<extra>Activo</extra>"
      )
    if (any(es_ciclo)) p_lag <- p_lag %>%
      add_markers(
        x = u_curr[es_ciclo],
        y = u_next[es_ciclo],
        name = "En Ciclo / Colapso",
        marker = list(
          color = "#D31929",
          size = 10,
          symbol = "diamond",
          line = list(color = "#E5AE32", width = 1.5)
        ),
        hovertemplate = "u_i = %{x:.4f}<br>u_{i+1} = %{y:.4f}<extra>Ciclo Repetitivo</extra>"
      )
    p_lag %>%
      layout(
        xaxis = list(title = "u_i", range = c(-0.03, 1.03)),
        yaxis = list(title = "u_{i+1}", range = c(-0.03, 1.03)),
        legend = list(orientation = "h", y = 1.12, x = 0),
        margin = list(t = 30, r = 15, b = 45, l = 45),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$rng_table <- renderDT({
    res <- req(rng_resultado())
    df <- res$df

    tabla_vista <- data.frame(
      `Iteración (i)` = df$Iteracion,
      `Semilla (x_i)` = df$Semilla_xi,
      `Desglose` = df$RayosX_HTML,
      `Siguiente (x_{i+1})` = df$Siguiente_x,
      `Pseudoaleatorio (u_i)` = df$u_i,
      `Estado` = df$Estado,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    datatable(
      tabla_vista,
      escape = FALSE,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(6, 10, 25, 50, 100),
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ iteraciones",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      )
    )
  })

  datos_exportar_rng <- reactive({
    res <- req(rng_resultado())
    df <- res$df
    data.frame(
      Iteracion_i = df$Iteracion,
      Semilla_xi = df$Semilla_xi,
      Operacion_Recurrente = df$Operacion,
      Siguiente_x = df$Siguiente_x,
      Numero_Pseudoaleatorio_ui = df$u_i,
      Estado_Secuencia = df$Estado,
      Media_Empirica = mean(df$u_i, na.rm = TRUE),
      Varianza_Empirica = if (nrow(df) > 1) var(df$u_i, na.rm = TRUE) else NA_real_,
      stringsAsFactors = FALSE
    )
  })

  output$dl_rng_xlsx <- downloadHandler(
    filename = function() {
      sprintf("Numeros_Pseudoaleatorios_%s_%s.xlsx", input$rng_method, Sys.Date())
    },
    content = function(file) {
      write_xlsx(datos_exportar_rng(), path = file)
    }
  )

  output$dl_rng_csv <- downloadHandler(
    filename = function() {
      sprintf("Numeros_Pseudoaleatorios_%s_%s.csv", input$rng_method, Sys.Date())
    },
    content = function(file) {
      write.csv(datos_exportar_rng(), file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  mc_trigger <- reactiveVal(1L)

  observeEvent(input$btn_run_mc, {
    mc_trigger(mc_trigger() + 1L)
  })

  observeEvent(input$preset_mc_gauss, {
    updateTextInput(session, "mc_func", value = "exp(-x^2)")
    updateNumericInput(session, "mc_a", value = 0)
    updateNumericInput(session, "mc_b", value = 1)
    updateNumericInput(session, "mc_n", value = 1500)
    mc_trigger(mc_trigger() + 1L)
  })

  observeEvent(input$preset_mc_trig, {
    updateTextInput(session, "mc_func", value = "sin(pi * x)")
    updateNumericInput(session, "mc_a", value = 0)
    updateNumericInput(session, "mc_b", value = 1)
    updateNumericInput(session, "mc_n", value = 1500)
    mc_trigger(mc_trigger() + 1L)
  })

  observeEvent(input$preset_mc_poly, {
    updateTextInput(session, "mc_func", value = "x^2 + 1")
    updateNumericInput(session, "mc_a", value = 0)
    updateNumericInput(session, "mc_b", value = 2)
    updateNumericInput(session, "mc_n", value = 1500)
    mc_trigger(mc_trigger() + 1L)
  })

  observeEvent(input$preset_mc_osc, {
    updateTextInput(session, "mc_func", value = "cos(2 * pi * x) + 0.5")
    updateNumericInput(session, "mc_a", value = 0)
    updateNumericInput(session, "mc_b", value = 1)
    updateNumericInput(session, "mc_n", value = 2000)
    mc_trigger(mc_trigger() + 1L)
  })

  mc_resultado <- reactive({
    mc_trigger()
    expr_str <- isolate(req(input$mc_func))
    a_val <- isolate(req(input$mc_a))
    b_val <- isolate(req(input$mc_b))
    n_val <- min(max(as.integer(isolate(req(input$mc_n))), 10L), 50000L)
    semilla_val <- isolate(req(input$mc_seed))
    fuente <- isolate(input$mc_rng_source)

    u_ext <- NULL
    if (identical(fuente, "tab1_seq")) {
      res_tab1 <- isolate(rng_resultado())
      if (!is.null(res_tab1) && nrow(res_tab1$df) >= 5L) {
        u_ext <- res_tab1$df$u_i
      }
    }

    tryCatch({
      simular_montecarlo(
        expr_texto = expr_str,
        a = a_val,
        b = b_val,
        N = n_val,
        semilla = semilla_val,
        u_externos = u_ext
      )
    }, error = function(e) {
      showNotification(paste("Montecarlo:", e$message), type = "error", duration = 6)
      NULL
    })
  })

  output$mc_kpi_cards <- renderUI({
    res <- req(mc_resultado())

    ref_txt <- if (is.na(res$valor_ref)) "No disponible" else sprintf("%.6f", res$valor_ref)
    err_sub <- if (is.na(res$err_abs)) {
      "Sin referencia numérica"
    } else if (is.na(res$err_rel_pct)) {
      sprintf("Error abs: %.6f", res$err_abs)
    } else {
      sprintf("Error abs: %.6f (%.3f%%)", res$err_abs, res$err_rel_pct)
    }

    div(
      class = "kpi-grid",
      div(
        class = "kpi-card",
        div(
          class = "kpi-label",
          span("Valor Aproximado"),
          span(class = "status-pill-pass", sprintf("N = %d", nrow(res$df_puntos)))
        ),
        div(class = "kpi-number", sprintf("%.6f", res$valor_aprox)),
        div(class = "kpi-subtext", sprintf("IC 95%%: [%.5f, %.5f]", res$ic_inf, res$ic_sup))
      ),
      div(
        class = "kpi-card",
        div(
          class = "kpi-label",
          span("Integración de Referencia"),
          span(class = "status-pill-pass", "Exacta")
        ),
        div(class = "kpi-number", ref_txt),
        div(class = "kpi-subtext", err_sub)
      ),
      div(
        class = "kpi-card",
        div(
          class = "kpi-label",
          span("Error Estándar (SE)"),
          span(class = "status-pill-pass", "O(1/sqrt(N))")
        ),
        div(class = "kpi-number", sprintf("%.6f", res$error_estandar)),
        div(class = "kpi-subtext", sprintf("Est. geométrica (dardos): %.5f", res$valor_geometrico))
      )
    )
  })

  output$mc_geom_plot <- renderPlotly({
    res <- req(mc_resultado())
    curva <- res$df_curva
    pts <- res$df_puntos

    if (nrow(pts) > 2500L) {
      idx_muestra <- round(seq(1, nrow(pts), length.out = 2500L))
      pts_plot <- pts[idx_muestra, , drop = FALSE]
    } else {
      pts_plot <- pts
    }

    pts_pos <- pts_plot[pts_plot$Clasificacion == "Bajo f(x) (+)", , drop = FALSE]
    pts_neg <- pts_plot[pts_plot$Clasificacion == "Región negativa (-)", , drop = FALSE]
    pts_out <- pts_plot[pts_plot$Clasificacion == "Fuera de la región", , drop = FALSE]

    p <- plot_ly()

    p <- p %>%
      add_ribbons(
        data = curva,
        x = ~x,
        ymin = 0,
        ymax = ~y,
        name = "Área de f(x)",
        fillcolor = "rgba(15, 31, 56, 0.12)",
        line = list(color = "rgba(0,0,0,0)"),
        hoverinfo = "skip"
      )

    if (nrow(pts_out) > 0) {
      p <- p %>% add_markers(
        data = pts_out,
        x = ~x_i,
        y = ~y_aleatorio,
        name = "Fuera de la región",
        marker = list(color = "rgba(100, 116, 139, 0.38)", size = 5),
        hovertemplate = "x = %{x:.4f}<br>y = %{y:.4f}<extra>Fuera</extra>"
      )
    }

    if (nrow(pts_pos) > 0) {
      p <- p %>% add_markers(
        data = pts_pos,
        x = ~x_i,
        y = ~y_aleatorio,
        name = "Dentro (+)",
        marker = list(color = "rgba(15, 31, 56, 0.85)", size = 6),
        hovertemplate = "x = %{x:.4f}<br>y = %{y:.4f}<extra>Dentro (+)</extra>"
      )
    }

    if (nrow(pts_neg) > 0) {
      p <- p %>% add_markers(
        data = pts_neg,
        x = ~x_i,
        y = ~y_aleatorio,
        name = "Dentro (-)",
        marker = list(color = "rgba(229, 174, 50, 0.9)", size = 6),
        hovertemplate = "x = %{x:.4f}<br>y = %{y:.4f}<extra>Dentro (-)</extra>"
      )
    }

    p %>%
      add_lines(
        data = curva,
        x = ~x,
        y = ~y,
        name = "Curva f(x)",
        line = list(color = "#D31929", width = 3),
        hovertemplate = "x = %{x:.4f}<br>f(x) = %{y:.4f}<extra>f(x)</extra>"
      ) %>%
      layout(
        xaxis = list(title = "x en [a, b]", zeroline = FALSE),
        yaxis = list(title = "f(x)", zeroline = TRUE, zerolinecolor = "#94A3B8"),
        legend = list(orientation = "h", y = 1.13, x = 0),
        margin = list(t = 32, r = 15, b = 45, l = 55),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$mc_conv_plot <- renderPlotly({
    res <- req(mc_resultado())
    pts <- res$df_puntos

    if (nrow(pts) > 1500L) {
      idx_conv <- unique(c(1L:30L, round(seq(31L, nrow(pts), length.out = 1200L)), nrow(pts)))
      df_c <- pts[idx_conv, , drop = FALSE]
    } else {
      df_c <- pts
    }

    df_c$ic_low <- df_c$Estimacion_Acumulada - 1.96 * df_c$Error_Estandar_Acum
    df_c$ic_high <- df_c$Estimacion_Acumulada + 1.96 * df_c$Error_Estandar_Acum

    p <- plot_ly(data = df_c, x = ~Simulacion) %>%
      add_ribbons(
        ymin = ~ic_low,
        ymax = ~ic_high,
        name = "Banda IC 95%",
        fillcolor = "rgba(229, 174, 50, 0.22)",
        line = list(color = "rgba(229, 174, 50, 0.45)", width = 1),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        y = ~Estimacion_Acumulada,
        name = "Estimación acumulada I_n",
        line = list(color = "#0F1F38", width = 2.4),
        hovertemplate = "Simulación n = %{x}<br>Estimación = %{y:.6f}<extra></extra>"
      )

    if (!is.na(res$valor_ref)) {
      p <- p %>%
        add_lines(
          x = c(1, nrow(pts)),
          y = c(res$valor_ref, res$valor_ref),
          name = "Valor de referencia",
          line = list(color = "#D31929", width = 2.5, dash = "dash"),
          hovertemplate = "Referencia exacta = %{y:.6f}<extra></extra>"
        )
    }

    p %>%
      layout(
        xaxis = list(title = "Número de simulaciones (n)"),
        yaxis = list(title = "Estimación de la integral"),
        legend = list(orientation = "h", y = 1.13, x = 0),
        margin = list(t = 32, r = 15, b = 45, l = 55),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$mc_summary_table <- renderDT({
    res <- req(mc_resultado())

    metricas <- data.frame(
      Indicador = c(
        "Estimación Montecarlo (Media Muestral)",
        "Estimación Geométrica (Éxito / Rechazo)",
        "Integración Numérica de Referencia",
        "Error Estándar Estimado (SE)",
        "Error Absoluto frente a Referencia",
        "Error Relativo (%)",
        "Intervalo de Confianza al 95%"
      ),
      Valor = c(
        sprintf("%.6f", res$valor_aprox),
        sprintf("%.6f", res$valor_geometrico),
        ifelse(is.na(res$valor_ref), "No disponible", sprintf("%.6f", res$valor_ref)),
        sprintf("%.6f", res$error_estandar),
        ifelse(is.na(res$err_abs), "No disponible", sprintf("%.6f", res$err_abs)),
        ifelse(is.na(res$err_rel_pct), "N/A", sprintf("%.4f %%", res$err_rel_pct)),
        sprintf("[ %.6f , %.6f ]", res$ic_inf, res$ic_sup)
      ),
      stringsAsFactors = FALSE
    )

    datatable(
      metricas,
      rownames = FALSE,
      options = list(
        dom = "t",
        paging = FALSE,
        ordering = FALSE
      )
    )
  })

  output$dl_mc_xlsx <- downloadHandler(
    filename = function() {
      sprintf("Montecarlo_Simulacion_%s.xlsx", Sys.Date())
    },
    content = function(file) {
      res <- req(mc_resultado())
      write_xlsx(res$df_puntos, path = file)
    }
  )

  output$dl_mc_csv <- downloadHandler(
    filename = function() {
      sprintf("Montecarlo_Simulacion_%s.csv", Sys.Date())
    },
    content = function(file) {
      res <- req(mc_resultado())
      write.csv(res$df_puntos, file = file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )

  # ---------------------------------------------------------------------------
  # Pestaña 3: Variables aleatorias discretas
  # ---------------------------------------------------------------------------

  disc_trigger <- reactiveVal(1L)

  observeEvent(input$btn_run_disc, {
    disc_trigger(disc_trigger() + 1L)
  })

  observeEvent(input$preset_disc_ross, {
    updateSelectInput(session, "disc_type", selected = "custom")
    updateTextInput(session, "disc_custom_x", value = "1, 2, 3, 4")
    updateTextInput(session, "disc_custom_p", value = "0.30, 0.20, 0.35, 0.15")
  })

  observeEvent(input$preset_disc_dado, {
    updateSelectInput(session, "disc_type", selected = "custom")
    updateTextInput(session, "disc_custom_x", value = "1, 2, 3, 4, 5, 6")
    updateTextInput(session, "disc_custom_p", value = paste(rep("1/6", 6), collapse = ", "))
  })

  disc_resultado <- reactive({
    disc_trigger()
    tipo <- isolate(input$disc_type)
    params <- isolate(switch(
      tipo,
      binom = list(n = input$disc_binom_n, p = input$disc_binom_p),
      pois = list(lambda = input$disc_pois_lambda),
      geom = list(p = input$disc_geom_p),
      custom = list(x = input$disc_custom_x, p = input$disc_custom_p)
    ))
    N <- min(max(as.integer(isolate(req(input$disc_n_sim))), 10L), 20000L)
    semilla <- isolate(req(input$disc_seed))

    tryCatch(
      simular_discreta(tipo, params, N, semilla),
      error = function(e) {
        showNotification(paste("V.A. discreta:", e$message), type = "error", duration = 6)
        NULL
      }
    )
  })

  output$disc_kpi_cards <- renderUI({
    res <- req(disc_resultado())

    tercera <- if (!is.null(res$info$comp_con_orden)) {
      kpi_tarjeta(
        "Comparaciones Promedio",
        "Inversa ordenada",
        sprintf("%.3f", res$info$comp_con_orden),
        sprintf("Sin ordenar las probabilidades: %.3f comparaciones", res$info$comp_sin_orden)
      )
    } else {
      kpi_tarjeta(
        "Máx. Discrepancia",
        sprintf("N = %d", length(res$muestra)),
        sprintf("%.4f", max(abs(res$tabla$Diferencia))),
        "max |frecuencia relativa − P(X = x)|"
      )
    }

    div(
      class = "kpi-grid",
      kpi_tarjeta(
        "Media Empírica",
        sprintf("Teórica: %.4f", res$media_teo),
        sprintf("%.4f", res$media_emp),
        sprintf("Error absoluto = %.4f", abs(res$media_emp - res$media_teo))
      ),
      kpi_tarjeta(
        "Varianza Empírica",
        sprintf("Teórica: %.4f", res$var_teo),
        sprintf("%.4f", res$var_emp),
        sprintf("Error absoluto = %.4f", abs(res$var_emp - res$var_teo))
      ),
      tercera
    )
  })

  output$disc_method_banner <- renderUI({
    res <- req(disc_resultado())
    div(
      class = "cycle-banner status-ok",
      div(class = "cycle-banner-title", span(res$nombre), span(class = "surface-card-meta", "Algoritmo utilizado")),
      p(class = "cycle-banner-desc", res$metodo)
    )
  })

  output$disc_bar_plot <- renderPlotly({
    res <- req(disc_resultado())
    tab <- res$tabla
    tab <- tab[tab$Frecuencia > 0 | tab$Prob_Teorica > 1e-4, , drop = FALSE]
    etiquetas <- as.character(tab$x)

    plot_ly() %>%
      add_bars(
        x = etiquetas, y = tab$Frec_Relativa, name = "Frecuencia relativa (empírica)",
        marker = list(color = "rgba(15, 31, 56, 0.85)"),
        hovertemplate = "x = %{x}<br>Empírica = %{y:.4f}<extra></extra>"
      ) %>%
      add_bars(
        x = etiquetas, y = tab$Prob_Teorica, name = "P(X = x) (teórica)",
        marker = list(color = "rgba(211, 25, 41, 0.75)"),
        hovertemplate = "x = %{x}<br>Teórica = %{y:.4f}<extra></extra>"
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "Valor de X", type = "category", categoryorder = "array", categoryarray = etiquetas),
        yaxis = list(title = "Probabilidad", rangemode = "tozero"),
        legend = list(orientation = "h", y = 1.12, x = 0),
        margin = list(t = 30, r = 15, b = 45, l = 55),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$disc_freq_table <- renderDT({
    res <- req(disc_resultado())
    tab <- res$tabla
    tab <- tab[tab$Frecuencia > 0 | tab$Prob_Teorica > 1e-4, , drop = FALSE]
    names(tab) <- c("x", "Frecuencia absoluta", "Frecuencia relativa", "P(X = x) teórica", "Diferencia")

    datatable(
      tab,
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ valores",
          paginate = list(previous = "Anterior", `next` = "Siguiente")
        )
      )
    ) %>%
      formatRound(columns = 3:5, digits = 4)
  })

  datos_exportar_disc <- reactive({
    res <- req(disc_resultado())
    list(
      Muestra = data.frame(i = seq_along(res$muestra), U_i = res$U, X_i = res$muestra),
      Frecuencias = res$tabla,
      Resumen = data.frame(
        Distribucion = res$nombre,
        Media_empirica = res$media_emp, Media_teorica = res$media_teo,
        Varianza_empirica = res$var_emp, Varianza_teorica = res$var_teo
      )
    )
  })

  output$dl_disc_xlsx <- downloadHandler(
    filename = function() sprintf("VA_Discreta_%s_%s.xlsx", input$disc_type, Sys.Date()),
    content = function(file) write_xlsx(datos_exportar_disc(), path = file)
  )

  output$dl_disc_txt <- downloadHandler(
    filename = function() sprintf("VA_Discreta_%s_%s.txt", input$disc_type, Sys.Date()),
    content = function(file) {
      write.table(datos_exportar_disc()$Muestra, file = file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # ---------------------------------------------------------------------------
  # Pestaña 4: Variables aleatorias continuas
  # ---------------------------------------------------------------------------

  cont_trigger <- reactiveVal(1L)

  observeEvent(input$btn_run_cont, {
    cont_trigger(cont_trigger() + 1L)
  })

  observeEvent(input$preset_cont_beta, {
    updateSelectInput(session, "cont_type", selected = "custom")
    updateSelectInput(session, "cont_custom_method", selected = "ar")
    updateTextInput(session, "cont_custom_fx", value = "20 * x * (1 - x)^3")
    updateNumericInput(session, "cont_custom_a", value = 0)
    updateNumericInput(session, "cont_custom_b", value = 1)
  })

  observeEvent(input$preset_cont_tri, {
    updateSelectInput(session, "cont_type", selected = "custom")
    updateSelectInput(session, "cont_custom_method", selected = "inv")
    updateTextInput(session, "cont_custom_fx", value = "ifelse(x <= 3, (x - 2) / 2, (2 - x / 3) / 2)")
    updateNumericInput(session, "cont_custom_a", value = 2)
    updateNumericInput(session, "cont_custom_b", value = 6)
  })

  observeEvent(input$preset_cont_cuad, {
    updateSelectInput(session, "cont_type", selected = "custom")
    updateTextInput(session, "cont_custom_fx", value = "3 * x^2")
    updateNumericInput(session, "cont_custom_a", value = 0)
    updateNumericInput(session, "cont_custom_b", value = 1)
  })

  cont_resultado <- reactive({
    cont_trigger()
    tipo <- isolate(input$cont_type)
    params <- isolate(switch(
      tipo,
      unif = list(a = input$cont_unif_a, b = input$cont_unif_b),
      exp = list(lambda = input$cont_exp_rate),
      norm = list(mu = input$cont_norm_mu, sigma = input$cont_norm_sd),
      custom = list(fx = input$cont_custom_fx, a = input$cont_custom_a, b = input$cont_custom_b)
    ))
    N <- min(max(as.integer(isolate(req(input$cont_n_sim))), 20L), 50000L)
    semilla <- isolate(req(input$cont_seed))
    metodo <- isolate(input$cont_custom_method)

    res <- tryCatch(
      simular_continua(tipo, params, N, semilla, metodo),
      error = function(e) {
        showNotification(paste("V.A. continua:", e$message), type = "error", duration = 6)
        NULL
      }
    )
    if (!is.null(res) && !is.null(res$info$integral) && abs(res$info$integral - 1) > 1e-3) {
      showNotification(
        sprintf("f(x) integra %.4f en [a, b]; se normalizó para que sea una densidad.", res$info$integral),
        type = "warning", duration = 6
      )
    }
    res
  })

  output$cont_kpi_cards <- renderUI({
    res <- req(cont_resultado())

    tercera <- if (!is.null(res$info$tasa_teo)) {
      kpi_tarjeta(
        "Tasa de Aceptación",
        sprintf("Teórica: %.4f", res$info$tasa_teo),
        sprintf("%.4f", res$info$tasa_emp),
        if (!is.null(res$info$c)) sprintf("Constante c = %.4f (1/c = proporción aceptada)", res$info$c)
        else "Método polar: P(S ≤ 1) = π/4"
      )
    } else {
      kpi_tarjeta(
        "Rango de la Muestra",
        sprintf("N = %d", length(res$muestra)),
        sprintf("[%.3f, %.3f]", min(res$muestra), max(res$muestra)),
        sprintf("Desviación estándar empírica = %.4f", sd(res$muestra))
      )
    }

    div(
      class = "kpi-grid",
      kpi_tarjeta(
        "Media Empírica",
        sprintf("Teórica: %.4f", res$media_teo),
        sprintf("%.4f", res$media_emp),
        sprintf("Error absoluto = %.4f", abs(res$media_emp - res$media_teo))
      ),
      kpi_tarjeta(
        "Varianza Empírica",
        sprintf("Teórica: %.4f", res$var_teo),
        sprintf("%.4f", res$var_emp),
        sprintf("Error absoluto = %.4f", abs(res$var_emp - res$var_teo))
      ),
      tercera
    )
  })

  output$cont_method_banner <- renderUI({
    res <- req(cont_resultado())
    div(
      class = "cycle-banner status-ok",
      div(class = "cycle-banner-title", span(res$nombre), span(class = "surface-card-meta", "Algoritmo utilizado")),
      p(class = "cycle-banner-desc", res$metodo)
    )
  })

  output$cont_hist_plot <- renderPlotly({
    res <- req(cont_resultado())
    x <- res$muestra
    lim <- c(min(res$rango[1], min(x)), max(res$rango[2], max(x)))
    malla <- seq(lim[1], lim[2], length.out = 400)
    dens_emp <- density(x, from = lim[1], to = lim[2], n = 400)

    plot_ly() %>%
      add_histogram(
        x = x, histnorm = "probability density", name = "Histograma",
        nbinsx = max(15L, min(60L, as.integer(ceiling(sqrt(length(x)))))),
        marker = list(color = "rgba(15, 31, 56, 0.70)", line = list(color = "#FFFFFF", width = 1)),
        hovertemplate = "Intervalo: %{x}<br>Densidad: %{y:.4f}<extra></extra>"
      ) %>%
      add_lines(
        x = dens_emp$x, y = dens_emp$y, name = "Densidad empírica (kernel)",
        line = list(color = "#E5AE32", width = 2.5),
        hovertemplate = "x = %{x:.4f}<br>Empírica = %{y:.4f}<extra></extra>"
      ) %>%
      add_lines(
        x = malla, y = res$dens(malla), name = "Densidad teórica f(x)",
        line = list(color = "#D31929", width = 3, dash = "dash"),
        hovertemplate = "x = %{x:.4f}<br>f(x) = %{y:.4f}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "x"),
        yaxis = list(title = "Densidad", rangemode = "tozero"),
        legend = list(orientation = "h", y = 1.12, x = 0),
        margin = list(t = 30, r = 15, b = 45, l = 55),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$cont_cdf_plot <- renderPlotly({
    res <- req(cont_resultado())
    x <- sort(res$muestra)
    n <- length(x)
    idx <- if (n > 2000) unique(round(seq(1, n, length.out = 2000))) else seq_len(n)
    lim <- c(min(res$rango[1], x[1]), max(res$rango[2], x[n]))
    malla <- seq(lim[1], lim[2], length.out = 400)

    plot_ly() %>%
      add_lines(
        x = x[idx], y = idx / n, name = "CDF empírica F_n(x)", line = list(shape = "hv", color = "#0F1F38", width = 2.2),
        hovertemplate = "x = %{x:.4f}<br>F_n(x) = %{y:.4f}<extra></extra>"
      ) %>%
      add_lines(
        x = malla, y = res$cdf(malla), name = "CDF teórica F(x)", line = list(color = "#D31929", width = 2.5, dash = "dash"),
        hovertemplate = "x = %{x:.4f}<br>F(x) = %{y:.4f}<extra></extra>"
      ) %>%
      layout(
        xaxis = list(title = "x"),
        yaxis = list(title = "Probabilidad acumulada", range = c(-0.02, 1.02)),
        legend = list(orientation = "h", y = 1.12, x = 0),
        margin = list(t = 30, r = 15, b = 45, l = 55),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  datos_exportar_cont <- reactive({
    res <- req(cont_resultado())
    list(
      Muestra = data.frame(i = seq_along(res$muestra), X_i = res$muestra),
      Resumen = data.frame(
        Distribucion = res$nombre, Metodo = res$metodo,
        Media_empirica = res$media_emp, Media_teorica = res$media_teo,
        Varianza_empirica = res$var_emp, Varianza_teorica = res$var_teo
      )
    )
  })

  output$dl_cont_xlsx <- downloadHandler(
    filename = function() sprintf("VA_Continua_%s_%s.xlsx", input$cont_type, Sys.Date()),
    content = function(file) write_xlsx(datos_exportar_cont(), path = file)
  )

  output$dl_cont_txt <- downloadHandler(
    filename = function() sprintf("VA_Continua_%s_%s.txt", input$cont_type, Sys.Date()),
    content = function(file) {
      write.table(datos_exportar_cont()$Muestra, file = file, sep = "\t", row.names = FALSE, quote = FALSE)
    }
  )

  # ---------------------------------------------------------------------------
  # Pestaña 5: Bondad de ajuste y diagnóstico estocástico
  # ---------------------------------------------------------------------------

  gof_trigger <- reactiveVal(1L)

  observeEvent(input$btn_run_gof, {
    gof_trigger(gof_trigger() + 1L)
  })

  gof_datos_ext <- reactive({
    req(input$gof_file)
    tryCatch(
      leer_muestra_externa(input$gof_file$datapath, input$gof_file$name),
      error = function(e) {
        showNotification(paste("Archivo:", e$message), type = "error", duration = 6)
        NULL
      }
    )
  })

  output$gof_col_ui <- renderUI({
    df <- req(gof_datos_ext())
    tagList(
      selectInput("gof_col", "Columna a evaluar", choices = names(df)),
      div(class = "input-helper", sprintf("%d filas y %d columnas numéricas detectadas.", nrow(df), ncol(df)))
    )
  })

  gof_resultado <- reactive({
    gof_trigger()
    fuente <- isolate(input$gof_source)
    alpha <- as.numeric(isolate(input$gof_alpha))

    if (fuente == "internal") {
      res1 <- isolate(rng_resultado())
      x <- if (!is.null(res1)) res1$df$u_i else NULL
      origen <- "Números de la Pestaña 1"
    } else if (fuente == "cont") {
      res4 <- isolate(cont_resultado())
      x <- if (!is.null(res4)) res4$muestra else NULL
      origen <- if (!is.null(res4)) paste("Pestaña 4:", res4$nombre) else "Pestaña 4"
    } else {
      df <- isolate(gof_datos_ext())
      col <- isolate(input$gof_col)
      x <- if (!is.null(df) && !is.null(col) && col %in% names(df)) df[[col]] else NULL
      origen <- if (!is.null(col)) paste0("Archivo ", isolate(input$gof_file$name), " (columna ", col, ")") else "Archivo"
    }

    x <- x[is.finite(x)]
    if (length(x) < 10) {
      showNotification("Se necesitan al menos 10 datos válidos para las pruebas.", type = "error", duration = 6)
      return(NULL)
    }

    dist <- isolate(input$gof_dist)
    estimar <- isTRUE(isolate(input$gof_estimar))
    if (dist == "unif01") {
      F0 <- function(t) punif(t, 0, 1)
      dist_txt <- "Uniforme(0, 1)"
    } else if (dist == "unif") {
      a <- if (estimar) min(x) else isolate(input$gof_unif_a)
      b <- if (estimar) max(x) else isolate(input$gof_unif_b)
      F0 <- function(t) punif(t, a, b)
      dist_txt <- sprintf("Uniforme(%.4f, %.4f)", a, b)
    } else if (dist == "norm") {
      mu <- if (estimar) mean(x) else isolate(input$gof_norm_mu)
      sigma <- if (estimar) sd(x) else isolate(input$gof_norm_sd)
      F0 <- function(t) pnorm(t, mu, sigma)
      dist_txt <- sprintf("Normal(%.4f, %.4f)", mu, sigma)
    } else {
      lambda <- if (estimar) 1 / mean(x) else isolate(input$gof_exp_rate)
      F0 <- function(t) pexp(t, lambda)
      dist_txt <- sprintf("Exponencial(%.4f)", lambda)
    }

    tipo_rachas <- isolate(input$gof_runs_type)
    tryCatch({
      list(
        x = x,
        F0 = F0,
        alpha = alpha,
        origen = origen,
        dist_txt = dist_txt,
        tipo_rachas = tipo_rachas,
        ks = prueba_ks(x, F0, alpha),
        rachas = prueba_rachas(x, tipo_rachas, alpha)
      )
    }, error = function(e) {
      showNotification(paste("Pruebas:", e$message), type = "error", duration = 6)
      NULL
    })
  })

  output$gof_kpi_cards <- renderUI({
    res <- req(gof_resultado())
    div(
      class = "kpi-grid",
      kpi_tarjeta(
        "1. Kolmogorov-Smirnov (KS)",
        sprintf("p = %.4f", res$ks$p_valor),
        sprintf("D = %.4f", res$ks$estadistico),
        if (res$ks$rechaza) sprintf("Se rechaza H0: los datos no siguen %s", res$dist_txt)
        else sprintf("No se rechaza H0: compatible con %s", res$dist_txt),
        estado = if (res$ks$rechaza) "fail" else "pass"
      ),
      kpi_tarjeta(
        "2. Prueba de Rachas",
        sprintf("p = %.4f", res$rachas$p_valor),
        sprintf("Z = %.4f", res$rachas$estadistico),
        if (res$rachas$rechaza) "Se rechaza H0: hay evidencia de dependencia"
        else "No se rechaza H0: secuencia compatible con independencia",
        estado = if (res$rachas$rechaza) "fail" else "pass"
      ),
      kpi_tarjeta(
        "Muestra Evaluada",
        sprintf("alpha = %.2f", res$alpha),
        sprintf("n = %d", length(res$x)),
        res$origen
      )
    )
  })

  output$gof_banner <- renderUI({
    res <- req(gof_resultado())
    ambos_ok <- !res$ks$rechaza && !res$rachas$rechaza
    avisos <- character(0)
    if (res$ks$hay_empates) {
      avisos <- c(avisos, "La muestra tiene valores repetidos (por ejemplo, por un ciclo corto del generador); el valor p de KS es aproximado.")
    }
    div(
      class = paste("cycle-banner", if (ambos_ok) "status-ok" else "status-ciclo"),
      div(
        class = "cycle-banner-title",
        span(if (ambos_ok) "La muestra supera ambas pruebas" else "La muestra no supera al menos una prueba"),
        span(class = "surface-card-meta", "Conclusión")
      ),
      p(
        class = "cycle-banner-desc",
        sprintf(
          "Con un nivel de significancia α = %.2f: %s el ajuste a %s (KS) y %s la independencia de la secuencia (rachas). %s",
          res$alpha,
          if (res$ks$rechaza) "se rechaza" else "no se rechaza",
          res$dist_txt,
          if (res$rachas$rechaza) "se rechaza" else "no se rechaza",
          paste(avisos, collapse = " ")
        )
      )
    )
  })

  output$gof_table <- renderDT({
    res <- req(gof_resultado())
    tabla <- data.frame(
      Prueba = c("Kolmogorov-Smirnov", if (res$tipo_rachas == "mediana") "Rachas (sobre / bajo la mediana)" else "Rachas (ascendentes / descendentes)"),
      `Hipótesis nula H0` = c(
        sprintf("Los datos siguen %s", res$dist_txt),
        "La secuencia es aleatoria (independiente)"
      ),
      `Estadístico` = c(sprintf("D = %.5f", res$ks$estadistico), sprintf("Z = %.5f", res$rachas$estadistico)),
      `Valor crítico` = c(sprintf("%.5f (aprox.)", res$ks$critico), sprintf("±%.4f", res$rachas$critico)),
      `Valor p` = c(sprintf("%.5f", res$ks$p_valor), sprintf("%.5f", res$rachas$p_valor)),
      Detalle = c(
        sprintf("n = %d", res$ks$n),
        sprintf("R = %d, E[R] = %.2f, V(R) = %.2f", res$rachas$R, res$rachas$media_R, res$rachas$var_R)
      ),
      `Decisión` = c(
        if (res$ks$rechaza) "Se rechaza H0" else "No se rechaza H0",
        if (res$rachas$rechaza) "Se rechaza H0" else "No se rechaza H0"
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    datatable(tabla, rownames = FALSE, options = list(dom = "t", paging = FALSE, ordering = FALSE, scrollX = TRUE)) %>%
      formatStyle(
        "Decisión",
        color = styleEqual(c("Se rechaza H0", "No se rechaza H0"), c("#D31929", "#059669")),
        fontWeight = "bold"
      )
  })

  output$gof_ecdf_plot <- renderPlotly({
    res <- req(gof_resultado())
    x <- sort(res$x)
    n <- length(x)
    Fn <- seq_len(n) / n
    idx <- if (n > 2000) unique(round(seq(1, n, length.out = 2000))) else seq_len(n)
    malla <- seq(x[1], x[n], length.out = 400)
    d <- res$ks$critico
    x_max <- res$ks$x_max
    Fn_max <- mean(res$x <= x_max)

    plot_ly() %>%
      add_ribbons(
        x = x[idx], ymin = pmax(Fn[idx] - d, 0), ymax = pmin(Fn[idx] + d, 1),
        name = sprintf("Banda KS (α = %.2f)", res$alpha),
        fillcolor = "rgba(229, 174, 50, 0.22)", line = list(color = "rgba(229, 174, 50, 0.4)", shape = "hv"),
        hoverinfo = "skip"
      ) %>%
      add_lines(
        x = x[idx], y = Fn[idx], name = "F_n(x) empírica", line = list(shape = "hv", color = "#0F1F38", width = 2),
        hovertemplate = "x = %{x:.4f}<br>F_n(x) = %{y:.4f}<extra></extra>"
      ) %>%
      add_lines(
        x = malla, y = res$F0(malla), name = "F0(x) teórica", line = list(color = "#D31929", width = 2.5, dash = "dash"),
        hovertemplate = "x = %{x:.4f}<br>F0(x) = %{y:.4f}<extra></extra>"
      ) %>%
      add_segments(
        x = x_max, xend = x_max, y = res$F0(x_max), yend = Fn_max,
        name = sprintf("D = %.4f", res$ks$estadistico), line = list(color = "#059669", width = 3),
        hoverinfo = "name"
      ) %>%
      layout(
        title = list(text = "ECDF vs. distribución teórica", font = list(size = 13)),
        xaxis = list(title = "x"),
        yaxis = list(title = "Probabilidad acumulada", range = c(-0.02, 1.02)),
        legend = list(orientation = "h", y = -0.22, x = 0),
        margin = list(t = 40, r = 10, b = 40, l = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })

  output$gof_runs_plot <- renderPlotly({
    res <- req(gof_resultado())
    x <- res$x
    n_max <- min(length(x), 300L)
    i <- seq_len(n_max)
    grupo <- res$rachas$signos[i]
    colores <- c(
      "Sobre la mediana" = "#0F1F38", "Bajo la mediana" = "#D31929", "Igual a la mediana" = "#94A3B8",
      "Sube" = "#0F1F38", "Baja" = "#D31929", "Igual" = "#94A3B8", "Inicio" = "#E5AE32"
    )

    p <- plot_ly() %>%
      add_lines(
        x = i, y = x[i], name = "Secuencia", line = list(color = "rgba(100, 116, 139, 0.45)", width = 1),
        hoverinfo = "skip"
      )
    for (g in unique(grupo)) {
      sel <- grupo == g
      p <- p %>% add_markers(
        x = i[sel], y = x[i][sel], name = g, marker = list(color = colores[[g]], size = 6),
        hovertemplate = "i = %{x}<br>x_i = %{y:.4f}<extra></extra>"
      )
    }
    if (!is.na(res$rachas$referencia)) {
      p <- p %>% add_lines(
        x = c(1, n_max), y = rep(res$rachas$referencia, 2), name = "Mediana",
        line = list(color = "#E5AE32", width = 2.5, dash = "dash"), hoverinfo = "name"
      )
    }

    p %>%
      layout(
        title = list(
          text = sprintf("Secuencia de rachas (R = %d%s)", res$rachas$R, if (length(x) > n_max) sprintf(", primeros %d datos", n_max) else ""),
          font = list(size = 13)
        ),
        xaxis = list(title = "Índice i"),
        yaxis = list(title = "x_i"),
        legend = list(orientation = "h", y = -0.22, x = 0),
        margin = list(t = 40, r = 10, b = 40, l = 50),
        paper_bgcolor = "rgba(0,0,0,0)",
        plot_bgcolor = "#F8FAFC"
      ) %>%
      config(displaylogo = FALSE)
  })
}

shinyApp(ui = ui, server = server)
