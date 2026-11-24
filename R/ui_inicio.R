# Pestaña de inicio: portada, acceso a los módulos y guía de uso.

integrantes <- c("Christopher Almeida", "Fernando Naula")

# Ilustración de la portada (SVG): histograma, densidad normal, densidad
# uniforme y puntos de un experimento de Montecarlo. Los puntos se generan con
# el mismo generador MCL del aplicativo.
ilustracion_portada <- function() {
  ancho <- 440
  alto <- 300
  base <- 250

  # Barras del histograma (alturas proporcionales a la densidad normal)
  centros <- seq(-2.75, 2.75, by = 0.5)
  alturas <- dnorm(centros) / dnorm(0) * 165
  x_bar <- 40 + (centros + 3) / 6 * 360
  barras <- paste(sprintf(
    "<rect x='%.1f' y='%.1f' width='26' height='%.1f' rx='3' fill='rgba(255,255,255,0.16)' stroke='rgba(255,255,255,0.35)' stroke-width='1'/>",
    x_bar - 13, base - alturas, alturas
  ), collapse = "")

  # Curva de densidad normal
  t <- seq(-3, 3, length.out = 120)
  px <- 40 + (t + 3) / 6 * 360
  py <- base - dnorm(t) / dnorm(0) * 175
  curva <- sprintf(
    "<path d='M %s' fill='none' stroke='#E5AE32' stroke-width='4' stroke-linecap='round'/>",
    paste(sprintf("%.1f %.1f", px, py), collapse = " L ")
  )

  # Puntos de Montecarlo
  flujo <- crear_flujo_uniforme(2026)
  u <- flujo(160)
  xs <- 40 + u[1:80] * 360
  ys <- 70 + u[81:160] * 180
  dentro <- ys >= base - dnorm((xs - 40) / 360 * 6 - 3) / dnorm(0) * 175
  puntos <- paste(sprintf(
    "<circle cx='%.1f' cy='%.1f' r='3.2' fill='%s'/>",
    xs, ys, ifelse(dentro, "rgba(211,25,41,0.9)", "rgba(255,255,255,0.45)")
  ), collapse = "")

  HTML(sprintf(
    "<svg viewBox='0 0 %d %d' class='hero-svg' role='img' aria-label='Histograma, densidad normal y puntos de Montecarlo'>
       <line x1='30' y1='%d' x2='420' y2='%d' stroke='rgba(255,255,255,0.55)' stroke-width='2'/>
       <line x1='40' y1='60' x2='40' y2='%d' stroke='rgba(255,255,255,0.25)' stroke-width='1.5'/>
       %s
       <line x1='40' y1='130' x2='400' y2='130' stroke='rgba(211,25,41,0.85)' stroke-width='2.5' stroke-dasharray='8 6'/>
       <text x='345' y='122' fill='rgba(255,255,255,0.85)' font-size='13' font-family='JetBrains Mono, monospace'>U(0,1)</text>
       %s
       %s
       <text x='300' y='52' fill='#E5AE32' font-size='14' font-family='JetBrains Mono, monospace'>f(x) = &#966;(x)</text>
     </svg>",
    ancho, alto, base, base, base, barras, curva, puntos
  ))
}

tarjeta_modulo <- function(numero, icono, titulo, descripcion, etiquetas, boton_id) {
  div(
    class = "home-module-card",
    div(
      class = "home-module-top",
      div(class = "home-module-icon", icon(icono)),
      span(class = "home-module-num", sprintf("Pestaña %d", numero))
    ),
    h3(class = "home-module-title", titulo),
    p(class = "home-module-desc", descripcion),
    div(class = "home-tags", lapply(etiquetas, function(e) span(class = "home-tag", e))),
    actionButton(boton_id, "Abrir módulo", icon = icon("arrow-right"), class = "btn btn-secondary-sim home-module-btn")
  )
}

paso_guia <- function(numero, icono, titulo, texto) {
  div(
    class = "home-step",
    div(class = "home-step-num", numero),
    div(
      div(class = "home-step-title", icon(icono), span(titulo)),
      p(class = "home-step-text", texto)
    )
  )
}

seccion_guia <- function(que_hace, pasos, lectura, sugerencia) {
  tagList(
    p(class = "guide-what", que_hace),
    div(
      class = "guide-cols",
      div(
        h4(class = "guide-subtitle", icon("list-ol"), "Cómo usarlo"),
        tags$ol(class = "guide-list", lapply(pasos, tags$li))
      ),
      div(
        h4(class = "guide-subtitle", icon("magnifying-glass-chart"), "Cómo leer los resultados"),
        tags$ul(class = "guide-list", lapply(lectura, tags$li))
      )
    ),
    div(class = "guide-tip", icon("lightbulb"), span(sugerencia))
  )
}

pestana_inicio <- function() {
  tabPanel(
    title = "Inicio",
    value = "tab_home",
    icon = icon("house"),
    div(
      class = "module-shell",

      # Portada ---------------------------------------------------------------
      div(
        class = "hero-epn",
        div(
          class = "hero-text",
          span(class = "hero-kicker", "Trabajo Grupal · Inicio"),
          h1(class = "hero-title", "Módulos y Guía de Uso"),
          p(
            class = "hero-desc",
            "Herramienta interactiva para generar números pseudoaleatorios, aproximar integrales por Montecarlo, ",
            "simular variables aleatorias discretas y continuas, y evaluar la calidad de las muestras con pruebas de ",
            "bondad de ajuste."
          ),
          div(
            class = "hero-chips",
            span(class = "hero-chip", icon("layer-group"), "5 módulos integrados"),
            span(class = "hero-chip", icon("code"), "Algoritmos propios, sin runif"),
            span(class = "hero-chip", icon("file-export"), "Exportación .xlsx · .csv · .txt")
          ),
          div(
            class = "hero-actions",
            actionButton("go_rng_hero", "Comenzar", icon = icon("play"), class = "btn btn-primary-sim"),
            tags$a(href = "#guia-uso", class = "btn hero-btn-ghost", icon("book-open"), "Ver guía de uso")
          )
        ),
        div(class = "hero-art", ilustracion_portada())
      ),

      # Módulos ---------------------------------------------------------------
      div(
        class = "home-section-head",
        h2(class = "home-section-title", "Módulos"),
        p(class = "home-section-sub", "Cada módulo corresponde a un tema del curso. Pulse «Abrir módulo» para ir directamente.")
      ),
      div(
        class = "home-module-grid",
        tarjeta_modulo(1, "dice", "Números Pseudoaleatorios",
                       "Genere secuencias U(0, 1) desde sus recurrencias y detecte ciclos, valores negativos y degeneración.",
                       c("MCL", "Lehmer", "Cuadrados medios"), "go_rng"),
        tarjeta_modulo(2, "chart-area", "Integración Montecarlo",
                       "Aproxime integrales definidas, observe los puntos en la región y la convergencia de la estimación.",
                       c("Media muestral", "Error estándar", "IC 95 %"), "go_mc"),
        tarjeta_modulo(3, "chart-column", "V.A. Discretas",
                       "Simule distribuciones clásicas o una ley propia y compare frecuencias empíricas con las teóricas.",
                       c("Binomial", "Poisson", "Geométrica", "Ley propia"), "go_disc"),
        tarjeta_modulo(4, "wave-square", "V.A. Continuas",
                       "Genere muestras continuas y contraste el histograma y la CDF con la densidad teórica.",
                       c("Inversa", "Método polar", "Aceptación y rechazo"), "go_cont"),
        tarjeta_modulo(5, "square-check", "Bondad de Ajuste",
                       "Evalúe si una muestra se ajusta a una distribución y si la secuencia es independiente.",
                       c("Kolmogorov-Smirnov", "Rachas", "Archivo externo"), "go_gof")
      ),

      # Guía de uso -----------------------------------------------------------
      div(
        id = "guia-uso",
        class = "home-section-head",
        h2(class = "home-section-title", "Guía de uso"),
        p(class = "home-section-sub", "Todos los módulos funcionan con el mismo flujo de tres pasos.")
      ),
      div(
        class = "home-steps",
        paso_guia(1, "sliders", "Configure",
                  "Elija el método o la distribución en el panel izquierdo e ingrese los parámetros. Los botones de ejemplo cargan casos del libro de Ross."),
        paso_guia(2, "play", "Ejecute",
                  "Pulse el botón rojo del panel. Los resultados solo se actualizan al pulsarlo, así puede cambiar varios parámetros antes de simular."),
        paso_guia(3, "file-export", "Analice y exporte",
                  "Revise las tarjetas de resultados, los gráficos interactivos (zoom y valores al pasar el cursor) y descargue los datos.")
      ),

      div(
        class = "surface-card guide-card",
        accordion(
          id = "guia_acordeon",
          open = FALSE,
          accordion_panel(
            "Pestaña 1 · Números pseudoaleatorios",
            icon = icon("dice"),
            seccion_guia(
              "Genera números U(0, 1) con el Método Congruencial Lineal, el método de Lehmer o el de los cuadrados medios, sin usar el generador de R.",
              c("Elija el algoritmo.", "Ingrese la semilla x0 y los parámetros del método (a, c, m o el número de cifras).",
                "Indique cuántos números generar (N) y pulse «Generar Secuencia».",
                "Descargue la tabla en .xlsx o .csv."),
              c("La media y la varianza deben acercarse a 0.5 y 1/12 ≈ 0.0833.",
                "El aviso superior indica si hubo ciclo, iterante negativo o degeneración a 0.",
                "En el histograma, las barras deben rodear la línea f(u) = 1.",
                "En el gráfico de rezagos, los puntos deben llenar el cuadrado sin patrones."),
              "Pruebe «Ciclo Corto (x0 = 3708)» para ver cómo falla el método de los cuadrados medios."
            )
          ),
          accordion_panel(
            "Pestaña 2 · Integración Montecarlo",
            icon = icon("chart-area"),
            seccion_guia(
              "Aproxima ∫ f(x) dx en [a, b] como (b − a) por el promedio de f en puntos uniformes.",
              c("Escriba f(x) con sintaxis de R, por ejemplo exp(-x^2) o sin(pi*x).",
                "Ingrese los límites a y b, el número de simulaciones N y la semilla.",
                "Elija la fuente de uniformes: el generador interno o la secuencia de la Pestaña 1.",
                "Pulse «Ejecutar Simulación Montecarlo»."),
              c("El valor aproximado se compara con la integración numérica de referencia.",
                "El error estándar decrece aproximadamente como 1/√N.",
                "En el gráfico de convergencia, la estimación se estabiliza y la banda del 95 % se estrecha."),
              "Si usa una secuencia corta de la Pestaña 1, los puntos se repiten y la estimación empeora: es una forma de ver la importancia del generador."
            )
          ),
          accordion_panel(
            "Pestaña 3 · Variables aleatorias discretas",
            icon = icon("chart-column"),
            seccion_guia(
              "Simula binomiales, Poisson y geométricas con la transformada inversa y sus recursiones, o una ley discreta ingresada por usted.",
              c("Elija la distribución e ingrese sus parámetros.",
                "Para una ley propia, escriba los valores y sus probabilidades separados por comas (se aceptan fracciones como 1/6). Deben sumar 1.",
                "Indique el tamaño de muestra N y la semilla, y pulse «Simular Variable Discreta».",
                "Descargue la muestra y la tabla de frecuencias en .xlsx o .txt."),
              c("Las barras empíricas (azul) deben parecerse a las teóricas (rojo).",
                "La media y la varianza empíricas se comparan con las teóricas.",
                "En una ley propia, la tarjeta de comparaciones muestra cuánto ahorra ordenar las probabilidades de mayor a menor."),
              "Aumente N y observe cómo disminuye la discrepancia máxima entre frecuencias y probabilidades."
            )
          ),
          accordion_panel(
            "Pestaña 4 · Variables aleatorias continuas",
            icon = icon("wave-square"),
            seccion_guia(
              "Simula uniformes, exponenciales y normales (método polar), o una densidad f(x) propia por aceptación y rechazo o transformada inversa numérica.",
              c("Elija la distribución e ingrese sus parámetros.",
                "Para una densidad propia, escriba f(x), el soporte [a, b] y el método. Si f no integra 1, se normaliza automáticamente.",
                "Pulse «Simular Variable Continua».",
                "Descargue la muestra en .xlsx o .txt."),
              c("El histograma y la densidad empírica (dorado) deben seguir a la densidad teórica (rojo).",
                "La CDF empírica escalonada debe superponerse a la teórica.",
                "En aceptación y rechazo, la proporción aceptada se acerca a 1/c; en el método polar, a π/4 ≈ 0.785."),
              "Compare los dos métodos con la misma densidad: dan la misma distribución, pero el rechazo descarta parte de los uniformes."
            )
          ),
          accordion_panel(
            "Pestaña 5 · Bondad de ajuste",
            icon = icon("square-check"),
            seccion_guia(
              "Contrasta si una muestra sigue una distribución (Kolmogorov-Smirnov) y si la secuencia es independiente (rachas).",
              c("Elija el origen de los datos: Pestaña 1, Pestaña 4 o un archivo .csv, .txt o .xlsx (y la columna a evaluar).",
                "Elija la distribución teórica F0 y, si corresponde, sus parámetros o la opción de estimarlos.",
                "Elija el tipo de prueba de rachas y el nivel de significancia α.",
                "Pulse «Ejecutar Pruebas Estadísticas»."),
              c("Si el valor p es menor que α, se rechaza H0 (la tarjeta se marca en rojo).",
                "En el gráfico ECDF, la curva teórica debe quedar dentro de la banda KS; la línea verde marca la distancia máxima D.",
                "En el gráfico de rachas, muy pocas o demasiadas rachas indican dependencia."),
              "Genere en la Pestaña 1 una secuencia con ciclo corto y evalúela aquí: las pruebas detectan que no es una buena secuencia aleatoria."
            )
          )
        )
      ),

      # Integración entre módulos ---------------------------------------------
      div(
        class = "home-section-head",
        h2(class = "home-section-title", "Cómo se conectan los módulos")
      ),
      div(
        class = "home-flow",
        div(class = "home-flow-node", icon("dice"), span("Pestaña 1"), tags$small("Genera uniformes")),
        div(class = "home-flow-arrow", icon("arrow-right"), tags$small("fuente de puntos")),
        div(class = "home-flow-node", icon("chart-area"), span("Pestaña 2"), tags$small("Integra f(x)")),
        div(class = "home-flow-sep"),
        div(class = "home-flow-node", icon("dice"), span("Pestaña 1"), tags$small("Secuencia u_i")),
        div(class = "home-flow-arrow", icon("arrow-right"), tags$small("se valida en")),
        div(class = "home-flow-node is-accent", icon("square-check"), span("Pestaña 5"), tags$small("KS y rachas")),
        div(class = "home-flow-arrow is-left", icon("arrow-left"), tags$small("se valida en")),
        div(class = "home-flow-node", icon("wave-square"), span("Pestaña 4"), tags$small("Muestra continua"))
      ),

      # Integrantes y referencias ---------------------------------------------
      div(
        class = "home-footer-grid",
        div(
          class = "surface-card",
          div(class = "surface-card-header", h3(class = "surface-card-title", icon("users"), " Integrantes"), span(class = "surface-card-meta", "Trabajo grupal")),
          tags$ul(class = "home-people", lapply(integrantes, function(n) tags$li(icon("user"), span(n))))
        ),
        div(
          class = "surface-card",
          div(class = "surface-card-header", h3(class = "surface-card-title", icon("book-open"), " Referencias"), span(class = "surface-card-meta", "Bibliografía")),
          tags$ul(
            class = "home-people",
            tags$li(icon("book"), span("Ross, S. M. ", tags$em("Simulación"), " (2.ª ed.). Prentice Hall.")),
            tags$li(icon("book"), span("Wickham, H. ", tags$em("Mastering Shiny"), ". mastering-shiny.org"))
          )
        )
      )
    )
  )
}
