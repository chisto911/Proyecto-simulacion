# Pestaña 5: Pruebas de bondad de ajuste y diagnóstico estocástico

# Prueba de Kolmogorov-Smirnov.
# D = max(D+, D-), con D+ = max(i/n - F0(x_(i))) y D- = max(F0(x_(i)) - (i-1)/n).
# El estadístico se calcula directamente; el valor p se obtiene con ks.test.
prueba_ks <- function(x, F0, alpha = 0.05) {
  x <- sort(x)
  n <- length(x)
  Fx <- F0(x)
  d_mas <- seq_len(n) / n - Fx
  d_menos <- Fx - (seq_len(n) - 1) / n
  D <- max(c(d_mas, d_menos))
  pos <- if (max(d_mas) >= max(d_menos)) which.max(d_mas) else which.max(d_menos)

  p_valor <- suppressWarnings(ks.test(x, F0)$p.value)
  d_critico <- sqrt(-0.5 * log(alpha / 2)) / sqrt(n)   # aproximación asintótica

  list(
    estadistico = D,
    p_valor = p_valor,
    critico = d_critico,
    rechaza = p_valor < alpha,
    x_max = x[pos],
    hay_empates = any(duplicated(x)),
    n = n
  )
}

# Prueba de rachas.
# "mediana": rachas por encima / por debajo de la mediana (Wald-Wolfowitz).
#   E[R] = 2 n1 n2 / n + 1,  V(R) = 2 n1 n2 (2 n1 n2 - n) / (n^2 (n - 1)).
# "ascendente": rachas ascendentes y descendentes.
#   E[R] = (2n - 1) / 3,     V(R) = (16n - 29) / 90.
prueba_rachas <- function(x, tipo = "mediana", alpha = 0.05) {
  n_total <- length(x)

  if (tipo == "mediana") {
    med <- median(x)
    s <- x[x != med] > med
    n1 <- sum(s)
    n2 <- sum(!s)
    n <- n1 + n2
    if (n1 == 0 || n2 == 0) stop("Todos los valores quedan del mismo lado de la mediana.")
    R <- 1 + sum(diff(s) != 0)
    media_R <- 2 * n1 * n2 / n + 1
    var_R <- 2 * n1 * n2 * (2 * n1 * n2 - n) / (n^2 * (n - 1))
    signos <- ifelse(x > med, "Sobre la mediana", ifelse(x < med, "Bajo la mediana", "Igual a la mediana"))
    referencia <- med
  } else {
    d <- diff(x)
    s <- d[d != 0] > 0
    if (length(s) < 2) stop("La secuencia no tiene suficientes cambios para contar rachas.")
    R <- 1 + sum(diff(s) != 0)
    media_R <- (2 * n_total - 1) / 3
    var_R <- (16 * n_total - 29) / 90
    signos <- c("Inicio", ifelse(d > 0, "Sube", ifelse(d < 0, "Baja", "Igual")))
    referencia <- NA_real_
  }

  Z <- (R - media_R) / sqrt(var_R)
  p_valor <- 2 * (1 - pnorm(abs(Z)))

  list(
    R = R,
    media_R = media_R,
    var_R = var_R,
    estadistico = Z,
    p_valor = p_valor,
    critico = qnorm(1 - alpha / 2),
    rechaza = p_valor < alpha,
    signos = signos,
    referencia = referencia
  )
}

# Lectura de una muestra externa (.csv, .txt o .xlsx).
leer_muestra_externa <- function(ruta, nombre) {
  ext <- tolower(tools::file_ext(nombre))
  if (ext %in% c("xlsx", "xls")) {
    df <- as.data.frame(readxl::read_excel(ruta))
  } else {
    linea <- readLines(ruta, n = 1, warn = FALSE)
    sep <- if (grepl(";", linea)) ";" else if (grepl("\t", linea)) "\t" else if (grepl(",", linea)) "," else ""
    primero <- trimws(if (sep == "") strsplit(trimws(linea), "[[:space:]]+")[[1]][1] else strsplit(linea, sep)[[1]][1])
    encabezado <- is.na(suppressWarnings(as.numeric(gsub(",", ".", primero))))
    df <- read.table(ruta, sep = sep, header = encabezado, dec = if (sep == ";") "," else ".",
                     stringsAsFactors = FALSE, check.names = FALSE)
  }
  # Columnas con coma decimal que se leyeron como texto
  for (col in names(df)) {
    if (is.character(df[[col]])) {
      conv <- suppressWarnings(as.numeric(gsub(",", ".", df[[col]])))
      if (all(!is.na(conv[!is.na(df[[col]]) & nzchar(df[[col]])]))) df[[col]] <- conv
    }
  }
  numericas <- names(df)[sapply(df, is.numeric)]
  if (length(numericas) == 0) stop("El archivo no tiene columnas numéricas.")
  df[, numericas, drop = FALSE]
}
