# Pestaña 3: Generación de variables aleatorias discretas
# Algoritmos basados en Ross, "Simulación", Capítulo 4, y en los códigos de clase.
# Todos reciben los uniformes desde el flujo MCL propio (no se usa runif).

# Binomial(n, p) por transformada inversa con la recursión
# P(X = i + 1) = [p / (1 - p)] * [(n - i) / (i + 1)] * P(X = i).
binomial_inversa <- function(n, p, U) {
  if (p > 0.5) {
    # Si X ~ Bin(n, 1 - p), entonces n - X ~ Bin(n, p). Evita que (1 - p)^n
    # sea demasiado pequeño y mejora la eficiencia.
    return(n - binomial_inversa(n, 1 - p, U))
  }
  c_rec <- p / (1 - p)
  pr <- (1 - p)^n
  Fx <- pr
  i <- 0
  while (U > Fx && i < n) {
    pr <- c_rec * (n - i) / (i + 1) * pr
    Fx <- Fx + pr
    i <- i + 1
  }
  i
}

# Poisson(lambda) por transformada inversa con la recursión
# P(X = i + 1) = lambda / (i + 1) * P(X = i).
poisson_inversa <- function(lambda, U) {
  p <- exp(-lambda)
  Fx <- p
  i <- 0
  while (U >= Fx) {
    p <- lambda * p / (i + 1)
    Fx <- Fx + p
    i <- i + 1
  }
  i
}

# Geométrica(p) (número de ensayos hasta el primer éxito):
# X = floor(log(U) / log(1 - p)) + 1.
geometrica_inversa <- function(p, U) {
  floor(log(U) / log(1 - p)) + 1
}

# Lee una lista de números escrita por el usuario ("1, 2, 3" o "1; 2; 3").
# Acepta fracciones como "1/6".
leer_vector_texto <- function(texto) {
  partes <- trimws(unlist(strsplit(as.character(texto), "[,;[:space:]]+")))
  partes <- partes[nzchar(partes)]
  valores <- sapply(partes, function(s) {
    if (grepl("^-?[0-9.]+/[0-9.]+$", s)) {
      fr <- as.numeric(strsplit(s, "/")[[1]])
      fr[1] / fr[2]
    } else {
      suppressWarnings(as.numeric(s))
    }
  }, USE.NAMES = FALSE)
  if (length(valores) == 0 || any(is.na(valores))) {
    stop("Revise los valores ingresados: deben ser números separados por comas.")
  }
  valores
}

# Distribución discreta arbitraria por transformada inversa discreta.
# Las probabilidades se ordenan de mayor a menor (como en clase) para reducir
# el número esperado de comparaciones.
validar_ley_discreta <- function(x, p) {
  if (length(x) != length(p)) {
    stop(sprintf("Se ingresaron %d valores y %d probabilidades: deben coincidir.", length(x), length(p)))
  }
  if (any(duplicated(x))) stop("Los valores del soporte no deben repetirse.")
  if (any(p < 0)) stop("Las probabilidades no pueden ser negativas.")
  if (abs(sum(p) - 1) > 1e-6) {
    stop(sprintf("Las probabilidades deben sumar 1 (suma actual = %.6f).", sum(p)))
  }
  invisible(TRUE)
}

simular_discreta <- function(tipo, params, N, semilla = 2026) {
  N <- as.integer(N)
  flujo <- crear_flujo_uniforme(semilla)
  U <- flujo(N)
  info <- list()

  if (tipo == "binom") {
    n <- as.integer(params$n)
    p <- as.numeric(params$p)
    if (is.na(n) || n < 1 || n > 1000) stop("El número de ensayos n debe estar entre 1 y 1000.")
    if (is.na(p) || p <= 0 || p >= 1) stop("La probabilidad p debe estar en (0, 1).")
    muestra <- sapply(U, function(u) binomial_inversa(n, p, u))
    soporte <- 0:n
    p_teo <- dbinom(soporte, n, p)
    media_teo <- n * p
    var_teo <- n * p * (1 - p)
    nombre <- sprintf("Binomial(n = %d, p = %.3f)", n, p)
    metodo <- "Transformada inversa con la recursión P(X = i+1) = [p/(1-p)]·[(n-i)/(i+1)]·P(X = i). Si p > 0.5 se genera n − Bin(n, 1−p)."

  } else if (tipo == "pois") {
    lambda <- as.numeric(params$lambda)
    if (is.na(lambda) || lambda <= 0 || lambda > 100) stop("La tasa lambda debe estar en (0, 100].")
    muestra <- sapply(U, function(u) poisson_inversa(lambda, u))
    soporte <- 0:max(max(muestra), qpois(0.9999, lambda))
    p_teo <- dpois(soporte, lambda)
    media_teo <- lambda
    var_teo <- lambda
    nombre <- sprintf("Poisson(lambda = %.3f)", lambda)
    metodo <- "Transformada inversa con la recursión P(X = i+1) = λ/(i+1)·P(X = i), partiendo de P(X = 0) = e^(−λ)."

  } else if (tipo == "geom") {
    p <- as.numeric(params$p)
    if (is.na(p) || p <= 0 || p >= 1) stop("La probabilidad p debe estar en (0, 1).")
    muestra <- sapply(U, function(u) geometrica_inversa(p, u))
    soporte <- 1:max(max(muestra), qgeom(0.9999, p) + 1)
    p_teo <- dgeom(soporte - 1, p)
    media_teo <- 1 / p
    var_teo <- (1 - p) / p^2
    nombre <- sprintf("Geométrica(p = %.3f), X = número de ensayos", p)
    metodo <- "Transformada inversa en forma cerrada: X = ⌊log(U) / log(1 − p)⌋ + 1."

  } else {
    x <- leer_vector_texto(params$x)
    p <- leer_vector_texto(params$p)
    validar_ley_discreta(x, p)

    orden <- order(p, decreasing = TRUE)
    x_ord <- x[orden]
    acum <- cumsum(p[orden])
    acum[length(acum)] <- 1
    muestra <- sapply(U, function(u) x_ord[which(u < acum)[1]])

    soporte <- sort(x)
    p_teo <- p[match(soporte, x)]
    media_teo <- sum(x * p)
    var_teo <- sum(x^2 * p) - media_teo^2
    nombre <- sprintf("Ley personalizada con %d valores", length(x))
    metodo <- "Transformada inversa discreta: X = x_j si F(x_{j−1}) ≤ U < F(x_j), con las probabilidades ordenadas de mayor a menor."
    info$comp_sin_orden <- sum(seq_along(p) * p)
    info$comp_con_orden <- sum(seq_along(p) * p[orden])
  }

  frec_abs <- sapply(soporte, function(v) sum(muestra == v))
  tabla <- data.frame(
    x = soporte,
    Frecuencia = frec_abs,
    Frec_Relativa = frec_abs / N,
    Prob_Teorica = p_teo,
    Diferencia = frec_abs / N - p_teo
  )

  list(
    muestra = muestra,
    U = U,
    tabla = tabla,
    nombre = nombre,
    metodo = metodo,
    media_emp = mean(muestra),
    var_emp = var(muestra),
    media_teo = media_teo,
    var_teo = var_teo,
    info = info
  )
}
