# Pestaña 4: Generación de variables aleatorias continuas
# Algoritmos basados en Ross, "Simulación", Capítulo 5, y en los códigos de clase.
# Todos reciben los uniformes desde el flujo MCL propio (no se usa runif).

# Normal estándar por el método polar: se generan V1, V2 ~ U(-1, 1) y se
# acepta el par si S = V1^2 + V2^2 <= 1; entonces
# X = V1 * sqrt(-2 log(S) / S) y Y = V2 * sqrt(-2 log(S) / S) son N(0, 1).
normal_polar <- function(N, flujo) {
  res <- numeric(0)
  intentos <- 0
  pares_aceptados <- 0
  while (length(res) < N) {
    k <- ceiling((N - length(res)) / 2 / (pi / 4)) + 5
    u <- flujo(2 * k)
    V1 <- 2 * u[1:k] - 1
    V2 <- 2 * u[(k + 1):(2 * k)] - 1
    S <- V1^2 + V2^2
    ok <- S > 0 & S <= 1
    factor <- sqrt(-2 * log(S[ok]) / S[ok])
    res <- c(res, V1[ok] * factor, V2[ok] * factor)
    intentos <- intentos + k
    pares_aceptados <- pares_aceptados + sum(ok)
  }
  list(x = res[1:N], intentos = intentos, tasa = pares_aceptados / intentos)
}

# Densidad personalizada sobre [a, b]: se valida, se normaliza y se tabula su
# función de distribución en una malla fina (regla del trapecio).
preparar_densidad <- function(expr_texto, a, b) {
  if (is.na(a) || is.na(b) || !is.finite(a) || !is.finite(b) || a >= b) {
    stop("El soporte debe cumplir a < b, con a y b finitos.")
  }
  f_usuario <- construir_funcion_segura(expr_texto)
  x_malla <- seq(a, b, length.out = 2001L)
  y_malla <- tryCatch(f_usuario(x_malla), error = function(e) {
    stop(paste("No se pudo evaluar f(x):", e$message))
  })
  if (any(!is.finite(y_malla))) stop("f(x) toma valores no finitos en [a, b].")
  if (any(y_malla < -1e-10)) stop("f(x) toma valores negativos en [a, b]: no es una densidad.")
  y_malla <- pmax(y_malla, 0)

  dx <- diff(x_malla)
  F_malla <- c(0, cumsum(dx * (y_malla[-1] + y_malla[-length(y_malla)]) / 2))
  integral <- F_malla[length(F_malla)]
  if (integral <= 0) stop("La integral de f(x) en [a, b] es cero.")

  f_norm <- function(x) {
    ifelse(x < a | x > b, 0, pmax(f_usuario(x), 0) / integral)
  }

  list(
    f = f_norm,
    f_usuario = f_usuario,
    x_malla = x_malla,
    y_malla = y_malla / integral,
    F_malla = F_malla / integral,
    integral = integral
  )
}

# Aceptación y rechazo con envolvente uniforme g(x) = 1 / (b - a) en [a, b].
# c = max f(x) / g(x) se obtiene con optimize (como en clase) y se compara con
# el máximo sobre la malla, para no perder máximos en densidades multimodales.
aceptacion_rechazo_uniforme <- function(dens, a, b, N, flujo) {
  g <- 1 / (b - a)
  c_opt <- optimize(function(x) dens$f(x) / g, interval = c(a, b), maximum = TRUE)$objective
  c_cte <- max(c_opt, max(dens$y_malla) / g)

  res <- numeric(0)
  intentos <- 0
  while (length(res) < N) {
    k <- ceiling((N - length(res)) * c_cte * 1.1) + 10
    Y <- a + (b - a) * flujo(k)
    U2 <- flujo(k)
    acepta <- U2 <= dens$f(Y) / (c_cte * g)
    res <- c(res, Y[acepta])
    intentos <- intentos + k
    if (intentos > 200 * N) stop("Demasiados rechazos: revise la densidad ingresada.")
  }
  list(x = res[1:N], c = c_cte, intentos = intentos, tasa = length(res) / intentos)
}

# Transformada inversa numérica: X = F^{-1}(U), invirtiendo por interpolación
# lineal la función de distribución tabulada en la malla.
inversa_numerica <- function(dens, N, flujo) {
  U <- flujo(N)
  x <- approx(x = dens$F_malla, y = dens$x_malla, xout = U, ties = mean)$y
  list(x = x)
}

simular_continua <- function(tipo, params, N, semilla = 2026, metodo_custom = "ar") {
  N <- as.integer(N)
  flujo <- crear_flujo_uniforme(semilla)
  info <- list()

  if (tipo == "unif") {
    a <- as.numeric(params$a)
    b <- as.numeric(params$b)
    if (is.na(a) || is.na(b) || a >= b) stop("Para la uniforme se requiere a < b.")
    x <- a + (b - a) * flujo(N)
    dens <- function(t) dunif(t, a, b)
    cdf <- function(t) punif(t, a, b)
    media_teo <- (a + b) / 2
    var_teo <- (b - a)^2 / 12
    rango <- c(a, b)
    nombre <- sprintf("Uniforme(a = %g, b = %g)", a, b)
    metodo <- "Transformación lineal: X = a + (b − a)·U."

  } else if (tipo == "exp") {
    lambda <- as.numeric(params$lambda)
    if (is.na(lambda) || lambda <= 0) stop("La tasa lambda debe ser positiva.")
    x <- -log(flujo(N)) / lambda
    dens <- function(t) dexp(t, lambda)
    cdf <- function(t) pexp(t, lambda)
    media_teo <- 1 / lambda
    var_teo <- 1 / lambda^2
    rango <- c(0, max(x))
    nombre <- sprintf("Exponencial(lambda = %g)", lambda)
    metodo <- "Transformada inversa: X = −log(U) / λ."

  } else if (tipo == "norm") {
    mu <- as.numeric(params$mu)
    sigma <- as.numeric(params$sigma)
    if (is.na(mu) || is.na(sigma) || sigma <= 0) stop("Se requiere sigma > 0.")
    polar <- normal_polar(N, flujo)
    x <- mu + sigma * polar$x
    dens <- function(t) dnorm(t, mu, sigma)
    cdf <- function(t) pnorm(t, mu, sigma)
    media_teo <- mu
    var_teo <- sigma^2
    rango <- range(x)
    nombre <- sprintf("Normal(mu = %g, sigma = %g)", mu, sigma)
    metodo <- "Método polar: (V1, V2) uniformes en el cuadrado [−1, 1]², se acepta si S = V1² + V2² ≤ 1 y X = μ + σ·V1·√(−2 log S / S)."
    info$tasa_teo <- pi / 4
    info$tasa_emp <- polar$tasa

  } else {
    a <- as.numeric(params$a)
    b <- as.numeric(params$b)
    d <- preparar_densidad(params$fx, a, b)
    if (metodo_custom == "ar") {
      ar <- aceptacion_rechazo_uniforme(d, a, b, N, flujo)
      x <- ar$x
      metodo <- sprintf("Aceptación y rechazo con envolvente uniforme en [%g, %g]: se acepta Y si U ≤ f(Y) / (c·g(Y)), con c = %.4f.", a, b, ar$c)
      info$c <- ar$c
      info$tasa_teo <- 1 / ar$c
      info$tasa_emp <- ar$tasa
    } else {
      x <- inversa_numerica(d, N, flujo)$x
      metodo <- "Transformada inversa numérica: F(x) se calcula integrando f por la regla del trapecio y X = F⁻¹(U) se obtiene por interpolación."
    }
    dens <- d$f
    cdf <- function(t) approx(d$x_malla, d$F_malla, xout = t, yleft = 0, yright = 1)$y
    dx <- diff(d$x_malla)
    trap <- function(v) sum(dx * (v[-1] + v[-length(v)]) / 2)
    media_teo <- trap(d$x_malla * d$y_malla)
    var_teo <- trap(d$x_malla^2 * d$y_malla) - media_teo^2
    rango <- c(a, b)
    nombre <- sprintf("Densidad personalizada f(x) = %s en [%g, %g]", params$fx, a, b)
    info$integral <- d$integral
  }

  list(
    muestra = x,
    dens = dens,
    cdf = cdf,
    rango = rango,
    nombre = nombre,
    metodo = metodo,
    media_emp = mean(x),
    var_emp = var(x),
    media_teo = media_teo,
    var_teo = var_teo,
    info = info
  )
}
