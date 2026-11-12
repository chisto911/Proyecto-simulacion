secuencia_uniforme_mcl <- function(n, semilla = 98765) {
  n <- as.integer(n)
  u <- numeric(n)
  x <- as.numeric(floor(abs(semilla))) %% 4294967296
  if (x == 0) x <- 1234567
  a <- 1664525
  c_inc <- 1013904223
  m <- 4294967296

  for (i in seq_len(n)) {
    x <- (a * x + c_inc) %% m
    u[i] <- x / m
  }
  u
}

construir_funcion_segura <- function(expr_texto) {
  expr_limpia <- trimws(as.character(expr_texto))
  if (!nzchar(expr_limpia)) {
    stop("Ingrese una expresión matemática válida para f(x).")
  }

  expr_parsed <- tryCatch(
    parse(text = expr_limpia),
    error = function(e) stop("Error de sintaxis en f(x): revise paréntesis y operadores.")
  )

  function(x) {
    env_eval <- list2env(list(x = x), parent = baseenv())
    val <- eval(expr_parsed, envir = env_eval)
    if (!is.numeric(val)) {
      stop("La expresión f(x) no produjo valores numéricos.")
    }
    if (length(val) == 1L && length(x) > 1L) {
      val <- rep_len(val, length(x))
    }
    if (length(val) != length(x)) {
      val <- vapply(x, function(xi) {
        as.numeric(eval(expr_parsed, envir = list2env(list(x = xi), parent = baseenv())))[1]
      }, numeric(1))
    }
    as.numeric(val)
  }
}

simular_montecarlo <- function(expr_texto, a, b, N, semilla = 202611, u_externos = NULL) {
  a <- as.numeric(a)
  b <- as.numeric(b)
  N <- as.integer(N)

  if (is.na(a) || is.na(b) || !is.finite(a) || !is.finite(b)) {
    stop("Los límites de integración a y b deben ser números finitos.")
  }
  if (a >= b) {
    stop("El límite inferior (a) debe ser estrictamente menor que el límite superior (b).")
  }
  if (is.na(N) || N < 10L) {
    stop("El número de simulaciones N debe ser al menos 10.")
  }

  f_fun <- construir_funcion_segura(expr_texto)

  x_malla <- seq(a, b, length.out = 400L)
  y_malla <- tryCatch(
    f_fun(x_malla),
    error = function(e) stop(paste("No se pudo evaluar f(x) en [a, b]:", e$message))
  )

  if (any(!is.finite(y_malla))) {
    stop("La función f(x) presenta asíntotas, divisiones por cero o valores indefinidos en [a, b].")
  }

  if (!is.null(u_externos) && length(u_externos) >= 5L) {
    u_x <- rep_len(as.numeric(u_externos), N)
  } else {
    u_x <- secuencia_uniforme_mcl(N, semilla = semilla)
  }
  u_y <- secuencia_uniforme_mcl(N, semilla = semilla + 7919)

  x_sim <- a + (b - a) * u_x
  fx_sim <- f_fun(x_sim)

  if (any(!is.finite(fx_sim))) {
    stop("Se encontraron valores no finitos al evaluar f(x) en los puntos muestreados.")
  }

  ancho <- b - a
  contrib <- ancho * fx_sim
  n_seq <- seq_len(N)
  est_acum <- cumsum(contrib) / n_seq

  media_final <- mean(contrib)
  w <- contrib - media_final
  suma_w2 <- cumsum(w^2)
  media_w <- cumsum(w) / n_seq
  ss_acum <- pmax(0, suma_w2 - n_seq * (media_w^2))

  var_acum <- rep(0, N)
  if (N >= 2L) {
    var_acum[2:N] <- ss_acum[2:N] / (n_seq[2:N] - 1)
  }
  se_acum <- sqrt(var_acum / n_seq)

  valor_aprox <- est_acum[N]
  error_estandar <- se_acum[N]
  ic_inf <- valor_aprox - 1.96 * error_estandar
  ic_sup <- valor_aprox + 1.96 * error_estandar

  ref_obj <- tryCatch(
    stats::integrate(f_fun, lower = a, upper = b, subdivisions = 500L),
    error = function(e) NULL
  )
  valor_ref <- if (!is.null(ref_obj)) ref_obj$value else NA_real_
  err_abs <- if (!is.na(valor_ref)) abs(valor_aprox - valor_ref) else NA_real_
  err_rel_pct <- if (!is.na(valor_ref) && abs(valor_ref) > 1e-12) {
    100 * err_abs / abs(valor_ref)
  } else {
    NA_real_
  }

  y_piso <- min(0, min(y_malla), min(fx_sim))
  y_techo <- max(0, max(y_malla), max(fx_sim))
  margen <- 0.06 * (y_techo - y_piso)
  if (margen == 0) margen <- 1
  caja_min <- min(0, y_piso - margen)
  caja_max <- max(0, y_techo + margen)

  y_rand <- caja_min + (caja_max - caja_min) * u_y

  bajo_pos <- (fx_sim >= 0) & (y_rand >= 0) & (y_rand <= fx_sim)
  sobre_neg <- (fx_sim < 0) & (y_rand <= 0) & (y_rand >= fx_sim)

  region_tipo <- ifelse(
    bajo_pos, "Bajo f(x) (+)",
    ifelse(sobre_neg, "Región negativa (-)", "Fuera de la región")
  )

  area_caja <- ancho * (caja_max - caja_min)
  prop_neta <- (sum(bajo_pos) - sum(sobre_neg)) / N
  est_geometrica <- area_caja * prop_neta

  df_puntos <- data.frame(
    Simulacion = n_seq,
    x_i = x_sim,
    y_aleatorio = y_rand,
    f_xi = fx_sim,
    Estimacion_Acumulada = est_acum,
    Error_Estandar_Acum = se_acum,
    Clasificacion = region_tipo,
    stringsAsFactors = FALSE
  )

  df_curva <- data.frame(x = x_malla, y = y_malla)

  list(
    valor_aprox = valor_aprox,
    valor_geometrico = est_geometrica,
    error_estandar = error_estandar,
    ic_inf = ic_inf,
    ic_sup = ic_sup,
    valor_ref = valor_ref,
    err_abs = err_abs,
    err_rel_pct = err_rel_pct,
    prop_aciertos = (sum(bajo_pos) + sum(sobre_neg)) / N,
    caja_min = caja_min,
    caja_max = caja_max,
    df_puntos = df_puntos,
    df_curva = df_curva
  )
}
