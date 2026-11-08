generar_mcl <- function(x0, a, c_inc, m, N) {
  x0 <- as.numeric(floor(x0))
  a <- as.numeric(floor(a))
  c_inc <- as.numeric(floor(c_inc))
  m <- as.numeric(floor(m))
  N <- as.integer(N)

  if (is.na(m) || m <= 1) stop("El módulo m debe ser un entero mayor que 1.")
  if (is.na(x0) || x0 < 0) stop("La semilla inicial x0 debe ser un entero no negativo.")
  if (is.na(a) || a <= 0) stop("El multiplicador a debe ser un entero positivo.")
  if (is.na(c_inc) || c_inc < 0) stop("El incremento c debe ser un entero no negativo.")

  iter_vec <- integer(N)
  xi_vec <- numeric(N)
  xnext_vec <- numeric(N)
  ui_vec <- numeric(N)
  op_txt <- character(N)
  xray_html <- character(N)
  estado_vec <- character(N)

  vistos <- new.env(hash = TRUE, parent = emptyenv())
  ciclo_detectado <- FALSE
  idx_inicio_ciclo <- NA_integer_
  idx_repite_ciclo <- NA_integer_
  longitud_ciclo <- NA_integer_
  colapso_cero <- FALSE

  x_actual <- x0

  for (i in seq_len(N)) {
    clave <- as.character(x_actual)
    if (!ciclo_detectado && exists(clave, envir = vistos, inherits = FALSE)) {
      ciclo_detectado <- TRUE
      idx_inicio_ciclo <- get(clave, envir = vistos, inherits = FALSE)
      idx_repite_ciclo <- i - 1L
      longitud_ciclo <- idx_repite_ciclo - idx_inicio_ciclo
    }
    if (!exists(clave, envir = vistos, inherits = FALSE)) {
      assign(clave, i - 1L, envir = vistos)
    }

    prod_val <- a * x_actual + c_inc
    x_sig <- prod_val %% m
    u_val <- x_sig / m

    iter_vec[i] <- i
    xi_vec[i] <- x_actual
    xnext_vec[i] <- x_sig
    ui_vec[i] <- round(u_val, 6)

    op_txt[i] <- sprintf("(%s * %s + %s) mod %s = %s",
                         format(a, scientific = FALSE),
                         format(x_actual, scientific = FALSE),
                         format(c_inc, scientific = FALSE),
                         format(m, scientific = FALSE),
                         format(x_sig, scientific = FALSE))

    xray_html[i] <- sprintf(
      "<span class='xray-chip'>(<span class='xray-cut'>%s</span> &times; <span class='xray-seed'>%s</span> + <span class='xray-cut'>%s</span>) mod %s = <span class='xray-core'>%s</span></span>",
      format(a, scientific = FALSE),
      format(x_actual, scientific = FALSE),
      format(c_inc, scientific = FALSE),
      format(m, scientific = FALSE),
      format(x_sig, scientific = FALSE)
    )

    if (x_sig == 0 && !colapso_cero && x_actual == 0) {
      colapso_cero <- TRUE
      estado_vec[i] <- "Colapso a 0"
    } else if (ciclo_detectado) {
      estado_vec[i] <- "En Ciclo"
    } else {
      estado_vec[i] <- "Activo"
    }

    x_actual <- x_sig
  }

  if (ciclo_detectado && idx_inicio_ciclo >= 0 && (idx_inicio_ciclo + 1L) <= N) {
    estado_vec[idx_inicio_ciclo + 1L] <- "Inicio de Ciclo"
  }

  df <- data.frame(
    Iteracion = iter_vec,
    Semilla_xi = xi_vec,
    Operacion = op_txt,
    Siguiente_x = xnext_vec,
    u_i = ui_vec,
    Estado = estado_vec,
    RayosX_HTML = xray_html,
    stringsAsFactors = FALSE
  )

  diag <- construir_diagnostico_rng(
    metodo = "Método Congruencial Lineal (MCL)",
    df = df,
    ciclo_detectado = ciclo_detectado,
    idx_inicio_ciclo = idx_inicio_ciclo,
    idx_repite_ciclo = idx_repite_ciclo,
    longitud_ciclo = longitud_ciclo,
    iterante_negativo = FALSE,
    idx_negativo = NA_integer_,
    val_negativo = NA_real_,
    colapso_cero = colapso_cero
  )

  list(df = df, diagnostico = diag)
}

generar_lehmer <- function(x0, c_mult, n_cifras = NULL, N = 50) {
  x0 <- as.numeric(floor(x0))
  c_mult <- as.numeric(floor(c_mult))
  N <- as.integer(N)

  if (is.na(x0) || x0 <= 0) stop("La semilla x0 debe ser un entero positivo.")
  if (is.na(c_mult) || c_mult <= 0) stop("El multiplicador c debe ser un entero positivo.")

  n_auto <- nchar(sprintf("%.0f", x0))
  n <- if (is.null(n_cifras) || is.na(n_cifras) || n_cifras < n_auto) n_auto else as.integer(n_cifras)
  k <- nchar(sprintf("%.0f", c_mult))
  base_10n <- 10^n

  iter_vec <- integer(N)
  xi_vec <- numeric(N)
  xnext_vec <- numeric(N)
  ui_vec <- numeric(N)
  op_txt <- character(N)
  xray_html <- character(N)
  estado_vec <- character(N)

  vistos <- new.env(hash = TRUE, parent = emptyenv())
  ciclo_detectado <- FALSE
  idx_inicio_ciclo <- NA_integer_
  idx_repite_ciclo <- NA_integer_
  longitud_ciclo <- NA_integer_
  iterante_negativo <- FALSE
  idx_negativo <- NA_integer_
  val_negativo <- NA_real_
  colapso_cero <- FALSE

  x_actual <- x0
  n_efectivo <- 0L

  for (idx in seq_len(N)) {
    i_label <- idx - 1L
    n_efectivo <- idx

    clave <- as.character(x_actual)
    if (!ciclo_detectado && exists(clave, envir = vistos, inherits = FALSE)) {
      ciclo_detectado <- TRUE
      idx_inicio_ciclo <- get(clave, envir = vistos, inherits = FALSE)
      idx_repite_ciclo <- i_label
      longitud_ciclo <- idx_repite_ciclo - idx_inicio_ciclo
    }
    if (!exists(clave, envir = vistos, inherits = FALSE)) {
      assign(clave, i_label, envir = vistos)
    }

    prod_val <- x_actual * c_mult
    prod_str <- sprintf(paste0("%0", n + k, ".0f"), prod_val)
    total_len <- nchar(prod_str)
    corte_izq <- total_len - n
    izq_str <- substr(prod_str, 1, corte_izq)
    der_str <- substr(prod_str, corte_izq + 1, total_len)

    izq_num <- as.numeric(izq_str)
    der_num <- as.numeric(der_str)
    x_sig <- der_num - izq_num
    u_val <- x_actual / base_10n

    iter_vec[idx] <- i_label
    xi_vec[idx] <- x_actual
    xnext_vec[idx] <- x_sig
    ui_vec[idx] <- round(u_val, n)

    op_txt[idx] <- sprintf("%s|%s  -->  %s - %s = %s",
                           izq_str, der_str, der_str, izq_str,
                           ifelse(x_sig >= 0, sprintf(paste0("%0", n, ".0f"), x_sig), as.character(x_sig)))

    xray_html[idx] <- sprintf(
      "<span class='xray-chip'><span class='xray-cut' title='k cifras izquierdas'>%s</span><span class='xray-sep'>|</span><span class='xray-core' title='n cifras derechas'>%s</span> &rarr; %s &minus; %s = <strong>%s</strong></span>",
      izq_str, der_str, der_str, izq_str,
      ifelse(x_sig >= 0, sprintf(paste0("%0", n, ".0f"), x_sig),
             sprintf("<span class='xray-alert'>%s (&lt; 0)</span>", x_sig))
    )

    if (x_sig < 0) {
      iterante_negativo <- TRUE
      idx_negativo <- i_label + 1L
      val_negativo <- x_sig
      estado_vec[idx] <- "Iterante Negativo"
      break
    } else if (x_sig == 0) {
      colapso_cero <- TRUE
      estado_vec[idx] <- "Colapso a 0"
    } else if (ciclo_detectado) {
      estado_vec[idx] <- "En Ciclo"
    } else {
      estado_vec[idx] <- "Activo"
    }

    x_actual <- x_sig
  }

  df <- data.frame(
    Iteracion = iter_vec[seq_len(n_efectivo)],
    Semilla_xi = xi_vec[seq_len(n_efectivo)],
    Operacion = op_txt[seq_len(n_efectivo)],
    Siguiente_x = xnext_vec[seq_len(n_efectivo)],
    u_i = ui_vec[seq_len(n_efectivo)],
    Estado = estado_vec[seq_len(n_efectivo)],
    RayosX_HTML = xray_html[seq_len(n_efectivo)],
    stringsAsFactors = FALSE
  )

  if (ciclo_detectado && !is.na(idx_inicio_ciclo)) {
    pos_fila <- which(df$Iteracion == idx_inicio_ciclo)
    if (length(pos_fila) > 0) df$Estado[pos_fila[1]] <- "Inicio de Ciclo"
  }

  diag <- construir_diagnostico_rng(
    metodo = sprintf("Método de Lehmer (n = %d cifras, k = %d cifras)", n, k),
    df = df,
    ciclo_detectado = ciclo_detectado,
    idx_inicio_ciclo = idx_inicio_ciclo,
    idx_repite_ciclo = idx_repite_ciclo,
    longitud_ciclo = longitud_ciclo,
    iterante_negativo = iterante_negativo,
    idx_negativo = idx_negativo,
    val_negativo = val_negativo,
    colapso_cero = colapso_cero
  )

  list(df = df, diagnostico = diag)
}

generar_cuadrados_medios <- function(x0, n_cifras = 4, N = 50) {
  x0 <- as.numeric(floor(x0))
  n <- as.integer(n_cifras)
  N <- as.integer(N)

  if (is.na(x0) || x0 < 0) stop("La semilla x0 debe ser un entero no negativo.")
  if (is.na(n) || n < 2 || (n %% 2 != 0)) {
    stop("El número de cifras n debe ser un entero par mayor o igual a 2 (ej. 4, 6, 8).")
  }

  n_x0 <- nchar(sprintf("%.0f", x0))
  if (n_x0 > n) {
    n <- if (n_x0 %% 2 == 0) n_x0 else n_x0 + 1L
  }

  base_10n <- 10^n
  mitad <- n %/% 2L

  iter_vec <- integer(N)
  xi_vec <- numeric(N)
  xnext_vec <- numeric(N)
  ui_vec <- numeric(N)
  op_txt <- character(N)
  xray_html <- character(N)
  estado_vec <- character(N)

  vistos <- new.env(hash = TRUE, parent = emptyenv())
  ciclo_detectado <- FALSE
  idx_inicio_ciclo <- NA_integer_
  idx_repite_ciclo <- NA_integer_
  longitud_ciclo <- NA_integer_
  colapso_cero <- FALSE

  x_actual <- x0

  for (idx in seq_len(N)) {
    i_label <- idx - 1L

    clave <- as.character(x_actual)
    if (!ciclo_detectado && exists(clave, envir = vistos, inherits = FALSE)) {
      ciclo_detectado <- TRUE
      idx_inicio_ciclo <- get(clave, envir = vistos, inherits = FALSE)
      idx_repite_ciclo <- i_label
      longitud_ciclo <- idx_repite_ciclo - idx_inicio_ciclo
    }
    if (!exists(clave, envir = vistos, inherits = FALSE)) {
      assign(clave, i_label, envir = vistos)
    }

    cuad_val <- x_actual^2
    cuad_str <- sprintf(paste0("%0", 2L * n, ".0f"), cuad_val)

    izq_str <- substr(cuad_str, 1, mitad)
    centro_str <- substr(cuad_str, mitad + 1L, mitad + n)
    der_str <- substr(cuad_str, mitad + n + 1L, 2L * n)

    x_sig <- as.numeric(centro_str)
    u_val <- x_actual / base_10n

    iter_vec[idx] <- i_label
    xi_vec[idx] <- x_actual
    xnext_vec[idx] <- x_sig
    ui_vec[idx] <- round(u_val, n)

    op_txt[idx] <- sprintf("%s|%s|%s  -->  x_%d = %s",
                           izq_str, centro_str, der_str, i_label + 1L, centro_str)

    xray_html[idx] <- sprintf(
      "<span class='xray-chip'><span class='xray-cut' title='%d cifras descartadas izq'>%s</span><span class='xray-sep'>|</span><span class='xray-core' title='%d cifras centrales extraídas'>%s</span><span class='xray-sep'>|</span><span class='xray-cut' title='%d cifras descartadas der'>%s</span></span>",
      mitad, izq_str, n, centro_str, mitad, der_str
    )

    if (x_actual == 0 || x_sig == 0) {
      colapso_cero <- TRUE
      estado_vec[idx] <- "Colapso a 0"
    } else if (ciclo_detectado) {
      estado_vec[idx] <- "En Ciclo"
    } else {
      estado_vec[idx] <- "Activo"
    }

    x_actual <- x_sig
  }

  df <- data.frame(
    Iteracion = iter_vec,
    Semilla_xi = xi_vec,
    Operacion = op_txt,
    Siguiente_x = xnext_vec,
    u_i = ui_vec,
    Estado = estado_vec,
    RayosX_HTML = xray_html,
    stringsAsFactors = FALSE
  )

  if (ciclo_detectado && !is.na(idx_inicio_ciclo)) {
    pos_fila <- which(df$Iteracion == idx_inicio_ciclo)
    if (length(pos_fila) > 0) df$Estado[pos_fila[1]] <- "Inicio de Ciclo"
  }

  diag <- construir_diagnostico_rng(
    metodo = sprintf("Método de los Cuadrados Medios (n = %d cifras)", n),
    df = df,
    ciclo_detectado = ciclo_detectado,
    idx_inicio_ciclo = idx_inicio_ciclo,
    idx_repite_ciclo = idx_repite_ciclo,
    longitud_ciclo = longitud_ciclo,
    iterante_negativo = FALSE,
    idx_negativo = NA_integer_,
    val_negativo = NA_real_,
    colapso_cero = colapso_cero
  )

  list(df = df, diagnostico = diag)
}

construir_diagnostico_rng <- function(metodo, df, ciclo_detectado,
                                      idx_inicio_ciclo, idx_repite_ciclo, longitud_ciclo,
                                      iterante_negativo, idx_negativo, val_negativo,
                                      colapso_cero) {
  if (iterante_negativo) {
    return(list(
      estado_tipo = "negativo",
      badge_kpi = sprintf("Fallo en x_%d < 0", idx_negativo),
      titulo = sprintf("Iterante Negativo Detectado (x_%d = %.0f < 0)",
                       idx_negativo, val_negativo),
      mensaje = sprintf(
        "En la iteración i = %d, al restar las k cifras izquierdas a las n cifras derechas se obtuvo x_%d = %.0f < 0, lo cual interrumpe la recurrencia estándar de Lehmer.",
        idx_negativo - 1L, idx_negativo, val_negativo
      ),
      longitud_ciclo = NA_integer_
    ))
  }

  if (colapso_cero && (!ciclo_detectado || (!is.na(longitud_ciclo) && longitud_ciclo <= 1L))) {
    return(list(
      estado_tipo = "cero",
      badge_kpi = "Degeneró a 0",
      titulo = "Degeneración Total: Absorción en 0",
      mensaje = "La secuencia alcanzó el estado absorbente x_i = 0 y los valores posteriores colapsan a 0.",
      longitud_ciclo = 1L
    ))
  }

  if (ciclo_detectado) {
    filas_ciclo <- df[df$Iteracion >= idx_inicio_ciclo & df$Iteracion <= idx_repite_ciclo, , drop = FALSE]
    nodos_str <- paste(sprintf("x_%d=%s", filas_ciclo$Iteracion, filas_ciclo$Semilla_xi), collapse = " -> ")

    return(list(
      estado_tipo = "ciclo",
      badge_kpi = sprintf("Ciclo L = %d (en i=%d)", longitud_ciclo, idx_repite_ciclo),
      titulo = sprintf("Ciclo Corto Detectado: x_%d = x_%d (Período L = %d)",
                       idx_repite_ciclo, idx_inicio_ciclo, longitud_ciclo),
      mensaje = sprintf(
        "En la iteración i = %d la semilla repite el valor de i = %d. A partir de ese punto la secuencia se vuelve periódica: [ %s ].",
        idx_repite_ciclo, idx_inicio_ciclo, nodos_str
      ),
      longitud_ciclo = longitud_ciclo
    ))
  }

  list(
    estado_tipo = "ok",
    badge_kpi = sprintf("Sin ciclos en N = %d", nrow(df)),
    titulo = sprintf("Secuencia sin degeneración (%d iteraciones únicas)", nrow(df)),
    mensaje = sprintf(
      "Los %d números generados con %s son distintos entre sí y no presentaron ciclos cortos ni valores negativos.",
      nrow(df), metodo
    ),
    longitud_ciclo = NA_integer_
  )
}
