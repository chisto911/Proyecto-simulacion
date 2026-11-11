# Flujo de números pseudoaleatorios U(0, 1) basado en el Método Congruencial
# Lineal (mismos parámetros que el generador interno de la Pestaña 2).
#
# A diferencia de una secuencia de longitud fija, el flujo conserva su estado:
# cada llamada continúa la recurrencia x_{n+1} = (a x_n + c) mod m. Esto es
# necesario en los métodos que consumen una cantidad aleatoria de uniformes
# (aceptación y rechazo, método polar).

crear_flujo_uniforme <- function(semilla = 2026) {
  a <- 1664525
  c_inc <- 1013904223
  m <- 4294967296

  x <- as.numeric(floor(abs(semilla))) %% m
  if (is.na(x) || x == 0) x <- 1234567

  function(k = 1L) {
    k <- as.integer(k)
    u <- numeric(k)
    for (i in seq_len(k)) {
      x <<- (a * x + c_inc) %% m
      # Se evita u = 0 exacto para que log(u) siempre esté definido.
      u[i] <- if (x == 0) 0.5 / m else x / m
    }
    u
  }
}
