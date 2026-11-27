# Proyecto de Simulación · EPN 2026-B

Aplicativo interactivo en **R Shiny** desarrollado para la materia de **Simulación** de la Escuela Politécnica Nacional (Facultad de Ciencias). Integra la generación de números pseudoaleatorios, la integración por Montecarlo, la generación de variables aleatorias discretas y continuas, y las pruebas de bondad de ajuste, implementando los algoritmos desde sus fundamentos (Ross, *Simulación*, capítulos 3 a 5).

- **Aplicación publicada:** `https://<usuario>.shinyapps.io/<nombre_app>` *(completar al publicar)*
- **Código fuente:** este repositorio

## Contenido del aplicativo

La interfaz usa `navbarPage` con una pestaña de inicio y los cinco módulos:

| Pestaña | Contenido |
|---|---|
| **Inicio** | Portada con ilustración, acceso directo a cada módulo, guía de uso paso a paso (cómo usar cada pestaña y cómo leer sus resultados) y esquema de cómo se conectan los módulos. |
| **1. Números pseudoaleatorios** | Método Congruencial Lineal, método de Lehmer y método de los cuadrados medios, implementados desde sus recurrencias (sin `runif`). Desglose aritmético de cada iteración, detección de ciclos, iterantes negativos y degeneración a 0, histograma frente a U(0, 1), gráfico de rezagos, E[X] y V(X), y exportación a `.xlsx` y `.csv`. |
| **2. Integración Montecarlo** | Estimación de ∫ f(x) dx en [a, b] con f(x) ingresada por el usuario. Gráfico geométrico de los puntos aleatorios, gráfico de convergencia con banda de confianza del 95 %, error estándar y comparación con `integrate`. Puede usar el generador interno o la secuencia de la Pestaña 1. |
| **3. V.A. discretas** | Binomial, Poisson y geométrica por transformada inversa con sus recursiones, y una ley discreta arbitraria ingresada por el usuario (se valida que Σ pᵢ = 1; acepta fracciones como `1/6`). Gráfico de barras empírico vs. teórico, tabla de frecuencias, momentos muestrales vs. teóricos, número esperado de comparaciones con y sin ordenar las probabilidades, y exportación a `.xlsx` y `.txt`. |
| **4. V.A. continuas** | Uniforme (transformación lineal), exponencial (transformada inversa) y normal (método polar). Densidad personalizada f(x) en [a, b] por aceptación y rechazo (envolvente uniforme, constante c calculada con `optimize`) o por transformada inversa numérica. Histograma con densidad empírica y teórica, CDF empírica vs. teórica, tasa de aceptación y exportación a `.xlsx` y `.txt`. |
| **5. Bondad de ajuste** | Prueba de Kolmogorov-Smirnov frente a U(0, 1), uniforme, normal o exponencial (parámetros fijos o estimados) y prueba de rachas (sobre/bajo la mediana o ascendentes/descendentes). Muestra de la Pestaña 1, de la Pestaña 4 o de un archivo externo (`.csv`, `.txt`, `.xlsx`). Tabla con estadístico, valor crítico, valor p y decisión para un α configurable, banda KS sobre la ECDF y gráfico de la secuencia de rachas. |

Todas las pestañas de simulación (2, 3 y 4) obtienen sus uniformes del **generador congruencial lineal propio** (`R/flujo_uniforme.R`), con semilla configurable para reproducir los resultados.

## Estructura del repositorio

```
.
├── app.R                  # Interfaz (ui) y servidor (server) del aplicativo
├── R/
│   ├── algoritmos_rng.R   # Pestaña 1: MCL, Lehmer y cuadrados medios
│   ├── montecarlo.R       # Pestaña 2: integración por Montecarlo
│   ├── flujo_uniforme.R   # Generador MCL con estado para las Pestañas 3 y 4
│   ├── discretas.R        # Pestaña 3: binomial, Poisson, geométrica y ley arbitraria
│   ├── continuas.R        # Pestaña 4: uniforme, exponencial, normal polar, densidad propia
│   ├── bondad_ajuste.R    # Pestaña 5: Kolmogorov-Smirnov, rachas y lectura de archivos
│   └── ui_inicio.R        # Pestaña de inicio y guía de uso
└── www/
    ├── custom.css         # Identidad visual EPN
    └── logo_epn.png
```

## Ejecución local

1. Instalar R (≥ 4.1) y, opcionalmente, RStudio o Positron.
2. Instalar los paquetes necesarios:

   ```r
   install.packages(c("shiny", "bslib", "plotly", "DT", "writexl", "readxl"))
   ```

3. Clonar el repositorio:

   ```bash
   git clone https://github.com/chisto911/Proyecto-simulacion.git
   ```

4. Abrir la carpeta del proyecto en R y ejecutar:

   ```r
   shiny::runApp()
   ```

   En RStudio también se puede abrir `app.R` y pulsar **Run App**.

## Publicación en shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<usuario>", token = "<token>", secret = "<secret>")
rsconnect::deployApp(appName = "<nombre_app>")
```

El `name`, `token` y `secret` se obtienen en shinyapps.io → **Account → Tokens → Show**.

## Referencias

- Ross, S. M. *Simulación* (2.ª ed.). Prentice Hall.
- Wickham, H. *Mastering Shiny*. <https://mastering-shiny.org/>

## Autores

- Cristopher Almeida
- Fernando Naula

Trabajo grupal de Simulación, EPN 2026-B.
