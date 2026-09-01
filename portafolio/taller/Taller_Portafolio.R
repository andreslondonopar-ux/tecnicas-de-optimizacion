##' @description
##' Solucion de "Actividad en clase - Retornos y portafolios manuales.pdf"
##' (agosto 2026). Cada pregunta del enunciado se cita en un comentario justo
##' antes de resolverla, y cada resultado termina con un cat("Respuesta: ...")
##' explicito. Mismo motor y resultados que Taller_Portafolio.Rmd, en formato
##' de script plano para correr por bloques en RStudio.

# Configuracion inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, psych, quantmod, TTR,
               purrr, PerformanceAnalytics, highcharter, timetk, readxl, scales, htmlwidgets)
options(scipen = 999)

# NOTA: EnvStats cargado junto con tidyverse/ggplot2 rompe la dispatch de
# print() sobre objetos ggplot (ver CLAUDE.md) - se dibuja el grob
# directamente en vez de depender de print().
dibujar <- function(p) {
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplotGrob(p))
}

# ============================================================================================
# PREGUNTA 1 - Carga de precios y seleccion de 15 acciones ----
# ============================================================================================

# ENUNCIADO: "Cargar la informacion de los precios en R y selecciona 15
# acciones del todo el universo."

url_taller <- "https://raw.githubusercontent.com/salas317/Data/main/TOSF/datos_taller_retornos.csv"

precios_todos <- read_csv(url_taller,
                          col_types = cols(date = col_date(format = "%m/%d/%Y"))) %>%
  timetk::tk_xts(date_var = date)

cat("Universo disponible:", ncol(precios_todos), "acciones,", nrow(precios_todos),
    "dias, de", format(zoo::index(precios_todos)[1]), "a",
    format(zoo::index(precios_todos)[nrow(precios_todos)]), "\n")

# 15 acciones de sectores distintos (tecnologia, financiero, salud, consumo
# basico, industrial, energia) - no las 15 primeras del listado, para que el
# portafolio de la pregunta 7 diversifique de verdad.
acciones_15 <- c("AAPL", "MSFT", "NVDA", "GOOGL",   # Tecnologia
                  "JPM", "V", "GS",                  # Financiero
                  "LLY", "JNJ", "UNH",                # Salud
                  "WMT", "KO", "PG",                  # Consumo basico
                  "CAT",                               # Industrial
                  "CVX")                               # Energia

precios <- precios_todos[, acciones_15]
print(head(precios, 3))

# ============================================================================================
# PREGUNTA 2 - Retornos a distintas frecuencias ----
# ============================================================================================

# ENUNCIADO: "Calcular los retornos. a. Diarios b. Semanales c. Mensuales
# d. Trimestrales e. Anuales. Interpreta el resultado de la ultima
# observacion de cada frecuencia para una de las acciones."

# NOTA: diff(log(x)) en vez de TTR::ROC(type="continuous") - con esta misma
# combinacion de librerias, TTR::ROC() sobre un objeto de to.weekly()/
# to.monthly()/etc. crashea R sin ningun mensaje de error (ver CLAUDE.md).

precios_semana <- xts::to.weekly(precios, OHLC = FALSE)
precios_mes <- xts::to.monthly(precios, indexAt = "lastof", OHLC = FALSE)
precios_trimestre <- xts::to.quarterly(precios, indexAt = "lastof", OHLC = FALSE)
precios_anual <- xts::to.yearly(precios, OHLC = FALSE)

retornos_diarios <- diff(log(precios)) %>% na.omit()
retornos_semana <- diff(log(precios_semana)) %>% na.omit()
retornos_mes <- diff(log(precios_mes)) %>% na.omit()
retornos_trimestre <- diff(log(precios_trimestre)) %>% na.omit()
retornos_anual <- diff(log(precios_anual)) %>% na.omit()

ultimo_diario <- as.numeric(tail(retornos_diarios$AAPL, 1))
ultimo_semanal <- as.numeric(tail(retornos_semana$AAPL, 1))
ultimo_mensual <- as.numeric(tail(retornos_mes$AAPL, 1))
ultimo_trimestral <- as.numeric(tail(retornos_trimestre$AAPL, 1))
ultimo_anual <- as.numeric(tail(retornos_anual$AAPL, 1))

cat("Respuesta 2 (AAPL, ultima observacion por frecuencia):\n")
cat("  Diario:", label_percent(accuracy = 0.01)(ultimo_diario),
    "- solo el ultimo dia de negociacion disponible.\n")
cat("  Semanal:", label_percent(accuracy = 0.01)(ultimo_semanal), "\n")
cat("  Mensual:", label_percent(accuracy = 0.01)(ultimo_mensual), "\n")
cat("  Trimestral:", label_percent(accuracy = 0.01)(ultimo_trimestral), "\n")
cat("  Anual:", label_percent(accuracy = 0.01)(ultimo_anual),
    "- acumula 12 meses de crecimiento, por eso es mucho mas alto que el diario.\n")

# ============================================================================================
# PREGUNTA 3 - Histograma de retornos semanales de una accion ----
# ============================================================================================

dibujar(ggplot(data.frame(retorno = as.numeric(retornos_semana$NVDA)), aes(x = retorno)) +
  geom_histogram(bins = 40, fill = "steelblue3", color = "black") +
  labs(title = "NVDA - Histograma de retornos semanales", x = "Retorno", y = "Frecuencia") +
  scale_x_continuous(labels = label_percent(0.1)) +
  theme_light())

cat("Respuesta 3: NVDA - cola derecha mas larga que la izquierda (semanas con",
    "subidas grandes), y mas dispersion que las acciones defensivas del grupo.\n")

# ============================================================================================
# PREGUNTA 4 - Histograma de retornos mensuales de otra accion ----
# ============================================================================================

dibujar(ggplot(data.frame(retorno = as.numeric(retornos_mes$JPM)), aes(x = retorno)) +
  geom_histogram(bins = 25, fill = "darkorange3", color = "black") +
  labs(title = "JPM - Histograma de retornos mensuales", x = "Retorno", y = "Frecuencia") +
  scale_x_continuous(labels = label_percent(0.1)) +
  theme_light())

cat("Respuesta 4: JPM (distinta de NVDA) - distribucion mas compacta y",
    "simetrica, menos observaciones extremas.\n")

# ============================================================================================
# PREGUNTA 5 - Densidad de retornos mensuales de 4 acciones ----
# ============================================================================================

acciones_densidad <- c("NVDA", "JPM", "LLY", "WMT")

retornos_densidad_long <- retornos_mes[, acciones_densidad] %>%
  tk_tbl(rename_index = "date") %>%
  gather(key = activo, value = retorno, -date, factor_key = TRUE)

dibujar(ggplot(retornos_densidad_long, aes(x = retorno, color = activo)) +
  geom_density(linewidth = 0.8) +
  labs(title = "Densidad de retornos mensuales", x = "Retorno", y = "Densidad", color = NULL) +
  scale_x_continuous(labels = label_percent(0.1)) +
  theme_light())

cat("Respuesta 5: NVDA muestra la densidad mas ancha (mayor dispersion); WMT y",
    "LLY las mas altas y angostas (defensivas, mas predecibles); JPM queda",
    "en un punto intermedio.\n")

# ============================================================================================
# PREGUNTA 6 - Grafico dinamico de retornos semanales de 5 acciones ----
# ============================================================================================

acciones_dinamico <- c("AAPL", "MSFT", "NVDA", "JPM", "CVX")

hc_dinamico <- highcharter::highchart(type = "stock") %>%
  hc_title(text = "Retornos semanales") %>%
  hc_add_series(data = retornos_semana[, acciones_dinamico[1]]*100, name = acciones_dinamico[1]) %>%
  hc_add_series(data = retornos_semana[, acciones_dinamico[2]]*100, name = acciones_dinamico[2]) %>%
  hc_add_series(data = retornos_semana[, acciones_dinamico[3]]*100, name = acciones_dinamico[3]) %>%
  hc_add_series(data = retornos_semana[, acciones_dinamico[4]]*100, name = acciones_dinamico[4]) %>%
  hc_add_series(data = retornos_semana[, acciones_dinamico[5]]*100, name = acciones_dinamico[5]) %>%
  hc_legend(enabled = TRUE) %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(labels = list(format = "{value:.1f}%")) %>%
  hc_exporting(enabled = TRUE) %>%
  hc_navigator(height = 10)
# NOTA: print() explicito, no dejar hc_dinamico suelto - si este script se
# corre con el boton "Source" de RStudio (source() con print.eval=FALSE por
# defecto), un widget de highcharter que quede suelto al final de un bloque
# NO se muestra (ni error ni aviso). Con print() se muestra siempre, sin
# importar como se corra el script.
print(hc_dinamico)

# ============================================================================================
# PREGUNTA 7 - Portafolio con proporciones diferentes ----
# ============================================================================================

# ENUNCIADO: "Construya un portafolio a partir de los 15 activos que tiene
# disponibles (las proporciones deben ser diferentes). Emplee los retornos
# mensuales."

# Ponderacion de conviccion decreciente: mas peso a las primeras acciones del
# listado, menos a las ultimas - garantiza 15 pesos genuinamente distintos
# que suman exactamente 100%.
w <- rev(seq_along(acciones_15)) / sum(seq_along(acciones_15))
names(w) <- acciones_15

cat("Respuesta 7 - pesos del portafolio (suman", sum(w), "):\n")
print(data.frame(accion = acciones_15, peso = label_percent(accuracy = 0.01)(w)))

portafolio_mensual <- PerformanceAnalytics::Return.portfolio(R = retornos_mes,
                                                              weights = w,
                                                              rebalance_on = "months") %>%
  `colnames<-`("retornos")

cat("Portafolio (rebalanceo mensual): retorno mensual promedio =",
    label_percent(accuracy = 0.01)(mean(as.numeric(portafolio_mensual))),
    ", desviacion estandar =",
    label_percent(accuracy = 0.01)(sd(as.numeric(portafolio_mensual))), "\n")

# ============================================================================================
# PREGUNTA 8 - Rebalanceo mensual vs. anual ----
# ============================================================================================

# ENUNCIADO: "Calcule los retornos del portafolio con rebalanceos mensuales
# y otro con rebalanceos anuales."

portafolio_anual <- PerformanceAnalytics::Return.portfolio(R = retornos_mes,
                                                            weights = w,
                                                            rebalance_on = "years") %>%
  `colnames<-`("retornos")

cat("Respuesta 8:\n")
print(data.frame(
  rebalanceo = c("Mensual", "Anual"),
  retorno_promedio = label_percent(accuracy = 0.01)(c(mean(as.numeric(portafolio_mensual)),
                                                         mean(as.numeric(portafolio_anual)))),
  desv_estandar = label_percent(accuracy = 0.01)(c(sd(as.numeric(portafolio_mensual)),
                                                     sd(as.numeric(portafolio_anual))))
))

# ============================================================================================
# PREGUNTA 9 - Comparacion grafica de las dos series ----
# ============================================================================================

comparacion_long <- bind_rows(
  data.frame(date = zoo::index(portafolio_mensual), retorno = as.numeric(portafolio_mensual), rebalanceo = "Mensual"),
  data.frame(date = zoo::index(portafolio_anual), retorno = as.numeric(portafolio_anual), rebalanceo = "Anual")
)

dibujar(ggplot(comparacion_long, aes(x = date, y = retorno, color = rebalanceo)) +
  geom_line(linewidth = 0.6) +
  labs(title = "Retornos del portafolio - rebalanceo mensual vs. anual",
       x = "Fecha", y = "Retorno mensual", color = NULL) +
  scale_y_continuous(labels = label_percent(0.1)) +
  theme_light())

diferencia_rebalanceo <- mean(as.numeric(portafolio_anual)) - mean(as.numeric(portafolio_mensual))
cat("Respuesta 9: las dos series se mueven practicamente juntas - diferencia de",
    label_percent(accuracy = 0.01)(diferencia_rebalanceo),
    "en el retorno promedio, y una desviacion estandar casi identica. Con solo",
    "15 activos y ninguno dominando el portafolio, la frecuencia de rebalanceo",
    "no cambia el resultado de forma dramatica en este periodo.\n")

cat("\n=== TALLER TERMINADO OK ===\n")
