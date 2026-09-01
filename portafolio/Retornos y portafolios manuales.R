##' @title Practica retornos y portafolios manuales
##' @description
##' Uso de lenguaje R para calcular los retornos de un activo y algunas formas de calcular el riesgo.

if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, psych, quantmod, TTR,
               purrr, PerformanceAnalytics, highcharter, timetk, readxl, scales)
options(scipen = 999)

# 1. Carga de los precios ----

## 1.1. Descarga de los precios desde yahoo ----

# SPDR S&P 500 ETF (SPY)
# iShares MSCI EAFE ETF (EFA)
# iShares S&P Small-Cap 600 Value ETF (IJS)
# iShares MSCI Emerging Markets ETF (EEM)
# iShares Core U.S. Aggregate Bond ETF (AGG)

# Una forma de consultar los tickers de yahoo

simbolos <- stockSymbols()

symbols <- c("SPY","EFA", "IJS", "EEM", "AGG")

precios <- quantmod::getSymbols(symbols,
                                src = 'yahoo',
                                from = "2010-12-31",
                                #to = "2025-12-31",
                                periodicity = "daily",
                                auto.assign = TRUE,
                                warnings = FALSE) %>%
    purrr::map(~quantmod::Ad(get(.))) %>%
    purrr::reduce(merge.xts) %>%
    `colnames<-`(symbols)

## 1.2. Desde un archivo .csv ----
# NOTA: ruta ajustada a esta carpeta (Portafolio/) - la ruta original del
# profesor ("../Data/TOSF/datos_etf.csv") asume su propia estructura de
# carpetas, que no existe en esta copia. El archivo datos_etf.csv ya viene
# junto al script en el mismo zip.

precios_csv <- read_csv("datos_etf.csv",
                        col_types =
                            cols(date =
                                     col_date(format = "%Y-%m-%d"))) %>%
    timetk::tk_xts(date_var = date)

## 1.3. Desde un archivo excel ----
# NOTA: mismo ajuste de ruta que el CSV.

precios_xlsx <- readxl::read_excel("datos_etf.xlsx",
                                   col_types = c("date", rep("numeric", 5))) %>%
    timetk::tk_xts(date_var = date)

## 1.4 Desde una ubicacion web ----
url <- "https://raw.githubusercontent.com/salas317/Data/main/TOSF/datos_etf.csv"

precios_url <- read_csv(file = url,
                        col_types =
                          cols(date =
                                 col_date(format = "%Y-%m-%d"))) %>%
  timetk::tk_xts(date_var = date)

# 2. Cambio de frecuencia de los precios ----

## 2.1. Mensual ----

precios_mes <- xts::to.monthly(x = precios, indexAt = "lastof", OHLC = FALSE)

## 2.2. Semanal ----

precios_semana <- xts::to.weekly(x = precios, OHLC = FALSE)

## 2.3. Trimestral ----

precios_trimestre <- xts::to.quarterly(x = precios, indexAt = "lastof", OHLC = FALSE)

## 2.4. Anual ----

precios_anual <- xts::to.yearly(x = precios, OHLC = FALSE)

# 3. Calculo de los retornos ----
# Solo trabajaremos con los retornos continuos

# NOTA (encontrado corriendo este script 2026-09-01): con TODAS estas
# librerias cargadas juntas (en particular highcharter + PerformanceAnalytics
# + EnvStats + ggpubr + psych), TTR::ROC(type="continuous") sobre un objeto
# de to.weekly()/to.monthly()/to.quarterly()/to.yearly() hace que R se cierre
# de golpe sin ningun mensaje de error - reproducido incluso con datos
# sinteticos (no depende de los datos de Yahoo). diff(log(x)) calcula
# exactamente el mismo retorno continuo (es la misma formula matematica,
# ln(P_t/P_t-1)) y no tiene ese problema - ademas evita otro bug ya conocido
# de TTR::ROC que desalinea la serie completa en ciertos casos.

retornos_diarios <- diff(log(precios)) %>% na.omit()

retornos_semana <- diff(log(precios_semana)) %>% na.omit()

retornos_mes <- diff(log(precios_mes)) %>% na.omit()

retornos_trimestre <- diff(log(precios_trimestre)) %>% na.omit()

retornos_anual <- diff(log(precios_anual)) %>% na.omit()

# 4. Visualizacion de los retornos ----

## 4.1. ggplot2 ----

retornos_mes_long <- retornos_mes %>%
    tk_tbl(rename_index = "date") %>%
    gather(key = activo,
           value = retorno,
           -date,
           factor_key = T)

# NOTA (encontrado corriendo este script 2026-09-01): con EnvStats cargado
# junto a tidyverse/ggplot2, print() sobre un objeto ggplot no dibuja la
# grafica - en vez de eso imprime la estructura interna del objeto como
# texto (ggplot2 4.x usa el sistema S7, y EnvStats interfiere con esa
# dispatch). Si una grafica de este script sale en blanco o imprime texto
# raro en vez de dibujarse, usar dibujar(p) en vez de print(p) - dibuja el
# grob directamente sin pasar por esa dispatch rota. En RStudio interactivo
# (donde el grafico se muestra en el panel Plots) puede que no haga falta,
# pero si algo sale mal, este es el motivo.
dibujar <- function(p) {
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplotGrob(p))
}

### 4.1.1. Histograma todos las acciones juntas ----
dibujar(retornos_mes_long %>%
    ggplot(mapping = aes(x = retorno, fill = activo, color = activo)) +
    geom_histogram(alpha = 0.45, binwidth = 0.005) +
    labs(title = "Retornos mensuales", x = "Retornos", y = "Frecuencia") +
    theme_light() +
    scale_x_continuous(labels = label_percent(0.01)) +
    theme(panel.grid = element_blank()))

### 4.1.2. Histograma separado por acciones ----
dibujar(retornos_mes_long %>%
    ggplot(mapping = aes(x = retorno, fill = activo, color = activo)) +
    geom_histogram(alpha = 0.45, binwidth = 0.005) +
    labs(title = "Retornos mensuales por activo", x = "Retornos", y = "Frecuencia") +
    theme_light() +
    scale_x_continuous(labels = label_percent(0.01)) +
    facet_wrap(~activo) +
    theme(panel.grid = element_blank()))

### 4.1.3. Densidad de los retornos ----
dibujar(retornos_mes_long %>%
    ggplot(mapping = aes(x = retorno, color = activo)) +
    geom_density(alpha = 1, linewidth = 0.7) +
    labs(title = "Densidad de retornos mensuales por accion", x = "Retornos", y = "Distribución") +
    theme_light() +
    scale_x_continuous(labels = label_percent(0.01)) +
    theme(panel.grid = element_blank()))

### 4.1.4. Graficos de lineas ----
dibujar(retornos_mes_long %>%
    ggplot(mapping = aes(y = retorno, x = date, color = activo)) +
    geom_line(alpha = 1, linewidth = 0.6) +
    labs(title = "Retornos mensuales por accion", x = "Mes", y = "Retorno") +
    theme_light() +
    scale_y_continuous(labels = label_percent(0.01)) +
    theme(panel.grid = element_blank()))

## 4.2. Highcharter ----

# NOTA: cada grafico de highcharter se envuelve en print() a proposito. Si
# corres este script con el boton "Source" de RStudio (source() con
# print.eval=FALSE por defecto), un grafico de highcharter que quede suelto
# al final de un bloque de codigo (sin print() ni asignar+imprimir) NO se
# muestra - ni error ni aviso, simplemente no aparece en el panel Viewer.
# Con print() explicito se muestra siempre, sin importar como se corra el
# script (Source, Source con Echo, o linea por linea con Ctrl+Enter).

### 4.2.1. Graficos de evolucion de los retornos ----

print(highcharter::highchart(type = "stock") %>%
  hc_title(text="Retornos mensuales") %>%
  hc_add_series(data = retornos_mes[, symbols[1]]*100, name = symbols[1]) %>%
  hc_add_series(data = retornos_mes[, symbols[2]]*100, name = symbols[2]) %>%
  hc_add_series(data = retornos_mes[, symbols[3]]*100, name = symbols[3]) %>%
  hc_add_series(data = retornos_mes[, symbols[4]]*100, name = symbols[4]) %>%
  hc_add_series(data = retornos_mes[, symbols[5]]*100, name = symbols[5]) %>%
  hc_legend(enabled = T) %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(labels = list(format = "{value:.1f}%")) %>%
  hc_exporting(enabled = T) %>%
  hc_navigator(height = 10))

print(highcharter::highchart(type = "stock") %>%
  hc_title(text="Retornos semanales") %>%
  hc_add_series(data = retornos_semana[, symbols[1]]*100, name = symbols[1]) %>%
  hc_add_series(data = retornos_semana[, symbols[2]]*100, name = symbols[2]) %>%
  hc_add_series(data = retornos_semana[, symbols[3]]*100, name = symbols[3]) %>%
  hc_add_series(data = retornos_semana[, symbols[4]]*100, name = symbols[4]) %>%
  hc_add_series(data = retornos_semana[, symbols[5]]*100, name = symbols[5]) %>%
  hc_legend(enabled = T) %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(labels = list(format = "{value:.1f}%")) %>%
  hc_exporting(enabled = T) %>%
  hc_navigator(height = 10))

### 4.2.2. Histogramas ----

hc_hist <- hist(retornos_mes[,symbols[1]]*100, plot = F)

print(hchart(hc_hist, color = "cornflowerblue") %>%
  hc_title(text =
             paste(symbols[1],
                   "Distribución de los retornos")) %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_xAxis(labels = list(format = "{value:.1f}%")) %>%
  hc_exporting(enabled = T) %>%
  hc_legend(enabled = F) %>%
  hc_navigator(height = 10))

# 4. Construccion de un portafolio con pesos definidos ----

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

## 4.1. Calculo de los retornos a pie ----

accion1_mes <- retornos_mes[,1]
accion2_mes <- retornos_mes[,2]
accion3_mes <- retornos_mes[,3]
accion4_mes <- retornos_mes[,4]
accion5_mes <- retornos_mes[,5]

portfolio_retornos_mes_pie <- w[1]*accion1_mes +
  w[2]*accion2_mes +
  w[3]*accion3_mes +
  w[4]*accion4_mes +
  w[5]*accion5_mes

names(portfolio_retornos_mes_pie) <- "retornos"

## 4.2. Calculo de retornos con xts y PerformanceAnalytics ----

portfolio_retornos_mes <- PerformanceAnalytics::Return.portfolio(R = retornos_mes,
                                                                 weights = w,
                                                                 rebalance_on = "months") %>%
  `colnames<-`("retornos")

## 4.3. Grafico en highchart ----

print(highchart(type = "stock") %>%
  hc_title(text = "Retornos mensuales de los portafolios") %>%
  hc_add_series(portfolio_retornos_mes$retornos*100,
                name = "Retornos rebalanceados mensualmente",
                color = "cornflowerblue") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(labels = list(format = "{value:.1f}%")) %>%
  hc_legend(enabled = T) %>%
  hc_exporting(enabled = T) %>%
  hc_navigator(height = 10))

# NOTA: ruta ajustada - se guarda en la carpeta actual (Portafolio/) en vez
# de "Data/..." (esa subcarpeta no existe acá).
save.image(file = "Retornos y portafolios manuales.RData")
