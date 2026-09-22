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

# 0. Reconstruccion del portafolio (ver unidad "Retornos y portafolios manuales") ----

symbols <- c("SPY","EFA", "IJS", "EEM", "AGG")

precios <- quantmod::getSymbols(symbols,
                                src = 'yahoo',
                                from = "2010-12-31",
                                periodicity = "daily",
                                auto.assign = TRUE,
                                warnings = FALSE) %>%
    purrr::map(~quantmod::Ad(get(.))) %>%
    purrr::reduce(merge.xts) %>%
    `colnames<-`(symbols)

precios_mes <- xts::to.monthly(x = precios, indexAt = "lastof", OHLC = FALSE)
retornos_mes <- diff(log(precios_mes)) %>% na.omit()

retornos_mes_long <- retornos_mes %>%
    tk_tbl(rename_index = "date") %>%
    gather(key = activo, value = retorno, -date, factor_key = T)

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

portfolio_retornos_mes <- PerformanceAnalytics::Return.portfolio(R = retornos_mes,
                                                                 weights = w,
                                                                 rebalance_on = "months") %>%
  `colnames<-`("retornos")

# 1. Desviacion estandar ----

## 1.1. De todos los activos considerados ----

PerformanceAnalytics::StdDev(R = retornos_mes)*100

## 1.2. Del portafolio ----

PerformanceAnalytics::StdDev(R = retornos_mes, weights = w)*100

## 1.3. Graficando los retornos por mes ----

portfolio_retornos_mes %>%
    ggplot(mapping = aes(x = index(.), y = retornos)) +
    geom_point(color = "cornflowerblue") +
    labs(title = "Dispersion de los retornos por fecha", x = "Mes", y = "Retorno") +
    theme_light() +
    theme(plot.title = element_text(size = 11),
          axis.title = element_text(size = 9),
          panel.grid = element_blank()) +
    scale_y_continuous(labels = label_percent(accuracy = 0.01))


### 1.3.1. Comparando con la desviacion estandar para observar retornos con desviaciones altas ----

sd_plot <- sd(portfolio_retornos_mes)
mean_plot <- mean(portfolio_retornos_mes)

portfolio_retornos_mes %>%
  as.data.frame() %>%
  rownames_to_column(var = "Fecha") %>%
  mutate(Fecha = ymd(Fecha)) %>%
  mutate(hist_col_red = if_else(retornos < (mean_plot - sd_plot),
                                retornos, as.numeric(NA)),
         hist_col_green = if_else(retornos > (mean_plot + sd_plot),
                                  retornos, as.numeric(NA)),
         hist_col_blue = if_else(retornos >= (mean_plot - sd_plot) & retornos <= (mean_plot + sd_plot),
                                 retornos, as.numeric(NA))) %>%
  ggplot(aes(x = Fecha)) +
  geom_point(aes(y = hist_col_red),
             color = "red") +
  geom_point(aes(y = hist_col_green),
             color = "green") +
  geom_point(aes(y = hist_col_blue),
             color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot),
             color = "purple",
             linetype = "dotted") +
  geom_hline(yintercept = (mean_plot-sd_plot),
             color = "purple",
             linetype = "dotted") +
  labs(title = "Dispersion de los retornos por mes", y = "Retorno", x = "Mes") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  theme_light() +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank())

## 1.4. Comparacion del portafolio y los activos ----

retornos_mes_long %>%
  group_by(activo) %>%
  summarise(retorno_esperado = mean(retorno),
            desviacion = sd(retorno)) %>%
  add_row(activo = "Portfolio",
          desviacion =
            sd(portfolio_retornos_mes),
          retorno_esperado =
            mean(portfolio_retornos_mes)) %>%
  ggplot(aes(x = desviacion,
             y = retorno_esperado,
             color = activo)) +
  geom_point(size = 2) +
  annotate("text",
           x = sd(portfolio_retornos_mes) * 1.13,
           y = mean(portfolio_retornos_mes),
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Retorno esperado vs riesgo",
       x = "Riesgo (StdDev)",
       y = "Retorno esperado") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01))

## 1.5. Desviacion estandar con ventana movil ----

ventana <- 24 # Dos años

rolling_sd <- rollapply(data = portfolio_retornos_mes,
                        width = ventana,
                        FUN = sd) %>%
  na.omit() %>%
  `colnames<-`("rolling_sd")

highchart(type = "stock") %>%
  hc_title(text = "Volatilidad movil de 24 meses") %>%
  hc_add_series(round(rolling_sd,4)*100,
                color = "cornflowerblue",
                name = "StdDev") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(labels = list(format = "{value}%"),
           opposite = FALSE,
           title = list(text = "StdDev")) %>%
  hc_navigator(height = 20) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled= T)

# 2. Asimetria ----

PerformanceAnalytics::skewness(retornos_mes)
PerformanceAnalytics::skewness(portfolio_retornos_mes)

retornos_mes_long %>%
  group_by(activo) %>%
  summarize(skew_accion = skewness(retorno)) %>%
  add_row(activo = "Portafolio",
          skew_accion = skewness(portfolio_retornos_mes)) %>%
  ggplot(aes(x = activo,
             y = skew_accion,
             colour = activo)) +
  geom_point() +
  annotate("text",
           x = "Portafolio",
           y = skewness(portfolio_retornos_mes) + 0.05,
           label = "Portafolio",
           color = "cornflowerblue") +
  theme_light() +
  labs(title = "Asimetria por activo y portafolio", x = "Activo", y = "Asimetria") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank())

## 2.1. Asimetria con ventana movil ----

ventana <- 24

rolling_skew <- rollapply(data = portfolio_retornos_mes,
                          width = ventana,
                          FUN = skewness) %>%
  na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Asimetria movil de 24 meses") %>%
  hc_add_series(rolling_skew,
                color = "cornflowerblue",
                name = "Skewness") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "Skewness"),
           opposite = FALSE) %>%
  hc_navigator(height = 20) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled= T)

# 3. Curtosis ----

# Este calculo de la curtosis esta centrado en 0

kurtosis(retornos_mes)
kurtosis(portfolio_retornos_mes)

retornos_mes_long %>%
  group_by(activo) %>%
  summarize(kurtosis_accion = kurtosis(retorno)) %>%
  add_row(activo = "Portafolio",
          kurtosis_accion = kurtosis(portfolio_retornos_mes)) %>%
  ggplot(aes(x = activo,
             y = kurtosis_accion,
             colour = activo)) +
  geom_point() +
  annotate("text",
           x = "Portafolio",
           y = kurtosis(portfolio_retornos_mes) + 0.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Curtosis por activo y portafolio", x = "Activo", y = "Curtosis") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank())

## 3.1 Curtosis con ventana movil ----

ventana <- 24

rolling_kurtosis <- rollapply(data = portfolio_retornos_mes,
                          width = ventana,
                          FUN = kurtosis) %>%
  na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Curtosis movil de 24 meses") %>%
  hc_add_series(rolling_kurtosis,
                color = "cornflowerblue",
                name = "Curtosis") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "Exceso de Curtosis"),
           opposite = FALSE) %>%
  hc_navigator(height = 20) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled= T)

# 4. Valor en riesgo ----

var_activos <- VaR(R = retornos_mes, p = 0.05, invert = T)
var_activos

var_portafolio <- VaR(R = portfolio_retornos_mes, p = 0.05, invert = T)
var_portafolio

## 4.1. Valor en riesgo por accion ----

retornos_mes_long %>%
  group_by(activo) %>%
  summarize(var_accion = VaR(R = retorno, invert = T, p = 0.05)) %>%
  add_row(activo = "Portafolio",
          var_accion = VaR(R = portfolio_retornos_mes, p = 0.05, invert = T)) %>%
  ggplot(aes(x = activo,
             y = var_accion,
             colour = activo)) +
  geom_point() +
  annotate("text",
           x = "Portafolio",
           y = VaR(R = portfolio_retornos_mes, p = 0.05, invert = T) * 1.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "VaR por activo y portafolio", x = "Activo", y = "VaR") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank()) +
    scale_y_continuous(labels = label_percent(accuracy = 0.01),
                       transform = transform_reverse())

## 4.2. Valor en riesgo vs retorno esperado ----

retornos_mes_long %>%
  group_by(activo) %>%
  summarise(retorno_esperado = mean(retorno),
            var = VaR(R = retorno, p = 0.05, invert = T)) %>%
  add_row(activo = "Portafolio",
          var = VaR(R = portfolio_retornos_mes, p = 0.05, invert = T),
          retorno_esperado = mean(portfolio_retornos_mes)) %>%
  ggplot(aes(x = var,
             y = retorno_esperado,
             color = activo)) +
  geom_point(size = 2) +
  annotate("text",
           x = VaR(R = portfolio_retornos_mes, p = 0.05, invert = T) * 1.05,
           y = mean(portfolio_retornos_mes),
           label = "Portafolio",
           color = "cornflowerblue") +
  theme_light() +
  labs(title = "Retorno esperado vs valor en riesgo",
       x = "Riesgo (VaR)",
       y = "Retorno esperado") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01),
                     transform = transform_reverse())


## 4.3. Valor en riesgo con ventana movil ----

ventana <- 24

rolling_var <- rollapply(data = portfolio_retornos_mes,
                        width = ventana,
                        FUN = function(x) VaR(R = x, p = 0.05, invert = T)) %>%
  na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Valor en riesgo movil de 24 meses") %>%
  hc_add_series(rolling_var*100,
                color = "cornflowerblue",
                name = "VaR") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "VaR"),
           opposite = FALSE,
           reversed = TRUE,
           labels = list(format = "{value}%")) %>%
  hc_navigator(height = 20) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled= T)

# 5. Perdida esperada ----

es_activos <- ES(R = retornos_mes, p = 0.05, invert = T)
es_activos

es_portafolio <- ES(R = portfolio_retornos_mes, p = 0.05, invert = T)
es_portafolio

## 5.1. Perdida esperada por accion ----

retornos_mes_long %>%
  group_by(activo) %>%
  summarize(es_accion = ES(R = retorno, invert = T, p = 0.05)) %>%
  add_row(activo = "Portafolio",
          es_accion = ES(R = portfolio_retornos_mes, p = 0.05, invert = T)) %>%
  ggplot(aes(x = activo,
             y = es_accion,
             colour = activo)) +
  geom_point() +
  annotate("text",
           x = "Portafolio",
           y = ES(R = portfolio_retornos_mes, p = 0.05, invert = T) * 1.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "ES por activo y portafolio", x = "Activo", y = "ES") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01),
                     transform = transform_reverse())

## 5.2. Perdida esperada vs retorno esperado ----

retornos_mes_long %>%
  group_by(activo) %>%
  summarise(retorno_esperado = mean(retorno),
            ES = ES(R = retorno, p = 0.05, invert = T)) %>%
  add_row(activo = "Portafolio",
          ES = ES(R = portfolio_retornos_mes, p = 0.05, invert = T),
          retorno_esperado = mean(portfolio_retornos_mes)) %>%
  ggplot(aes(x = ES,
             y = retorno_esperado,
             color = activo)) +
  geom_point(size = 2) +
  annotate("text",
           x = ES(R = portfolio_retornos_mes, p = 0.05, invert = T) * 1.13,
           y = mean(portfolio_retornos_mes),
           label = "Portafolio",
           color = "cornflowerblue") +
  theme_light() +
  labs(title = "Retorno esperado vs perdida esperada",
       x = "Riesgo (ES)",
       y = "Retorno esperado") +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01),
                     transform = transform_reverse())

## 5.3. Perdida esperada con ventana movil ----

ventana <- 24

rolling_es <- rollapply(data = portfolio_retornos_mes,
                        width = ventana,
                        FUN = function(x) ES(R = x, p = 0.05, invert = T)) %>%
  na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Perdida esperada movil de 24 meses") %>%
  hc_add_series(rolling_es*100,
                color = "cornflowerblue",
                name = "ES") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "ES"),
           opposite = FALSE,
           reversed = TRUE,
           labels = list(format = "{value}%")) %>%
  hc_navigator(height = 20) %>%
  hc_scrollbar(enabled = F) %>%
  hc_exporting(enabled= T)
