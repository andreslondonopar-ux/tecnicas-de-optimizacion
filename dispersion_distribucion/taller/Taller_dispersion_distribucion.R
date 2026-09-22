##' @title Taller en clase - Riesgo y distribucion
##' @description
##' Resuelve los 15 puntos del taller "Actividad en clase - Riesgo y
##' distribucion" (TO 2026-2) sobre el portafolio de 5 ETFs de la unidad
##' de Retornos y portafolios manuales, usando retornos SEMANALES.

if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, psych, quantmod, TTR,
               purrr, PerformanceAnalytics, highcharter, timetk, readxl, scales)
options(scipen = 999)

# 0. Reconstruccion del portafolio (retornos semanales) ----

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

precios_semana <- xts::to.weekly(x = precios, OHLC = FALSE)
retornos_semana <- diff(log(precios_semana)) %>% na.omit()

retornos_semana_long <- retornos_semana %>%
    tk_tbl(rename_index = "date") %>%
    gather(key = activo, value = retorno, -date, factor_key = T)

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

portfolio_retornos_semana <- PerformanceAnalytics::Return.portfolio(R = retornos_semana,
                                                                     weights = w,
                                                                     rebalance_on = "weeks") %>%
  `colnames<-`("retornos")

ventana <- 26 # 6 meses en semanas
alpha <- 0.01 # 99% de confianza

# 1. Desviacion estandar por activo ----

PerformanceAnalytics::StdDev(R = retornos_semana)*100

# 2. Desviacion estandar del portafolio ----

PerformanceAnalytics::StdDev(R = retornos_semana, weights = w)*100

# 3. Retornos semanales del portafolio +/- 1 sd ----

sd_plot <- sd(portfolio_retornos_semana)
mean_plot <- mean(portfolio_retornos_semana)

portfolio_retornos_semana %>%
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
  geom_point(aes(y = hist_col_red), color = "red") +
  geom_point(aes(y = hist_col_green), color = "green") +
  geom_point(aes(y = hist_col_blue), color = "blue") +
  geom_hline(yintercept = (mean_plot + sd_plot), color = "purple", linetype = "dotted") +
  geom_hline(yintercept = (mean_plot - sd_plot), color = "purple", linetype = "dotted") +
  labs(title = "Retornos semanales del portafolio vs +/-1 sd", y = "Retorno", x = "Semana") +
  scale_x_date(breaks = pretty_breaks(n = 6)) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  theme_light() +
  theme(plot.title = element_text(size = 11),
        axis.title = element_text(size = 9),
        panel.grid = element_blank())

# 4. Desviacion estandar vs retorno esperado ----

retornos_semana_long %>%
  group_by(activo) %>%
  summarise(retorno_esperado = mean(retorno), desviacion = sd(retorno)) %>%
  add_row(activo = "Portafolio",
          desviacion = sd(portfolio_retornos_semana),
          retorno_esperado = mean(portfolio_retornos_semana)) %>%
  ggplot(aes(x = desviacion, y = retorno_esperado, color = activo)) +
  geom_point(size = 2) +
  annotate("text", x = sd(portfolio_retornos_semana) * 1.13, y = mean(portfolio_retornos_semana),
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Retorno esperado vs riesgo (StdDev)", x = "Riesgo (StdDev)", y = "Retorno esperado") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01))

# 5. Desviacion estandar movil (6 meses) del portafolio ----

rolling_sd <- rollapply(data = portfolio_retornos_semana, width = ventana, FUN = sd) %>%
  na.omit() %>% `colnames<-`("rolling_sd")

highchart(type = "stock") %>%
  hc_title(text = "Volatilidad movil de 6 meses (portafolio)") %>%
  hc_add_series(round(rolling_sd,4)*100, color = "cornflowerblue", name = "StdDev") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(labels = list(format = "{value}%"), opposite = FALSE, title = list(text = "StdDev")) %>%
  hc_navigator(height = 20) %>% hc_scrollbar(enabled = F) %>% hc_exporting(enabled = T)

# 6. Asimetria por activo y portafolio ----

PerformanceAnalytics::skewness(retornos_semana)
PerformanceAnalytics::skewness(portfolio_retornos_semana)

retornos_semana_long %>%
  group_by(activo) %>%
  summarize(skew_accion = skewness(retorno)) %>%
  add_row(activo = "Portafolio", skew_accion = skewness(portfolio_retornos_semana)) %>%
  ggplot(aes(x = activo, y = skew_accion, colour = activo)) +
  geom_point() +
  annotate("text", x = "Portafolio", y = skewness(portfolio_retornos_semana) + 0.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Asimetria por activo y portafolio", x = "Activo", y = "Asimetria") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank())

# 7. Asimetria movil (6 meses) del portafolio ----

rolling_skew <- rollapply(data = portfolio_retornos_semana, width = ventana, FUN = skewness) %>% na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Asimetria movil de 6 meses (portafolio)") %>%
  hc_add_series(rolling_skew, color = "cornflowerblue", name = "Skewness") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "Skewness"), opposite = FALSE) %>%
  hc_navigator(height = 20) %>% hc_scrollbar(enabled = F) %>% hc_exporting(enabled = T)

# 8. Curtosis por activo y portafolio ----

kurtosis(retornos_semana)
kurtosis(portfolio_retornos_semana)

retornos_semana_long %>%
  group_by(activo) %>%
  summarize(kurtosis_accion = kurtosis(retorno)) %>%
  add_row(activo = "Portafolio", kurtosis_accion = kurtosis(portfolio_retornos_semana)) %>%
  ggplot(aes(x = activo, y = kurtosis_accion, colour = activo)) +
  geom_point() +
  annotate("text", x = "Portafolio", y = kurtosis(portfolio_retornos_semana) + 0.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Curtosis por activo y portafolio", x = "Activo", y = "Curtosis") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank())

# 9. Curtosis movil (6 meses) del portafolio ----

rolling_kurtosis <- rollapply(data = portfolio_retornos_semana, width = ventana, FUN = kurtosis) %>% na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Curtosis movil de 6 meses (portafolio)") %>%
  hc_add_series(rolling_kurtosis, color = "cornflowerblue", name = "Curtosis") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "Exceso de Curtosis"), opposite = FALSE) %>%
  hc_navigator(height = 20) %>% hc_scrollbar(enabled = F) %>% hc_exporting(enabled = T)

# 10. VaR a 1 semana, 99% de confianza ----

var_activos <- VaR(R = retornos_semana, p = alpha, invert = T)
var_activos

var_portafolio <- VaR(R = portfolio_retornos_semana, p = alpha, invert = T)
var_portafolio

retornos_semana_long %>%
  group_by(activo) %>%
  summarize(var_accion = VaR(R = retorno, invert = T, p = alpha)) %>%
  add_row(activo = "Portafolio", var_accion = VaR(R = portfolio_retornos_semana, p = alpha, invert = T)) %>%
  ggplot(aes(x = activo, y = var_accion, colour = activo)) +
  geom_point() +
  annotate("text", x = "Portafolio", y = VaR(R = portfolio_retornos_semana, p = alpha, invert = T) * 1.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "VaR(99%) a 1 semana por activo y portafolio", x = "Activo", y = "VaR") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01), transform = transform_reverse())

# 11. VaR vs retorno esperado ----

retornos_semana_long %>%
  group_by(activo) %>%
  summarise(retorno_esperado = mean(retorno), var = VaR(R = retorno, p = alpha, invert = T)) %>%
  add_row(activo = "Portafolio",
          var = VaR(R = portfolio_retornos_semana, p = alpha, invert = T),
          retorno_esperado = mean(portfolio_retornos_semana)) %>%
  ggplot(aes(x = var, y = retorno_esperado, color = activo)) +
  geom_point(size = 2) +
  annotate("text", x = VaR(R = portfolio_retornos_semana, p = alpha, invert = T) * 1.05,
           y = mean(portfolio_retornos_semana), label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Retorno esperado vs VaR(99%)", x = "Riesgo (VaR)", y = "Retorno esperado") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01), transform = transform_reverse())

# 12. VaR movil (6 meses) del portafolio ----

rolling_var <- rollapply(data = portfolio_retornos_semana, width = ventana,
                          FUN = function(x) VaR(R = x, p = alpha, invert = T)) %>% na.omit()

highchart(type = "stock") %>%
  hc_title(text = "VaR(99%) movil de 6 meses (portafolio)") %>%
  hc_add_series(rolling_var*100, color = "cornflowerblue", name = "VaR") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "VaR"), opposite = FALSE, reversed = TRUE, labels = list(format = "{value}%")) %>%
  hc_navigator(height = 20) %>% hc_scrollbar(enabled = F) %>% hc_exporting(enabled = T)

# 13. Perdida esperada (ES) a 1 semana, 99% de confianza ----

es_activos <- ES(R = retornos_semana, p = alpha, invert = T)
es_activos

es_portafolio <- ES(R = portfolio_retornos_semana, p = alpha, invert = T)
es_portafolio

retornos_semana_long %>%
  group_by(activo) %>%
  summarize(es_accion = ES(R = retorno, invert = T, p = alpha)) %>%
  add_row(activo = "Portafolio", es_accion = ES(R = portfolio_retornos_semana, p = alpha, invert = T)) %>%
  ggplot(aes(x = activo, y = es_accion, colour = activo)) +
  geom_point() +
  annotate("text", x = "Portafolio", y = ES(R = portfolio_retornos_semana, p = alpha, invert = T) * 1.05,
           label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "ES(99%) a 1 semana por activo y portafolio", x = "Activo", y = "ES") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01), transform = transform_reverse())

# 14. ES vs retorno esperado ----

retornos_semana_long %>%
  group_by(activo) %>%
  summarise(retorno_esperado = mean(retorno), ES = ES(R = retorno, p = alpha, invert = T)) %>%
  add_row(activo = "Portafolio",
          ES = ES(R = portfolio_retornos_semana, p = alpha, invert = T),
          retorno_esperado = mean(portfolio_retornos_semana)) %>%
  ggplot(aes(x = ES, y = retorno_esperado, color = activo)) +
  geom_point(size = 2) +
  annotate("text", x = ES(R = portfolio_retornos_semana, p = alpha, invert = T) * 1.13,
           y = mean(portfolio_retornos_semana), label = "Portafolio", color = "cornflowerblue") +
  theme_light() +
  labs(title = "Retorno esperado vs ES(99%)", x = "Riesgo (ES)", y = "Retorno esperado") +
  theme(plot.title = element_text(size = 11), axis.title = element_text(size = 9), panel.grid = element_blank()) +
  scale_y_continuous(labels = label_percent(accuracy = 0.01)) +
  scale_x_continuous(labels = label_percent(accuracy = 0.01), transform = transform_reverse())

# 15. ES movil (6 meses) del portafolio ----

rolling_es <- rollapply(data = portfolio_retornos_semana, width = ventana,
                         FUN = function(x) ES(R = x, p = alpha, invert = T)) %>% na.omit()

highchart(type = "stock") %>%
  hc_title(text = "Perdida esperada (99%) movil de 6 meses (portafolio)") %>%
  hc_add_series(rolling_es*100, color = "cornflowerblue", name = "ES") %>%
  hc_add_theme(hc_theme_gridlight()) %>%
  hc_yAxis(title = list(text = "ES"), opposite = FALSE, reversed = TRUE, labels = list(format = "{value}%")) %>%
  hc_navigator(height = 20) %>% hc_scrollbar(enabled = F) %>% hc_exporting(enabled = T)
