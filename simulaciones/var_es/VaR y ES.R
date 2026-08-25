##' @author Andres Salas
##' @description
##' Codigo para practica en clase simulaciones.

# Configuración inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, psych, readr, PerformanceAnalytics, scales)
options(scipen = 999)

# 1. Definición de la función ----

simulacion_activo <- function(valor_inicial, media_rendimiento, sd_rendimiento, periodos){
  media_rendimiento <- media_rendimiento
  sd_rendimiento <- sd_rendimiento
  activo <- data.frame(periodo = 0:periodos) %>% 
    mutate(retorno = c(0, rnorm(n = periodos, mean = media_rendimiento, sd =sd_rendimiento))) %>% 
    mutate(rendimiento = 1 + retorno) %>% 
    mutate(rend_acum = cumprod(rendimiento)) %>%
    mutate(retorno_acum = rend_acum - 1) %>% 
    mutate(capital = valor_inicial*rend_acum, .after = 1)
  return(activo)
}

# 2. Realizando la simulación a un periodo ----

# 2.1 Simulación ----

valor_inicial <- 100000
retorno <- 0.04
volatilidad <- 0.10
tiempo <- 1
numero_simulaciones <- 100000

monte_carlo_sim <-
  purrr::map(.x = rep(valor_inicial, numero_simulaciones), .f = simulacion_activo,
             periodos = tiempo,
             media_rendimiento = retorno,
             sd_rendimiento = volatilidad) %>% 
  list_rbind(names_to = "simulacion")

# 2.2. Calculo del VaR ----

valor_riesgo <- function(retornos, capital, probabilidad){
  VaR <- quantile(retornos, probs = probabilidad)
  ES <- mean(retornos[retornos<VaR])
  VaR_Cap <- -capital*VaR
  ES_Cap <- -capital*ES
  return(list("VaR" = VaR,
              "ES" = ES,
              "VaR_Capital" = VaR_Cap,
              "ES_Capital" = ES_Cap))
}

sim_final <- monte_carlo_sim %>% 
  group_by(simulacion) %>% 
  summarise(capital = last(capital),
            retornos = last(retorno),
            retorno_acum = last(retorno_acum))

alpha  <- 0.05

VaR_sim_1 <- valor_riesgo(retornos = sim_final$retorno_acum, capital = valor_inicial, probabilidad = alpha)

VaR_sim_1$VaR %>% label_percent(accuracy = 0.01)(.)
VaR_sim_1$VaR_Capital %>% label_currency()(.)

# 2.3 Calculo del ES ----

VaR_sim_1$ES %>% label_percent(accuracy = 0.01)(.)
VaR_sim_1$ES_Capital %>% label_currency()(.)

# 3. Multiples periodos ----

valor_inicial <- 100000
retorno <- 0.04
volatilidad <- 0.10
tiempo <- 12
numero_simulaciones <- 100000

monte_carlo_sim_12 <-
  purrr::map(.x = rep(valor_inicial, numero_simulaciones), .f = simulacion_activo,
             periodos = tiempo,
             media_rendimiento = retorno,
             sd_rendimiento = volatilidad) %>% 
  list_rbind(names_to = "simulacion")

# 2.2. Calculo del VaR ----

sim_final_12 <- monte_carlo_sim_12 %>% 
  group_by(simulacion) %>% 
  summarise(capital = last(capital),
            retornos = last(retorno),
            retorno_acum = last(retorno_acum))

VaR_sim_12 <- valor_riesgo(retornos = sim_final_12$retorno_acum, probabilidad = alpha, capital = valor_inicial)

VaR_sim_12$VaR %>% label_percent(accuracy = 0.01)(.)
VaR_sim_12$VaR_Capital %>% label_currency()(.)

# 2.3 Calculo del ES ----

VaR_sim_12$ES %>% label_percent(accuracy = 0.01)(.)
VaR_sim_12$ES_Capital %>% label_currency()(.)
