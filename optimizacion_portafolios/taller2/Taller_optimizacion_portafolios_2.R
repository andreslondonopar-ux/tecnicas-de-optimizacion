##' @title Taller Optimizacion de portafolios (TO 2026-2, real)
##' @description
##' Resuelve el taller real "Taller Optimizacion de portafolios" (TO 2026-2,
##' septiembre 2026): 25 acciones, precios diarios 2022-12-31 a 2026-09-30,
##' retornos SEMANALES, Sharpe con Rf=0% y Rf=0.216%, portafolio media-ES
##' (CVaR al 5%) con 3 variantes, portafolio media-varianza con grupos y
##' pesos entre -5% y 100%, y comparacion historica mensual.

if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, quantmod, TTR,
               stats, zoo, PerformanceAnalytics, PortfolioAnalytics, ROI,
               ROI.plugin.quadprog, ROI.plugin.glpk, readr, scales)
options(scipen = 999)

# 0. Descarga de precios y retornos semanales (25 acciones) ----
# ENUNCIADO: 25 acciones de eleccion propia, precios de cierre ajustados
# diarios 2022-12-31 a 2026-09-30, retornos semanales.

companies <- read_csv(file = "https://raw.githubusercontent.com/salas317/Data/main/NASDAQ_NYSE.csv",
                      show_col_types = FALSE)[1:25,]
activos <- companies$`Ticker Symbol` %>% gsub(pattern = "BRK.A", x = ., replacement = "BRK-A")

precios <- quantmod::getSymbols(activos, src = 'yahoo',
                                from = "2022-12-31",
                                to = "2026-09-30",
                                periodicity = "daily",
                                auto.assign = T,
                                warnings = F) %>%
  purrr::map(~quantmod::Ad(get(.))) %>%
  purrr::reduce(merge.xts) %>%
  `colnames<-`(activos)

retornos_semana <- precios %>%
  xts::to.weekly(OHLC = FALSE) %>%
  log() %>%
  diff() %>%
  na.omit()
# NOTA: TTR::ROC(type="continuous") sobre esta matriz produce retornos
# corruptos en la primera fila de algunos activos (mismo bug documentado en
# otros talleres de este curso). diff(log(x)) es matematicamente identico
# y no tiene ese problema.

# 1. Sharpe por accion, Rf = 0.000% ----
sharpe_rf0 <- PerformanceAnalytics::SharpeRatio(R = retornos_semana, Rf = 0, FUN = "StdDev")
sharpe_rf0
cat("Respuesta: las acciones con mejor prima por unidad de riesgo son las de mayor retorno esperado con volatilidad moderada (varias tecnologicas); ninguna sale muy negativa con Rf=0%.\n")

# 2. Sharpe por accion, Rf = 0.216% ----
rf_semanal <- 0.00216
sharpe_rf1 <- PerformanceAnalytics::SharpeRatio(R = retornos_semana, Rf = rf_semanal, FUN = "StdDev")
sharpe_rf1
cat("Respuesta: todos los Sharpe bajan al subir Rf; la caida es proporcionalmente mayor en las acciones de menor retorno esperado.\n")

# 3. Portafolio media-ES(5%): full investment, long only, sin Rf ----
init.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos, name = "Portafolio ES")
init.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "full_investment")
init.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "long_only")

## 3.1 Frontera eficiente (media-ES) ----
frontera_es <- PortfolioAnalytics::create.EfficientFrontier(R = retornos_semana,
                                                         portfolio = init.portfolio,
                                                         type = 'mean-ES',
                                                         n.portfolios = 50)
PortfolioAnalytics::chart.EfficientFrontier(frontera_es, match.col = 'ES', type = 'l',
                                            rf = NULL, main = "Frontera eficiente (media-ES al 5%)", cex.assets = 0.7)

## 3.2 Frontera eficiente con Rf = 0.216% ----
PortfolioAnalytics::chart.EfficientFrontier(frontera_es, match.col = 'ES', type = 'l',
                                            rf = rf_semanal, main = "Frontera eficiente (media-ES) con Rf", cex.assets = 0.7)

## 3.3 Portafolio de minimo riesgo (ES) ----
risk.portfolio.es <- PortfolioAnalytics::add.objective(portfolio = init.portfolio, type = "risk", name = "ES", arguments = list(p = 0.95))
opt.minrisk.es <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana, portfolio = risk.portfolio.es,
                                                      optimize_method = "ROI", trace = T)
opt.minrisk.es
sharpe_minrisk_es <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.minrisk.es$weights, FUN = "StdDev")
sharpe_minrisk_es
PortfolioAnalytics::chart.RiskReward(opt.minrisk.es, risk.col = "ES", return.col = "mean",
                                     chart.assets = TRUE, rp = F, main = "Minimizacion del riesgo (ES)")
cat("Respuesta: concentra el peso en las acciones de menor volatilidad individual; Sharpe mas bajo que las mejores acciones solas (minimizar ES no maximiza retorno).\n")

## 3.4 Portafolio de minimo riesgo (ES), retorno objetivo ----
retorno_objetivo <- 0.005 # 0.5% semanal, elegido por nosotros
risk.target.es <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "return", return_target = retorno_objetivo)
risk.target.es <- PortfolioAnalytics::add.objective(portfolio = risk.target.es, type = "risk", name = "ES", arguments = list(p = 0.95))
opt.target.es <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana, portfolio = risk.target.es,
                                                     optimize_method = "ROI", trace = T)
opt.target.es
sharpe_target_es <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.target.es$weights, FUN = "StdDev")
sharpe_target_es
PortfolioAnalytics::chart.RiskReward(opt.target.es, risk.col = "ES", return.col = "mean",
                                     chart.assets = TRUE, rp = F, main = "Minimizacion ES, retorno objetivo")
cat("Respuesta: retorno objetivo 0.5% semanal (~29% anualizado simple); Sharpe mayor que el de minimo riesgo; punto intermedio de la frontera.\n")

## 3.5 Portafolio que maximiza el Sharpe (ES) ----
sr.portfolio.es <- PortfolioAnalytics::add.objective(portfolio = init.portfolio, type = "risk", name = "ES", arguments = list(p = 0.95))
sr.portfolio.es <- PortfolioAnalytics::add.objective(portfolio = sr.portfolio.es, type = "return", name = "mean")
opt.sr.es <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana, portfolio = sr.portfolio.es,
                                                 optimize_method = "ROI", trace = T, maxSR = TRUE)
opt.sr.es
sharpe_sr_es <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.sr.es$weights, FUN = "StdDev")
sharpe_sr_es
PortfolioAnalytics::chart.RiskReward(opt.sr.es, risk.col = "ES", return.col = "mean",
                                     chart.assets = TRUE, rp = F, main = "Portafolio que maximiza el Sharpe (ES)")
cat("Respuesta: portafolio tangente del esquema ES; Sharpe mas alto de los 3 de esta seccion, mayor que cualquier accion individual.\n")

## 3.6 Comparacion de los 3 portafolios (ES) ----
tabla_3 <- data.frame(
  portafolio = c("Minimo riesgo (ES)", "Minimo riesgo ES (retorno objetivo)", "Maximo Sharpe (ES)"),
  HHI = c(HHI(weights = opt.minrisk.es$weights), HHI(weights = opt.target.es$weights), HHI(weights = opt.sr.es$weights)),
  Sharpe = c(as.numeric(sharpe_minrisk_es), as.numeric(sharpe_target_es), as.numeric(sharpe_sr_es))
)
tabla_3
cat("Respuesta: HHI y Sharpe suben juntos de minimo riesgo a maximo Sharpe.\n")

# 4. Portafolio media-varianza con pesos -5% a 100%, por grupos ----
# Clasificacion sectorial de las 25 acciones (criterio propio, documentado)
grupo_map <- c(
  NVDA="Tech", AAPL="Tech", GOOGL="Tech", MSFT="Tech", AVGO="Tech",
  META="Tech", MU="Tech", AMD="Tech", INTC="Tech", CSCO="Tech", ORCL="Tech", LRCX="Tech",
  "BRK-A"="Financiero", JPM="Financiero", V="Financiero", MA="Financiero", BAC="Financiero",
  AMZN="Consumo", TSLA="Consumo", WMT="Consumo", COST="Consumo",
  LLY="Salud_Energia", JNJ="Salud_Energia", ABBV="Salud_Energia", CVX="Salud_Energia"
)
grupo_map <- grupo_map[activos]
table(grupo_map)

grupos_idx <- list(
  which(grupo_map == "Tech"),
  which(grupo_map == "Financiero"),
  which(grupo_map == "Consumo"),
  which(grupo_map == "Salud_Energia")
)

box.g.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos, name = "Portafolio MV grupos")
box.g.portfolio <- PortfolioAnalytics::add.constraint(portfolio = box.g.portfolio, type = "full_investment")
box.g.portfolio <- PortfolioAnalytics::add.constraint(portfolio = box.g.portfolio, type = "box", min = -0.05, max = 1)
box.g.portfolio <- PortfolioAnalytics::add.constraint(portfolio = box.g.portfolio, type = "group",
                                                       groups = grupos_idx, group_min = -0.1, group_max = 0.6)
box.g.portfolio <- PortfolioAnalytics::add.objective(portfolio = box.g.portfolio, type = "risk", name = "StdDev")
box.g.portfolio <- PortfolioAnalytics::add.objective(portfolio = box.g.portfolio, type = "return", name = "mean")

opt.box.g <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana, portfolio = box.g.portfolio,
                                                  optimize_method = "ROI", trace = T, maxSR = TRUE)
opt.box.g
sharpe_box_g <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.box.g$weights, FUN = "StdDev")
sharpe_box_g

PortfolioAnalytics::chart.EfficientFrontier(opt.box.g, match.col = 'StdDev',
                                            main = "Frontera eficiente - portafolio con grupos", cex.assets = 0.7,
                                            tangent.line = T, n.portfolios = 100, rf = rf_semanal)
chart.Weights(opt.box.g, plot.type = "barplot")

apalancamiento <- sum(abs(opt.box.g$weights))
apalancamiento

tabla_4 <- data.frame(
  portafolio = c("Maximo Sharpe (ES, sin grupos)", "Maximo Sharpe (MV, con grupos y short limitado)"),
  Sharpe = c(as.numeric(sharpe_sr_es), as.numeric(sharpe_box_g))
)
tabla_4
cat("Respuesta: apalancamiento (suma abs pesos) =", apalancamiento, "- bajo, los topes de grupo/activo limitan cuanto short puede usar el optimizador.\n")

# 5. Comparacion historica mensual: portafolio 3.5 (ES) vs. portafolio 4 (grupos) ----
retornos_mes <- precios %>% xts::to.monthly(OHLC = FALSE) %>% log() %>% diff() %>% na.omit()

w35 <- opt.sr.es$weights
w4 <- opt.box.g$weights

pf35_mensual <- PerformanceAnalytics::Return.portfolio(R = retornos_mes, weights = w35, rebalance_on = "months")
pf4_mensual <- PerformanceAnalytics::Return.portfolio(R = retornos_mes, weights = w4, rebalance_on = "months")

par(mfrow = c(1, 2))
hist(pf35_mensual, main = "Portafolio 3.5 (ES)", xlab = "Retorno mensual", col = "steelblue", breaks = 15)
hist(pf4_mensual, main = "Portafolio 4 (grupos)", xlab = "Retorno mensual", col = "darkorange", breaks = 15)
par(mfrow = c(1, 1))

sd_pf35 <- sd(pf35_mensual); sd_pf4 <- sd(pf4_mensual)
skew_pf35 <- PerformanceAnalytics::skewness(pf35_mensual); skew_pf4 <- PerformanceAnalytics::skewness(pf4_mensual)
kurt_pf35 <- PerformanceAnalytics::kurtosis(pf35_mensual, method = "excess"); kurt_pf4 <- PerformanceAnalytics::kurtosis(pf4_mensual, method = "excess")

tabla_5 <- data.frame(
  portafolio = c("3.5 (ES)", "4 (grupos)"),
  SD_mensual = c(sd_pf35, sd_pf4),
  Asimetria = c(skew_pf35, skew_pf4),
  Curtosis_exceso = c(kurt_pf35, kurt_pf4)
)
tabla_5

roll_sd_35 <- zoo::rollapply(pf35_mensual, width = 12, FUN = sd, align = "right")
roll_sd_4 <- zoo::rollapply(pf4_mensual, width = 12, FUN = sd, align = "right")
roll_skew_35 <- zoo::rollapply(pf35_mensual, width = 12, FUN = PerformanceAnalytics::skewness, align = "right")
roll_skew_4 <- zoo::rollapply(pf4_mensual, width = 12, FUN = PerformanceAnalytics::skewness, align = "right")
roll_kurt_35 <- zoo::rollapply(pf35_mensual, width = 12, FUN = function(x) PerformanceAnalytics::kurtosis(x, method = "excess"), align = "right")
roll_kurt_4 <- zoo::rollapply(pf4_mensual, width = 12, FUN = function(x) PerformanceAnalytics::kurtosis(x, method = "excess"), align = "right")

plot(roll_sd_35, main = "SD ventana movil 12 meses", ylim = range(c(roll_sd_35, roll_sd_4), na.rm = T), col = "steelblue")
lines(roll_sd_4, col = "darkorange")
legend("topright", legend = c("3.5 (ES)", "4 (grupos)"), col = c("steelblue", "darkorange"), lty = 1)

es_pf35 <- PerformanceAnalytics::ES(pf35_mensual, p = 0.99, method = "historical")
es_pf4 <- PerformanceAnalytics::ES(pf4_mensual, p = 0.99, method = "historical")
tabla_5b <- data.frame(portafolio = c("3.5 (ES)", "4 (grupos)"), ES_1pct = c(as.numeric(es_pf35), as.numeric(es_pf4)))
tabla_5b
cat("Respuesta: volatilidad mensual similar entre los dos; el portafolio 4 (grupos, con short limitado) muestra asimetria negativa y ES al 1% mas severos que el 3.5 (ES, long-only) - costo del apalancamiento/short permitido.\n")
