##' @title Taller en clase - Optimizacion de portafolios (Media-Varianza)
##' @description
##' Resuelve el "Taller Optimizacion de portafolios" (TO 2026-2): 20
##' acciones, precios diarios 2022-12-31 a 2026-09-25, retornos
##' SEMANALES, Sharpe con Rf=0% y Rf=0.216%, frontera eficiente,
##' 3 portafolios (minimo riesgo, minimo riesgo a retorno objetivo,
##' maximo Sharpe) y un cuarto portafolio con restriccion de peso
##' maximo 25% por activo.

if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, quantmod, TTR,
               stats, zoo, PerformanceAnalytics, PortfolioAnalytics, ROI,
               ROI.plugin.quadprog, ROI.plugin.glpk, readr, scales)
options(scipen = 999)

# 0. Descarga de precios y retornos semanales (20 acciones) ----

companies <- read_csv(file = "https://raw.githubusercontent.com/salas317/Data/main/NASDAQ_NYSE.csv",
                      show_col_types = FALSE)[1:20,]

activos <- companies$`Ticker Symbol` %>% gsub(pattern = "BRK.A", x = ., replacement = "BRK-A")

precios <- quantmod::getSymbols(activos, src = 'yahoo',
                                from = "2022-12-31",
                                to = "2026-09-25",
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
# NOTA: TTR::ROC(type="continuous") sobre este objeto (20 columnas, ~194 filas
# semanales) produce un valor corrupto en la primera fila de BRK-A (retorno de
# 603%, un outlier absurdo) - mismo bug de TTR::ROC ya documentado en otras
# materias del proyecto al aplicarse sobre matrices xts anchas con ciertas
# librerias cargadas. diff(log(x)) da exactamente el mismo resultado
# matematico y no tiene ese problema - verificado que el resto de la serie es
# identica entre ambos metodos.

# 1. Sharpe por accion, Rf = 0.000% ----

sharpe_rf0 <- PerformanceAnalytics::SharpeRatio(R = retornos_semana, Rf = 0, FUN = "StdDev")
sharpe_rf0

# 2. Sharpe por accion, Rf = 0.216% ----

rf_semanal <- 0.00216

sharpe_rf1 <- PerformanceAnalytics::SharpeRatio(R = retornos_semana, Rf = rf_semanal, FUN = "StdDev")
sharpe_rf1

# 3. Portafolio de media-varianza (full investment, long only, sin activo libre de riesgo) ----

## 3.0 Especificacion base ----

init.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos, name = "Portafolio MV")
init.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "full_investment")
init.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "long_only")

## 3.1 Frontera eficiente ----

frontera <- PortfolioAnalytics::create.EfficientFrontier(R = retornos_semana,
                                                         portfolio = init.portfolio,
                                                         type = 'mean-StdDev',
                                                         n.portfolios = 100)

PortfolioAnalytics::chart.EfficientFrontier(frontera,
                                            match.col = 'StdDev',
                                            type = 'l',
                                            rf = NULL,
                                            main = "Frontera eficiente",
                                            cex.assets = 0.7)

## 3.2 Frontera eficiente con Rf = 0.216% ----

PortfolioAnalytics::chart.EfficientFrontier(frontera,
                                            match.col = 'StdDev',
                                            type = 'l',
                                            rf = rf_semanal,
                                            main = "Frontera eficiente con activo libre de riesgo",
                                            cex.assets = 0.7)

## 3.3 Portafolio de minimo riesgo ----

risk.portfolio <- PortfolioAnalytics::add.objective(portfolio = init.portfolio, type = "risk", name = "StdDev")

opt.minrisk <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana,
                                                      portfolio = risk.portfolio,
                                                      optimize_method = "ROI",
                                                      trace = T)
opt.minrisk

sharpe_minrisk <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.minrisk$weights, FUN = "StdDev")
sharpe_minrisk

PortfolioAnalytics::chart.RiskReward(opt.minrisk, risk.col = "StdDev", return.col = "mean",
                                     chart.assets = TRUE, rp = F, main = "Minimizacion del riesgo")

## 3.4 Portafolio de minimo riesgo, retorno objetivo ----

retorno_objetivo <- 0.005 # 0.5% semanal, elegido por nosotros

risk.target.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "return",
                                                             return_target = retorno_objetivo)
risk.target.portfolio <- PortfolioAnalytics::add.objective(portfolio = risk.target.portfolio, type = "risk", name = "StdDev")

opt.target <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana,
                                                     portfolio = risk.target.portfolio,
                                                     optimize_method = "ROI",
                                                     trace = T)
opt.target

sharpe_target <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.target$weights, FUN = "StdDev")
sharpe_target

PortfolioAnalytics::chart.RiskReward(opt.target, risk.col = "StdDev", return.col = "mean",
                                     chart.assets = TRUE, rp = F, main = "Minimizacion del riesgo, retorno objetivo")

## 3.5 Portafolio que maximiza el Sharpe ----

sr.portfolio <- PortfolioAnalytics::add.objective(portfolio = init.portfolio, type = "risk", name = "StdDev")
sr.portfolio <- PortfolioAnalytics::add.objective(portfolio = sr.portfolio, type = "return", name = "mean")

opt.sr <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana,
                                                 portfolio = sr.portfolio,
                                                 optimize_method = "ROI",
                                                 trace = T,
                                                 maxSR = TRUE)
opt.sr

sharpe_sr <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.sr$weights, FUN = "StdDev")
sharpe_sr

PortfolioAnalytics::chart.RiskReward(opt.sr, risk.col = "StdDev", return.col = "mean",
                                     chart.assets = TRUE, rp = F, main = "Portafolio que maximiza el Sharpe")

PortfolioAnalytics::chart.EfficientFrontier(opt.sr,
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.7,
                                            tangent.line = T,
                                            n.portfolios = 100,
                                            rf = rf_semanal)

## 3.6 Comparacion de los 3 portafolios: HHI y Sharpe ----

tabla_3 <- data.frame(
  portafolio = c("Minimo riesgo", "Minimo riesgo (retorno objetivo)", "Maximo Sharpe"),
  HHI = c(HHI(weights = opt.minrisk$weights), HHI(weights = opt.target$weights), HHI(weights = opt.sr$weights)),
  Sharpe = c(as.numeric(sharpe_minrisk), as.numeric(sharpe_target), as.numeric(sharpe_sr))
)
tabla_3

# 4. Portafolio con restriccion de peso maximo 25% por activo ----

box.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio, type = "box", min = 0, max = 0.25)
box.portfolio <- PortfolioAnalytics::add.objective(portfolio = box.portfolio, type = "risk", name = "StdDev")
box.portfolio <- PortfolioAnalytics::add.objective(portfolio = box.portfolio, type = "return", name = "mean")

opt.box <- PortfolioAnalytics::optimize.portfolio(R = retornos_semana,
                                                  portfolio = box.portfolio,
                                                  optimize_method = "ROI",
                                                  trace = T,
                                                  maxSR = TRUE)
opt.box

sharpe_box <- SharpeRatio(R = retornos_semana, Rf = rf_semanal, weights = opt.box$weights, FUN = "StdDev")
sharpe_box

PortfolioAnalytics::chart.EfficientFrontier(opt.box,
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente - portafolio con limite de 25% por activo",
                                            cex.assets = 0.7,
                                            tangent.line = T,
                                            n.portfolios = 100,
                                            rf = rf_semanal)

chart.Weights(opt.box, plot.type = "barplot")

tabla_4 <- data.frame(
  portafolio = c("Maximo Sharpe (sin limite)", "Maximo Sharpe (max. 25% por activo)"),
  Sharpe = c(as.numeric(sharpe_sr), as.numeric(sharpe_box))
)
tabla_4
