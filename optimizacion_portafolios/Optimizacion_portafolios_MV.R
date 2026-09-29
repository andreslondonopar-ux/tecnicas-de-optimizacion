##' @description Optimizacion de portafolios
##' @author Andres Salas

# Carga de paquetes e instalación (en caso de ser necesario) ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, quantmod, TTR,
               stats, zoo, PerformanceAnalytics, PortfolioAnalytics, ROI,
               ROI.plugin.quadprog, ROI.plugin.glpk, readr, CVXR)
options(scipen = 999)

# Definicion automatica de la ruta de trabajo -----------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# 1. Descarga de la informacion y calculo de retornos ----

free_rate <- 0.009489

companies <- read_csv(file = "https://raw.githubusercontent.com/salas317/Data/main/NASDAQ_NYSE.csv",
                      show_col_types = FALSE)[1:20,]

activos <- companies$`Ticker Symbol`

precios <- quantmod::getSymbols(activos, src = 'yahoo',
                                from = "2020-12-31",
                                #to = "2025-08-31",
                                periodicity = "daily",
                                auto.assign = T,
                                warnings = F) %>%
  purrr::map(~quantmod::Ad(get(.))) %>%
  purrr::reduce(merge.xts) %>%
  `colnames<-`(activos)

# Tenemos un error en la consulta. Esto se debe a un ticker que no coincide.

activos <- activos %>% gsub(pattern = "BRK.A", x = ., replacement = "BRK-A")

precios <- quantmod::getSymbols(activos, src = 'yahoo',
                                from = "2020-12-31",
                                #to = "2025-08-31",
                                periodicity = "daily",
                                auto.assign = T,
                                warnings = F) %>%
  purrr::map(~quantmod::Ad(get(.))) %>%
  purrr::reduce(merge.xts) %>%
  `colnames<-`(activos)

retornos_mes <- precios %>%
  xts::to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  TTR::ROC(type = "continuous",
           na.pad = F)

# 2. Cual accion con mayor prima por unidad de riesgo (Sharpe) ----

PerformanceAnalytics::SharpeRatio(R = retornos_mes, Rf = 0, FUN = "StdDev")
PerformanceAnalytics::SharpeRatio(R = retornos_mes, Rf = 0, FUN = "VaR", p = 0.99)
PerformanceAnalytics::SharpeRatio(R = retornos_mes, Rf = 0, FUN = "ES", p = 0.99)

# 3. Portafolio M-V basico ----

## 3.1. Iniciar el portafolio ----

init.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos,
                                                     name = "Portafolio inicial")

## 3.2 Restricciones ----

### 3.2.1. Uso de todo el capital ----

init.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio,
                                                     type = "full_investment")

### 3.2.2. No short selling ----

init.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio,
                                                     type = "long_only")

print(init.portfolio)

## 3.3. Frontera eficiente ----

meansd.eff <- PortfolioAnalytics::create.EfficientFrontier(R = retornos_mes,
                                                           portfolio = init.portfolio,
                                                           type = 'mean-StdDev',
                                                           n.portfolios = 100)

## Evolucion de laos pesos en la frontera eficiente ---

PortfolioAnalytics::chart.EF.Weights(object = meansd.eff,
                 match.col = "StdDev")

### 3.3.1. Sin activos libres de riesgo ----

PortfolioAnalytics::chart.EfficientFrontier(meansd.eff,
                                            match.col = 'StdDev',
                                            type = 'l',
                                            rf=NULL,
                                            main = "Frontera eficiente",
                                            cex.assets = 0.7)

### 3.3.2. Con activos libres de riesgo ----

PortfolioAnalytics::chart.EfficientFrontier(meansd.eff,
                                            match.col = 'StdDev',
                                            type = 'l',
                                            rf = free_rate,
                                            main = "Frontera eficiente con activo libre de riesgo",
                                            cex.assets = 0.7)

## 3.4. Objetivos ----

### 3.4.1. Minimizar el riesgo sin importar el retorno----

risk.portfolio <- PortfolioAnalytics::add.objective(portfolio = init.portfolio,
                                                    type = "risk",
                                                    name = "StdDev")

opt.sd <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                 portfolio = risk.portfolio,
                                                 optimize_method = "ROI",
                                                 trace = T)
opt.sd

PortfolioAnalytics::chart.RiskReward(opt.sd,
                                     risk.col = "StdDev",
                                     return.col = "mean",
                                     chart.assets = TRUE,
                                     rp = F,
                                     # xlim = c(0,0.2),
                                     main = "Minimizacion del riesgo")

### 3.4.2. Maximizar el retorno sin importar el riesgo ----

mean.portfolio <- PortfolioAnalytics::add.objective(portfolio = init.portfolio,
                                                    type = "return",
                                                    name = "mean")

opt.mean <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                   portfolio = mean.portfolio,
                                                   optimize_method = "ROI",
                                                   trace = T)
opt.mean

PortfolioAnalytics::chart.RiskReward(opt.mean,
                                     risk.col = "StdDev",
                                     return.col = "mean",
                                     chart.assets = TRUE,
                                     rp = F,
                                     # xlim = c(0,0.2),
                                     main = "Maximizacion del retorno")

### 3.4.3. Minimizar el riesgo para un retorno dado ----

risk.portfolio.return <-  PortfolioAnalytics::add.constraint(portfolio = init.portfolio,
                                                             type = "return",
                                                             return_target = 0.08)

risk.portfolio.return <- PortfolioAnalytics::add.objective(portfolio = risk.portfolio.return,
                                                           type = "risk",
                                                           name = "StdDev")

opt.sd.return <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                        portfolio = risk.portfolio.return,
                                                        optimize_method = "ROI",
                                                        trace = T)

opt.sd.return

PortfolioAnalytics::chart.RiskReward(opt.sd.return,
                                     risk.col = "StdDev",
                                     return.col = "mean",
                                     chart.assets = TRUE,
                                     rp = F,
                                     # xlim = c(0,0.2),
                                     main = "Minimizacion del riesgo con retorno objetivo")

### 3.4.4. Maximizar Sharpe Ratio ----

sr.portfolio <- PortfolioAnalytics::add.objective(portfolio = init.portfolio,
                                                    type = "risk",
                                                    name = "StdDev")
sr.portfolio <- PortfolioAnalytics::add.objective(portfolio = sr.portfolio,
                                                    type = "return",
                                                    name = "mean")

opt.sr <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                 portfolio = sr.portfolio,
                                                 optimize_method = "ROI",
                                                 trace = T,
                                                 maxSR = TRUE)
opt.sr

PortfolioAnalytics::chart.RiskReward(opt.sr,
                                     risk.col = "StdDev",
                                     return.col = "mean",
                                     chart.assets = TRUE,
                                     rp = F,
                                     # xlim = c(0,0.2),
                                     main = "Portafolio que maximiza el Sharpe")

PortfolioAnalytics::chart.EfficientFrontier(opt.sr,
                                            match.col = 'StdDev',,
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.7,
                                            tangent.line = T,
                                            n.portfolios = 100,
                                            rf = free_rate)

chart.Weights(opt.sr)
chart.Weights(opt.sr, plot.type = "barplot")

diversification(weights = opt.sr$weights) # A mayor numero mejor diversificacion.
HHI(weights = opt.sr$weights) # A mayor numero mas concentracion en pocos activos

### 3.4.5. Comparacion ratios de Sharp ----

SharpeRatio(R = retornos_mes, Rf = 0, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.mean$weights, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.sd$weights, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.sd.return$weights, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.sr$weights, FUN = "StdDev")

## 3.5. Añadiendo una restriccion de pesos ----

box.portfolio <- PortfolioAnalytics::add.constraint(portfolio = init.portfolio,
                                                  type = "box", min = 0, max = 0.3)
box.portfolio

box.eff <- PortfolioAnalytics::create.EfficientFrontier(R = retornos_mes,
                                                            portfolio = box.portfolio,
                                                            type = 'mean-StdDev',
                                                            n.portfolios = 100)
box.eff

PortfolioAnalytics::chart.EfficientFrontier(box.eff,
                                            match.col = 'StdDev',
                                            type = 'l',
                                            rf=NULL,
                                            main = "Frontera eficiente",
                                            cex.assets = 0.7)

box.portfolio <- PortfolioAnalytics::add.objective(portfolio = box.portfolio,
                                                   type = "risk",
                                                   name = "StdDev")
box.portfolio <- PortfolioAnalytics::add.objective(portfolio = box.portfolio,
                                                   type = "return",
                                                   name = "mean")

opt.box.sr <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                  portfolio = box.portfolio,
                                                  optimize_method = "ROI",
                                                  trace = T,
                                                  maxSR = TRUE)
opt.box.sr

PortfolioAnalytics::chart.EfficientFrontier(opt.box.sr,
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.7,
                                            tangent.line = T,
                                            n.portfolios = 100,
                                            rf = 0)

PortfolioAnalytics::chart.EfficientFrontier(opt.box.sr,
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.7,
                                            tangent.line = T,
                                            n.portfolios = 100,
                                            rf = free_rate)

chart.Weights(opt.box.sr)
chart.Weights(opt.box.sr, plot.type = "barplot")

diversification(weights = opt.box.sr$weights)
HHI(weights = opt.box.sr$weights)

SharpeRatio(R = retornos_mes, Rf = 0, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.sr$weights, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.box.sr$weights, FUN = "StdDev")
