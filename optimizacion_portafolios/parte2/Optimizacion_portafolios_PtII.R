##' @description Optimizacion de portafolios fPortfolio
##' @author Andres Salas

# Carga de paquetes e instalación (en caso de ser necesario) ----
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, quantmod, TTR,
               stats, zoo, PerformanceAnalytics, PortfolioAnalytics, ROI,
               ROI.plugin.quadprog, ROI.plugin.glpk, DEoptim, corpcor, CVXR)
options(scipen = 999)

# Definicion automatica de la ruta de trabajo -----------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Descarga de la informacion y calculo de retornos ----

free_rate <- 0.009489

companies <- read_csv(file = "https://raw.githubusercontent.com/salas317/Data/main/NASDAQ_NYSE.csv",
                      show_col_types = FALSE)

activos <- companies$`Ticker Symbol`[1:30]

activos <- activos %>% gsub(pattern = "\\.",  replacement = "-", x = .)

precios <- quantmod::getSymbols(activos, src = 'yahoo',
                                from = "2020-12-31",
                                to = "2026-09-30",
                                periodicity = "daily",
                                auto.assign = T,
                                warnings = F) %>% 
  purrr::map(~quantmod::Ad(get(.))) %>% 
  purrr::reduce(merge.xts) %>% 
  `colnames<-`(activos)

retornos_mes <- precios %>% 
  xts::to.monthly(indexAt = "lastof", OHLC = F) %>% 
  TTR::ROC(type = "continuous",
           na.pad = TRUE) %>% 
  na.omit()

SharpeRatio(R = retornos_mes, Rf = 0, FUN = "StdDev")
SharpeRatio(R = retornos_mes, Rf = 0, FUN = "VaR", p = 0.95)
SharpeRatio(R = retornos_mes, Rf = 0, FUN = "ES", p = 0.95)

# 2. Portafolio M-V basico maximizando el ratio de Sharpe----

opt.sr <- activos %>%  
  portfolio.spec() %>%  
  add.constraint(type = "full_investment") %>%  
  add.constraint(type = "long_only") %>%  
  add.objective(type = "risk",
                name = "StdDev") %>%  
  add.objective(type = "return",
                name = "mean") %>% 
  optimize.portfolio(R = retornos_mes, 
                     optimize_method = "ROI",
                     trace = T, 
                     maxSR = TRUE)
opt.sr

PortfolioAnalytics::chart.RiskReward(opt.sr, 
                                     risk.col = "StdDev", 
                                     return.col = "mean", 
                                     chart.assets = TRUE,
                                     rp = F, 
                                     main = "Portafolio que maximiza el Sharpe")

PortfolioAnalytics::chart.EfficientFrontier(opt.sr, 
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.6,
                                            tangent.line = F,
                                            rf = 0, 
                                            n.portfolios = 100)

PortfolioAnalytics::chart.EfficientFrontier(opt.sr, 
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.6,
                                            tangent.line = T,
                                            rf = free_rate, 
                                            n.portfolios = 100)

## 2.5. Resultados ----

summary(opt.sr)

chart.Weights(opt.sr)
chart.Weights(opt.sr, plot.type = "barplot")
diversification(weights = opt.sr$weights)
HHI(weights = opt.sr$weights)
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.sr$weights, FUN = "StdDev")

# 3. Portafolio M-V con restricciones por grupos ----

gr.portfolio <- activos %>%  
  portfolio.spec() %>%  
  add.constraint(type = "full_investment") %>%  
  add.constraint(type = "long_only")

## 3.1. Restricciones de peso de los grupos ----

gr.portfolio <- gr.portfolio  %>%  add.constraint(type = "group",
                                               groups = list(Tech = c(1:8,11,14,18,20,22,25,27),
                                                             Pharma = c(9,16,29),
                                                             Financial = c(10,12,15,17,21),
                                                             Other = c(13,19,23,24,26,28,30)),
                                               group_min = c(0.1, 0.1, 0.1, 0.1),
                                               group_max = c(0.4, 0.4, 0.3, 0.3))

summary(gr.portfolio)

## 3.2. Objetivos ----

gr.portfolio <- gr.portfolio %>% add.objective(type = "risk",
                                              name = "StdDev") %>% 
  add.objective(type = "return",
                name = "mean")

summary(gr.portfolio)

## 3.4. Optimizacion ----

opt.gr <- gr.portfolio %>% optimize.portfolio(R = retornos_mes,
                                             optimize_method = "ROI",
                                             trace = T,
                                             maxSR = TRUE)
opt.gr

PortfolioAnalytics::chart.RiskReward(opt.gr, 
                                     risk.col = "StdDev", 
                                     return.col = "mean", 
                                     chart.assets = TRUE,
                                     main = "Portafolio que maximiza el Sharpe")

PortfolioAnalytics::chart.EfficientFrontier(opt.gr, 
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.6,
                                            tangent.line = T,
                                            rf = free_rate, 
                                            n.portfolios = 100)

## 3.5. Resultados ----

summary(opt.gr)

chart.Weights(opt.gr)
chart.Weights(opt.gr, plot.type = "barplot")
chart.GroupWeights(opt.gr)
diversification(weights = opt.gr$weights)
SharpeRatio(R = retornos_mes, Rf = free_rate, weights = opt.gr$weights, FUN = "StdDev")

# 5. Portfolio M-V con limite de posiciones ----

## 5.1. Iniciar el portafolio ----

pos.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos)

## 5.2 Restricciones ----

### 5.2.1. Uso de todo el capital ----

pos.portfolio <- PortfolioAnalytics::add.constraint(portfolio = pos.portfolio,
                                                   type = "full_investment")

### 5.2.2. Limite de posiciones ----

pos.portfolio <- PortfolioAnalytics::add.constraint(portfolio = pos.portfolio,
                                                   type = "position_limit", 
                                                   max_pos_long = 4,
                                                   max_pos_short = 0)

pos.portfolio <- PortfolioAnalytics::add.constraint(portfolio = pos.portfolio,
                                                     type = "box", min = 0, max = 1)

## 5.3. Objetivos ----

### 5.3.1. Riesgo como desviacion estandar ----

pos.portfolio <- PortfolioAnalytics::add.objective(portfolio = pos.portfolio,
                                                  type = "risk",
                                                  name = "StdDev")

### 5.3.2. Rentabilidad como media de los retornos ----

pos.portfolio <- PortfolioAnalytics::add.objective(portfolio = pos.portfolio,
                                                  type = "return",
                                                  name = "mean")

summary(pos.portfolio)

## 5.4. Optimizacion ----

# Esta optimización se realiza para maximizar la utilidad cuadrática (Lo discutiremos mas adelante)

opt.pos <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                 portfolio = pos.portfolio,
                                                 optimize_method = "ROI", 
                                                 trace = T, 
                                                 maxSR = FALSE)
opt.pos

PortfolioAnalytics::chart.RiskReward(opt.pos, 
                                     risk.col = "StdDev", 
                                     return.col = "mean", 
                                     chart.assets = TRUE, 
                                     main = "Portafolio que maximiza el Sharpe")

PortfolioAnalytics::chart.EfficientFrontier(opt.pos, 
                                            match.col = 'StdDev',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.6,
                                            tangent.line = T,
                                            rf = free_rate, 
                                            n.portfolios = 100)

## 5.5. Resultados ----

summary(opt.pos)
opt.pos$weights
chart.Weights(opt.pos, 
              plot.type = "barplot", 
              legend.loc = NULL, 
              ylim = c(0,1))
diversification(weights = opt.pos$weights)
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.pos$weights, FUN = "StdDev")
sum(opt.pos$weights)

# 6. Portafolio M-ES maximizando el ratio de Sharpe----

## 6.1. Iniciar el portafolio ----

es.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos)

## 6.2. Restricciones ----

### 6.2.1. Uso de todo el capital ----

es.portfolio <- PortfolioAnalytics::add.constraint(portfolio = es.portfolio,
                                                   type = "full_investment")

### 6.2.2. No short selling ----

es.portfolio <- PortfolioAnalytics::add.constraint(portfolio = es.portfolio,
                                                   type = "long_only")

## 6.3. Objetivos ----

### 6.3.1. Riesgo como desviacion estandar ----

es.portfolio <- PortfolioAnalytics::add.objective(portfolio = es.portfolio,
                                                  type = "risk",
                                                  name = "ES",
                                                  arguments = list(p = 0.95))

### 6.3.2. Rentabilidad como media de los retornos ----

es.portfolio <- PortfolioAnalytics::add.objective(portfolio = es.portfolio,
                                                  type = "return",
                                                  name = "mean")

summary(es.portfolio)

## 6.4. Optimizacion ----

opt.es <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                 portfolio = es.portfolio,
                                                 optimize_method = "ROI", 
                                                 trace = T, 
                                                 maxSR = TRUE)
opt.es

PortfolioAnalytics::chart.RiskReward(opt.es, 
                                     risk.col = "ES", 
                                     return.col = "mean", 
                                     chart.assets = TRUE,
                                     rp = F, 
                                     main = "Portafolio que maximiza el Sharpe")

PortfolioAnalytics::chart.EfficientFrontier(opt.es, 
                                            match.col = 'ES',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.6,
                                            tangent.line = T, 
                                            n.portfolios = 100,
                                            rf = free_rate)

## 6.5. Resultados ----

summary(opt.es)

chart.Weights(opt.es)
diversification(weights = opt.es$weights)
SharpeRatio(R = retornos_mes, Rf = 0, weights = opt.es$weights, FUN = "StdDev")


# 7. Portafolio M - ES con restricción de grupos ----

## 7.1. Iniciar el portafolio ----

gr.es.portfolio <- PortfolioAnalytics::portfolio.spec(assets = activos)

## 7.2 Restricciones ----

### 7.2.1. Uso de todo el capital ----

gr.es.portfolio <- PortfolioAnalytics::add.constraint(portfolio = gr.es.portfolio,
                                                   type = "weight_sum",
                                                   min_sum = 0.7,
                                                   max_sum = 0.7)

### 7.2.2. No short selling ----

gr.es.portfolio <- PortfolioAnalytics::add.constraint(portfolio = gr.es.portfolio,
                                                   type = "box",
                                                   min = -0.05,
                                                   max = 1)

### 7.2.3. Restricciones de peso de los grupos ----

gr.es.portfolio <- add.constraint(portfolio = gr.es.portfolio, 
                               type = "group", 
                               groups = list(Tech = c(1:8,11,14,18,20,22,25,27),
                                             Pharma = c(9,16,29),
                                             Financial = c(10,12,15,17,21),
                                             Other = c(13,19,23,24,26,28,30)),
                               group_min = c(-0.3, -0.3, -0.2, -0.2),
                               group_max = c(0.4, 0.4, 0.3, 0.2))

summary(gr.es.portfolio)

## 7.3. Objetivos ----

### 7.3.1. Riesgo como desviacion estandar ----

gr.es.portfolio <- PortfolioAnalytics::add.objective(portfolio = gr.es.portfolio,
                                                  type = "risk",
                                                  name = "ES")

### 7.3.2. Rentabilidad como media de los retornos ----

gr.es.portfolio <- PortfolioAnalytics::add.objective(portfolio = gr.es.portfolio,
                                                  type = "return",
                                                  name = "mean")

summary(gr.es.portfolio)

## 7.4. Optimizacion ----

opt.gr.es <- PortfolioAnalytics::optimize.portfolio(R = retornos_mes,
                                                 portfolio = gr.es.portfolio,
                                                 optimize_method = "ROI", 
                                                 trace = T, 
                                                 maxSR = TRUE)
opt.gr.es

PortfolioAnalytics::chart.RiskReward(opt.gr.es, 
                                     risk.col = "ES", 
                                     return.col = "mean", 
                                     chart.assets = TRUE,
                                     main = "Portafolio que maximiza Ratio de Sharpe con ES")

PortfolioAnalytics::chart.EfficientFrontier(opt.gr.es, 
                                            match.col = 'ES',
                                            main = "Frontera eficiente con portafolio optimo",
                                            cex.assets = 0.6,
                                            tangent.line = T,
                                            rf = free_rate, 
                                            n.portfolios = 100)

summary(opt.gr.es)

chart.Weights(opt.gr.es)
chart.GroupWeights(opt.gr.es)
diversification(weights = opt.gr.es$weights)
SharpeRatio(R = retornos_mes, Rf = free_rate, weights = opt.gr.es$weights, FUN = "StdDev")
sum(opt.gr.es$weights)
