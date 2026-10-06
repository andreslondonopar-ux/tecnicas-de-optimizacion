##' @title Taller Optimizacion de portafolios (TO 2026-2, real)
##' @description
##' Resuelve el taller real "Taller Optimizacion de portafolios" (TO 2026-2,
##' octubre 2026): 25 acciones, precios diarios 2022-12-31 a 2026-09-30,
##' retornos SEMANALES, Sharpe con Rf=0% y Rf=0.216%, portafolio media-ES
##' (CVaR al 5%) con 3 variantes, portafolio media-varianza con grupos y
##' pesos entre -5% y 100%, y comparacion historica semanal (rebalanceo mensual).

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
cat("Respuesta 1: con Rf = 0% las 25 acciones tienen Sharpe positivo. Los mas altos son NVDA (0,230), MU (0,188) y META (0,169); los mas bajos, ORCL (0,043), CVX (0,048) y BAC (0,075).\n\n")

# 2. Sharpe por accion, Rf = 0.216% ----
rf_semanal <- 0.00216
sharpe_rf1 <- PerformanceAnalytics::SharpeRatio(R = retornos_semana, Rf = rf_semanal, FUN = "StdDev")
sharpe_rf1
cat("Respuesta 2: al subir Rf a 0,216% semanal todos los Sharpe bajan, porque se resta mas al numerador. El orden de las mejores casi no cambia (NVDA 0,195, MU 0,161, LRCX 0,133), pero las de menor retorno esperado caen mucho en proporcion: CVX pasa a negativo (-0,019) y MA, BRK-A y ORCL quedan cerca de cero. Con Rf = 0,216% solo 24 de las 25 acciones tienen Sharpe positivo.\n\n")

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
cat("Respuesta 3.3 a: El portafolio de minimo ES (ES ~ 2,63% semanal) usa solo 8 de las 25 acciones: JNJ 28,7%, V 19,6%, BRK-A 19,0%, MSFT 15,6%, WMT 11,6%, AMD 2,0%, LRCX 2,0% y COST 1,6%. Las otras 17 quedan en cero. Se apoya en acciones de baja volatilidad (JNJ, V, BRK-A, WMT) mas MSFT.\n\n")
cat("Respuesta 3.3 b: Su Sharpe es 0,0795 (Rf = 0,216%). Es bajo frente a las mejores acciones (NVDA 0,195, MU 0,161) y superior al de unas 16 de las 25. Aun asi, su desviacion semanal (1,53%) es menor que la de cualquier accion individual (la menor es BRK-A, 2,13%): es el efecto de la diversificacion. Minimiza el riesgo sin buscar retorno, por eso queda en el extremo izquierdo de la frontera.\n\n")

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
cat("Respuesta 3.4 a: Se eligio un retorno objetivo de 0,5% semanal (~26% anual simple), intermedio entre el retorno del portafolio de minimo riesgo (0,34%) y el del maximo Sharpe (0,65%). El portafolio usa 11 acciones: JNJ 36,9%, WMT 20,8%, MSFT 13,9%, AMD 6,9%, NVDA 5,2%, BRK-A 4,7%, GOOGL 4,4%, MU 2,3%, META 2,1%, V 2,0% y CSCO 0,8%. Para subir el retorno incorpora tecnologicas (AMD, NVDA, GOOGL, MU, META) y mantiene JNJ y WMT para controlar el ES (~3,02% semanal).\n\n")
cat("Respuesta 3.4 b: Su Sharpe es 0,1615, el doble que el del minimo riesgo (0,0795). Solo NVDA (0,195) lo supera; MU (0,161) queda practicamente igual. Supera al resto de las acciones individuales, con una desviacion semanal de 1,76%.\n\n")
cat("Respuesta 3.4 c: Es un punto intermedio de la frontera: a la derecha del portafolio de minimo riesgo (mas retorno y mas ES) y a la izquierda del portafolio tangente (maximo Sharpe).\n\n")

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
cat("Respuesta 3.5 a: El portafolio de maximo Sharpe (esquema ES) usa 10 acciones: JNJ 27,4%, WMT 20,9%, GOOGL 18,1%, NVDA 13,4%, META 6,3%, AMD 4,9%, CSCO 3,8%, MU 2,0%, MSFT 2,0% y V 1,2%. Combina defensivas (JNJ, WMT) con tecnologicas de alto retorno (GOOGL, NVDA, META).\n\n")
cat("Respuesta 3.5 b: Su Sharpe es 0,2015, mayor que el de todas las acciones individuales (la mejor, NVDA, 0,195). Su retorno es 0,65% semanal con desviacion de 2,14%.\n\n")
cat("Respuesta 3.5 c: Es el portafolio tangente: el punto donde la recta que sale de Rf = 0,216% toca la frontera eficiente media-ES. Esta en la parte alta de la frontera, con mayor retorno y mayor ES (~3,78% semanal) que los otros dos portafolios.\n\n")

## 3.6 Comparacion de los 3 portafolios (ES) ----
tabla_3 <- data.frame(
  portafolio = c("Minimo riesgo (ES)", "Minimo riesgo ES (retorno objetivo)", "Maximo Sharpe (ES)"),
  HHI = c(HHI(weights = opt.minrisk.es$weights), HHI(weights = opt.target.es$weights), HHI(weights = opt.sr.es$weights)),
  Sharpe = c(as.numeric(sharpe_minrisk_es), as.numeric(sharpe_target_es), as.numeric(sharpe_sr_es))
)
tabla_3
cat("Respuesta 3.6 a: Con el HHI (menor = mas diversificado) el portafolio mas diversificado es el de maximo Sharpe (0,178, ~5,6 acciones efectivas), seguido por el de minimo riesgo (0,196, ~5,1) y por ultimo el de retorno objetivo (0,212, ~4,7), que es el mas concentrado porque carga mucho en JNJ (36,9%).\n\n")
cat("Respuesta 3.6 b: El Sharpe sube de forma monotona: minimo riesgo 0,0795 -> retorno objetivo 0,1615 -> maximo Sharpe 0,2015. Cada portafolio exige mas retorno por unidad de riesgo que el anterior, y solo el de maximo Sharpe supera a todas las acciones individuales.\n\n")

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
cat("Respuesta 4 a: Las graficas de arriba muestran la frontera eficiente con las restricciones de caja (-5% a 100% por accion) y de grupo (-10% a 60%), junto con la linea tangente trazada desde Rf = 0,216%. El portafolio optimo es el punto de tangencia.\n\n")
cat("Respuesta 4 b: Posiciones largas: JNJ 25,1%, NVDA 18,1%, JPM 16,8%, GOOGL 16,8%, WMT 14,0%, MU 8,8%, ABBV 7,6%, COST 7,5%, BRK-A 7,4%, META 7,1%, LLY 4,9%, MSFT 2,8% y AMD 2,6%. Posiciones cortas: AVGO, V, MA, BAC y ORCL en el tope de -5% cada una, LRCX (-5,0%), INTC (-3,2%), TSLA (-2,6%), AAPL (-2,5%), CSCO (-1,0%) y CVX (-0,1%); AMZN queda en ~0. Por grupo: Salud/Energia 37,6%, Tech 34,3%, Consumo 18,9% y Financiero 9,2%, todos dentro del rango -10% a 60%.\n\n")
cat("Respuesta 4 c: Su Sharpe es 0,2440, el mas alto de todos: supera al de maximo Sharpe del punto 3.5 (0,2015), al de retorno objetivo (0,1615), al de minimo riesgo (0,0795) y a la mejor accion individual (NVDA, 0,195). Mejora porque puede vender en corto las acciones mas debiles y porque el riesgo es la desviacion estandar, no el ES. Su retorno es 0,71% semanal con desviacion de 2,01%.\n\n")
cat("Respuesta 4 d: La suma de valores absolutos de los pesos es 1,79: posiciones largas por 139,6% del capital y cortas por 39,6%. Es decir, se toma una exposicion bruta de 1,79 veces el capital (79% por encima del capital invertido). Lo considero moderado: esta por debajo de 2x, y el limite de -5% por accion y -10% por grupo impide un apalancamiento mayor, pero no es despreciable.\n\n")

# 5. Comparacion historica semanal (rebalanceo mensual): portafolio 3.5 (ES) vs. portafolio 4 (grupos) ----
w35 <- opt.sr.es$weights
w4 <- opt.box.g$weights

pf35_sem <- PerformanceAnalytics::Return.portfolio(R = retornos_semana, weights = w35, rebalance_on = "months")
pf4_sem <- PerformanceAnalytics::Return.portfolio(R = retornos_semana, weights = w4, rebalance_on = "months")

par(mfrow = c(1, 2))
hist(pf35_sem, main = "Portafolio 3.5 (ES)", xlab = "Retorno semanal", col = "steelblue", breaks = 20)
hist(pf4_sem, main = "Portafolio 4 (grupos)", xlab = "Retorno semanal", col = "darkorange", breaks = 20)
par(mfrow = c(1, 1))

sd_pf35 <- sd(pf35_sem); sd_pf4 <- sd(pf4_sem)
skew_pf35 <- PerformanceAnalytics::skewness(pf35_sem); skew_pf4 <- PerformanceAnalytics::skewness(pf4_sem)
kurt_pf35 <- PerformanceAnalytics::kurtosis(pf35_sem, method = "excess"); kurt_pf4 <- PerformanceAnalytics::kurtosis(pf4_sem, method = "excess")

tabla_5 <- data.frame(
  portafolio = c("3.5 (ES)", "4 (grupos)"),
  SD_semanal = c(sd_pf35, sd_pf4),
  Asimetria = c(skew_pf35, skew_pf4),
  Curtosis_exceso = c(kurt_pf35, kurt_pf4)
)
tabla_5

roll_sd_35 <- zoo::rollapply(pf35_sem, width = 52, FUN = sd, align = "right")
roll_sd_4 <- zoo::rollapply(pf4_sem, width = 52, FUN = sd, align = "right")
roll_skew_35 <- zoo::rollapply(pf35_sem, width = 52, FUN = PerformanceAnalytics::skewness, align = "right")
roll_skew_4 <- zoo::rollapply(pf4_sem, width = 52, FUN = PerformanceAnalytics::skewness, align = "right")
roll_kurt_35 <- zoo::rollapply(pf35_sem, width = 52, FUN = function(x) PerformanceAnalytics::kurtosis(x, method = "excess"), align = "right")
roll_kurt_4 <- zoo::rollapply(pf4_sem, width = 52, FUN = function(x) PerformanceAnalytics::kurtosis(x, method = "excess"), align = "right")

plot(roll_sd_35, main = "SD ventana movil 52 semanas", ylim = range(c(roll_sd_35, roll_sd_4), na.rm = T), col = "steelblue")
lines(roll_sd_4, col = "darkorange")
legend("topright", legend = c("3.5 (ES)", "4 (grupos)"), col = c("steelblue", "darkorange"), lty = 1)

es_pf35 <- PerformanceAnalytics::ES(pf35_sem, p = 0.99, method = "historical")
es_pf4 <- PerformanceAnalytics::ES(pf4_sem, p = 0.99, method = "historical")
tabla_5b <- data.frame(portafolio = c("3.5 (ES)", "4 (grupos)"), ES_1pct = c(as.numeric(es_pf35), as.numeric(es_pf4)))
tabla_5b
cat("Respuesta 5 a: Se calcularon 195 retornos semanales con rebalanceo mensual. El portafolio 3.5 va de -8,35% a +6,94% con media de 0,65% semanal. El portafolio 4 va de -8,86% a +6,20% con media de 0,71%. Los dos histogramas son parecidos; el del portafolio 4 tiene mas masa en la cola izquierda y un retorno medio algo mayor.\n\n")
cat("Respuesta 5 b: La desviacion estandar semanal historica es 2,15% para el 3.5 y 2,01% para el 4. En ventana movil de 52 semanas, el 3.5 oscila entre 1,75% y 2,71% (promedio 2,18%) y el 4 entre 1,57% y 2,57% (promedio 2,06%). El portafolio 4 es menos volatil en todo el historico, coherente con que su objetivo es la desviacion estandar.\n\n")
cat("Respuesta 5 c: Asimetria historica: -0,166 para el 3.5 y -0,679 para el 4. Ambas son negativas, pero la del portafolio 4 es cuatro veces mayor: tiene mas probabilidad de perdidas extremas que de ganancias extremas.\n\n")
cat("Respuesta 5 d: Asimetria en ventana movil de 52 semanas: promedio -0,13 para el 3.5 (rango -0,67 a 0,49) y -0,64 para el 4 (rango -1,31 a -0,01). La del 3.5 llega a ser positiva en algunas ventanas; la del 4 es negativa en todas.\n\n")
cat("Respuesta 5 e: Exceso de curtosis historico: 1,17 para el 3.5 y 2,37 para el 4. Ambos son positivos (colas mas pesadas que la normal) y el del portafolio 4 es el doble.\n\n")
cat("Respuesta 5 f: Exceso de curtosis en ventana movil de 52 semanas: promedio 0,77 para el 3.5 (rango -0,63 a 3,17) y 1,63 para el 4 (rango 0,03 a 5,51). El portafolio 4 tiene colas mas pesadas en casi todas las ventanas.\n\n")
cat("Respuesta 5 g: La perdida esperada historica al 1% es -6,69% semanal para el portafolio 3.5 y -7,14% para el portafolio 4. Con 195 semanas el 1% son solo 2 observaciones, asi que la cifra es ruidosa. Aun asi coincide con la lectura de asimetria y curtosis: el portafolio 4 tiene la cola de perdidas mas severa pese a su menor desviacion estandar.\n\n")
