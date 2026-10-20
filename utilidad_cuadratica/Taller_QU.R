#' ---
#' title: "Taller en clase: utilidad cuadrática y medidas de rentabilidad"
#' subtitle: "Técnicas de optimización 2026-2"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---

# Taller en clase - Utilidad cuadratica, medidas de rentabilidad y downside risk
# (QU-Rentabilidad-Downside-Risk.pdf, Andres Felipe Salas, octubre 2026)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, quantmod, zoo, PerformanceAnalytics, PortfolioAnalytics,
               ROI, ROI.plugin.quadprog, ROI.plugin.glpk, readr, scales)
options(scipen = 999)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# 1. Retornos mensuales de 25 acciones (31-12-2020 a 30-09-2026) ----
# Las 25 acciones son las primeras del listado NASDAQ/NYSE usado en el curso (por capitalizacion).
# Los precios se cachean en Data/ para que los resultados sean reproducibles.
archivo_precios <- "Data/precios_diarios.csv"
if (!file.exists(archivo_precios)) {
  companies <- read_csv("https://raw.githubusercontent.com/salas317/Data/main/NASDAQ_NYSE.csv",
                        show_col_types = FALSE)[1:25, ]
  tickers <- c(gsub("BRK.A", "BRK-A", companies$`Ticker Symbol`), "^GSPC")
  px <- quantmod::getSymbols(tickers, src = "yahoo", from = "2020-12-31", to = "2026-09-30",
                             periodicity = "daily", auto.assign = TRUE, warnings = FALSE) %>%
    purrr::map(~ quantmod::Ad(get(.))) %>% purrr::reduce(merge.xts) %>% `colnames<-`(tickers)
  write.zoo(px, archivo_precios, sep = ",")
}
px <- as.xts(read.zoo(archivo_precios, sep = ",", header = TRUE, index.column = 1, FUN = as.Date, check.names = FALSE))
retornos_all <- px %>% xts::to.monthly(OHLC = FALSE, indexAt = "lastof") %>% log() %>% diff() %>% na.omit()
retornos_mes <- retornos_all[, colnames(retornos_all) != "^GSPC"]
retornos_mercado <- retornos_all[, "^GSPC"]
activos <- colnames(retornos_mes)
cat("Acciones:", length(activos), "| Meses:", nrow(retornos_mes),
    "|", as.character(first(index(retornos_mes))), "a", as.character(last(index(retornos_mes))), "\n")

lambda <- 3  # aversion al riesgo elegida (inversionista moderado: entre agresivo <=2 y conservador >=4)

# 2. Portafolio de maxima utilidad cuadratica: todo el capital, sin cortos ----
# 3. Igual, con pesos entre -5% y 60% ----
spec_qu <- function(min_w, max_w) {
  p <- portfolio.spec(assets = activos)
  p <- add.constraint(p, type = "full_investment")
  p <- add.constraint(p, type = "box", min = min_w, max = max_w)
  # PortfolioAnalytics maximiza mean - risk_aversion * Var; la utilidad de la clase es mean - (lambda/2) * Var.
  # Por eso se pasa lambda / 2 (verificado contra un optimizador independiente).
  add.objective(p, type = "quadratic_utility", risk_aversion = lambda / 2)
}
opt2 <- optimize.portfolio(R = retornos_mes, portfolio = spec_qu(0, 1),     optimize_method = "ROI", trace = TRUE)
opt3 <- optimize.portfolio(R = retornos_mes, portfolio = spec_qu(-0.05, 0.60), optimize_method = "ROI", trace = TRUE)
w2 <- opt2$weights; w3 <- opt3$weights
pesos <- tibble(Accion = activos, `Punto 2 (sin cortos)` = round(as.numeric(w2), 4), `Punto 3 (-5% a 60%)` = round(as.numeric(w3), 4))
w2 <- pmax(w2, 0); w2 <- w2 / sum(w2)  # elimina ruido numerico (~1e-16) de ROI
print(pesos, n = Inf)

# 4. Utilidad esperada: E[U] = E[Rp] - (lambda/2) * Var[Rp] ----
mu <- colMeans(retornos_mes); Sigma <- cov(retornos_mes)
medidas <- function(w) {
  w <- as.numeric(w); m <- sum(w * mu); v <- as.numeric(t(w) %*% Sigma %*% w)
  c(Media = m, Varianza = v, Desviacion = sqrt(v), Utilidad = m - lambda / 2 * v)
}
tabla4 <- rbind(`Punto 2` = medidas(w2), `Punto 3` = medidas(w3))
print(round(tabla4, 5))

# Graficas de los portafolios de los puntos 2 y 3 ----
grafico_pesos_qu <- function(w, titulo) {
  d <- tibble(Accion = activos, Peso = as.numeric(w)) %>% filter(abs(Peso) > 0.0005) %>% arrange(Peso)
  ggplot(d, aes(x = factor(Accion, levels = Accion), y = Peso, fill = Peso > 0)) +
    geom_col(show.legend = FALSE) +
    geom_text(aes(label = percent(Peso, accuracy = 0.1), hjust = ifelse(Peso >= 0, -0.1, 1.1)), size = 3) +
    coord_flip() + scale_y_continuous(labels = percent, expand = expansion(mult = 0.18)) +
    labs(title = titulo, x = NULL, y = "Peso") + theme_minimal()
}
print(grafico_pesos_qu(w2, "Punto 2: máxima utilidad cuadrática (sin cortos)"))
print(grafico_pesos_qu(w3, "Punto 3: máxima utilidad cuadrática (pesos de -5% a 60%)"))
riesgo_retorno <- bind_rows(
  tibble(Nombre = activos, Media = mu, Desviacion = sqrt(diag(Sigma)), Tipo = "Acción"),
  tibble(Nombre = c("Portafolio 2", "Portafolio 3"), Media = tabla4[, "Media"],
         Desviacion = tabla4[, "Desviacion"], Tipo = "Portafolio"))
print(ggplot(riesgo_retorno, aes(Desviacion, Media, color = Tipo, label = Nombre)) +
        geom_point(size = 2) + geom_text(vjust = -0.7, size = 2.5, show.legend = FALSE) +
        scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent) +
        labs(title = "Riesgo y retorno mensual: acciones y portafolios", x = "Desviación estándar", y = "Retorno medio") +
        theme_minimal())

# 5. beta* que maximiza la utilidad de la combinacion con activo libre de riesgo ----
# E[U(Rc)] = Rf + beta (E[Rp] - Rf) - (lambda/2) beta^2 Var[Rp]  =>  beta* = (E[Rp] - Rf) / (lambda Var[Rp])
beta_star <- function(w, rf) { m <- medidas(w); (m["Media"] - rf) / (lambda * m["Varianza"]) }
rf2 <- 0.015; rf3 <- 0.010
b2 <- unname(beta_star(w2, rf2)); b3 <- unname(beta_star(w3, rf3))
u_comb <- function(w, rf, b) { m <- medidas(w); rf + b * (m["Media"] - rf) - lambda / 2 * b^2 * m["Varianza"] }
tabla5 <- tibble(Portafolio = c("Punto 2 (Rf = 1,50%)", "Punto 3 (Rf = 1,00%)"),
                 Rf = c(rf2, rf3), beta_estrella = c(b2, b3),
                 `Utilidad de la combinacion` = c(u_comb(w2, rf2, b2), u_comb(w3, rf3, b3)))
print(tabla5)

# 6. Medidas de rentabilidad y downside risk (rebalanceo anual) ----
pf2 <- Return.portfolio(R = retornos_mes, weights = w2, rebalance_on = "years")
pf3 <- Return.portfolio(R = retornos_mes, weights = w3, rebalance_on = "years")
Ra <- merge(retornos_mes, pf2, pf3); colnames(Ra) <- c(activos, "Portafolio 2", "Portafolio 3")
Rf <- 0.01; MAR <- 0.015   # mismos parametros de los ejemplos de las diapositivas
tabla6 <- rbind(
  Sharpe        = as.numeric(SharpeRatio(Ra, Rf = Rf, FUN = "StdDev")),
  Treynor       = as.numeric(TreynorRatio(Ra, retornos_mercado, Rf = Rf, scale = 12)),
  Sortino       = as.numeric(SortinoRatio(Ra, MAR = MAR, threshold = "MAR")),
  Jensen_alpha  = as.numeric(CAPM.jensenAlpha(Ra, retornos_mercado, Rf = Rf)),
  M2            = as.numeric(Modigliani(Ra, retornos_mercado, Rf = Rf)),
  Info_ratio    = as.numeric(InformationRatio(Ra, retornos_mercado, scale = 12)),
  Downside_dev  = as.numeric(DownsideDeviation(Ra, MAR = MAR, method = "full")),
  Semidesviacion = as.numeric(SemiDeviation(Ra))
)
colnames(tabla6) <- colnames(Ra)
print(round(t(tabla6), 4))

write_csv(as_tibble(t(tabla6), rownames = "Activo"), "salidas/tabla_medidas.csv")

# ==============================================================================
# PARTE B - EJEMPLOS DE LA CLASE (diapositivas): 10 acciones, mensual 31-12-2020 a 30-09-2026
# ==============================================================================
tickers_clase <- c("AAPL", "AVGO", "LLY", "MSFT", "NVDA", "ORCL", "PG", "TSLA", "UNH", "WMT")
archivo_clase <- "Data/precios_clase.csv"
if (!file.exists(archivo_clase)) {
  tk <- c(tickers_clase, "^GSPC")
  pc <- quantmod::getSymbols(tk, src = "yahoo", from = "2020-12-31", to = "2026-09-30",
                             periodicity = "daily", auto.assign = TRUE, warnings = FALSE) %>%
    purrr::map(~ quantmod::Ad(get(.))) %>% purrr::reduce(merge.xts) %>% `colnames<-`(tk)
  write.zoo(pc, archivo_clase, sep = ",")
}
pc <- as.xts(read.zoo(archivo_clase, sep = ",", header = TRUE, index.column = 1, FUN = as.Date, check.names = FALSE))
ret_c <- pc %>% xts::to.monthly(OHLC = FALSE, indexAt = "lastof") %>% log() %>% diff() %>% na.omit()
retornos_mes <- ret_c[, tickers_clase]          # mismos nombres que usan las diapositivas
retornos_mercado <- ret_c[, "^GSPC"]
cat("Ejemplos de clase:", ncol(retornos_mes), "acciones |", nrow(retornos_mes), "meses\n")

# Frontera eficiente (sin cortos, todo el capital) ----
init.portfolio <- portfolio.spec(assets = tickers_clase) %>%
  add.constraint(type = "full_investment") %>% add.constraint(type = "long_only")
ef_clase <- create.EfficientFrontier(R = retornos_mes, portfolio = init.portfolio, type = "mean-StdDev", n.portfolios = 50)
chart.EfficientFrontier(ef_clase, match.col = "StdDev", type = "l", main = "Frontera eficiente (10 acciones de clase)", cex.assets = 0.7)

# Utilidad cuadratica con lambda = 1 (por defecto) y lambda = 0.6, y su equivalente media-desviacion ----
# Aqui se usa risk_aversion tal como en las diapositivas (PortfolioAnalytics maximiza media - lambda * Var).
qu <- function(lam) init.portfolio %>% add.objective(type = "quadratic_utility", risk_aversion = lam) %>%
  optimize.portfolio(R = retornos_mes, portfolio = ., optimize_method = "ROI", trace = TRUE)
QU.portfolio  <- qu(1)
QU.portfolio2 <- qu(0.6)
print(QU.portfolio)
print(QU.portfolio2)
ms.portfolio <- init.portfolio %>% add.objective(type = "return", name = "mean") %>%
  add.objective(type = "risk", name = "StdDev") %>%
  optimize.portfolio(R = retornos_mes, portfolio = ., optimize_method = "ROI", trace = TRUE)
print(ms.portfolio)

# Cambio de la utilidad con la aversion al riesgo (lambda de 1 a 10) ----
aversion <- seq(from = 1, to = 10, by = 0.25)
utilidad <- NULL
for (i in aversion) {
  q <- qu(i)
  media <- q[["objective_measures"]][["mean"]]
  des <- q[["objective_measures"]][["StdDev"]]
  UQ <- media - (i / 2) * (des^2)
  utilidad <- rbind(utilidad, c(i, media, des, UQ))
}
utilidad <- utilidad %>% as.data.frame() %>% `colnames<-`(c("Aversion", "Media", "Desviacion", "MaxU"))
print(ggplot(utilidad, aes(x = Aversion, y = MaxU)) + geom_line() + theme_light() +
        labs(title = "Cambio en la utilidad en relación con la aversión al riesgo",
             x = "Aversión al riesgo", y = "Utilidad esperada") +
        scale_y_continuous(labels = label_percent(accuracy = 0.01)) + scale_x_continuous(n.breaks = 10))

# Medidas de rentabilidad y downside risk de las diapositivas (Rf = 1%, MAR = 1.5%, mercado ^GSPC) ----
ejemplos <- rbind(
  Treynor        = as.numeric(TreynorRatio(Ra = retornos_mes, Rb = retornos_mercado, Rf = 0.01, scale = 12)),
  Sortino        = as.numeric(SortinoRatio(R = retornos_mes, MAR = 0.015, threshold = "MAR")),
  Jensen_alpha   = as.numeric(CAPM.jensenAlpha(Ra = retornos_mes, Rb = retornos_mercado, Rf = 0.01)),
  Modigliani_M2  = as.numeric(Modigliani(Ra = retornos_mes, Rb = retornos_mercado, Rf = 0.01)),
  Tracking_error = as.numeric(TrackingError(Ra = retornos_mes, Rb = retornos_mercado, scale = 12)),
  Info_ratio     = as.numeric(InformationRatio(Ra = retornos_mes, Rb = retornos_mercado, scale = 12)),
  Downside_dev   = as.numeric(DownsideDeviation(R = retornos_mes, MAR = 0.015, method = "full")),
  Downside_var   = as.numeric(DownsideDeviation(R = retornos_mes, MAR = 0.015, method = "full"))^2,
  Downside_pot   = as.numeric(DownsidePotential(R = retornos_mes, MAR = 0.015)),
  Semidesviacion = as.numeric(SemiDeviation(R = retornos_mes)),
  Semivarianza   = as.numeric(SemiVariance(R = retornos_mes))
)
colnames(ejemplos) <- tickers_clase
print(round(t(ejemplos), 4))
