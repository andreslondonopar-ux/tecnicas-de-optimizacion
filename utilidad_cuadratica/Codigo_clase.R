#' ---
#' title: "Código de la clase: utilidad cuadrática y downside risk"
#' subtitle: "Técnicas de optimización 2026-2 · ejemplos de las diapositivas (10 acciones)"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, xts, quantmod, zoo, PerformanceAnalytics, PortfolioAnalytics,
               ROI, ROI.plugin.quadprog, ROI.plugin.glpk, readr, scales)
options(scipen = 999)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

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
