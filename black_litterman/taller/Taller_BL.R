#' ---
#' title: "Taller en clase: modelo de Black-Litterman"
#' subtitle: "Técnicas de optimización 2026-2"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---

# Practica en clase (Black-Litterman.pdf, Andres Salas, octubre 2026): 10 acciones,
# retornos mensuales 2014-11-28 a 2026-09-30, Rf = 4.0430% anual, retorno de la referencia = 1.008% mensual.

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales, xts, quantmod, zoo, PerformanceAnalytics)
options(scipen = 999, width = 200, tibble.width = Inf, tibble.print_max = Inf)
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Practica I: datos ----
tickers <- c("AAPL", "AVGO", "LLY", "MSFT", "NVDA", "ORCL", "PG", "TSLA", "UNH", "WMT")
archivo_precios <- "Data/precios_bl.csv"
archivo_cap <- "Data/market_cap.csv"
if (!file.exists(archivo_precios)) {
  px <- quantmod::getSymbols(tickers, src = "yahoo", from = "2014-11-28", to = "2026-09-30",
                             periodicity = "daily", auto.assign = TRUE, warnings = FALSE) %>%
    purrr::map(~ quantmod::Ad(get(.))) %>% purrr::reduce(merge.xts) %>% `colnames<-`(tickers)
  write.zoo(px, archivo_precios, sep = ",")
}
if (!file.exists(archivo_cap)) {
  q <- quantmod::getQuote(Symbols = tickers, src = "yahoo", what = yahooQF("Market Capitalization"))
  write_csv(tibble(Symbol = tickers, market_cap = q$`Market Capitalization`), archivo_cap)
}
px <- as.xts(read.zoo(archivo_precios, sep = ",", header = TRUE, index.column = 1, FUN = as.Date, check.names = FALSE))
monthly_returns <- px %>% xts::to.monthly(OHLC = FALSE, indexAt = "lastof") %>% log() %>% diff() %>% na.omit()
cat("Acciones:", ncol(monthly_returns), "| Meses:", nrow(monthly_returns), "|",
    as.character(first(index(monthly_returns))), "a", as.character(last(index(monthly_returns))), "\n")

market_cap <- read_csv(archivo_cap, show_col_types = FALSE) %>% {setNames(.$market_cap, .$Symbol)}
market_cap <- market_cap[colnames(monthly_returns)]
bench_prop <- market_cap / sum(market_cap)
print(tibble(Symbol = names(market_cap), `Market cap (USD)` = as.numeric(market_cap), Peso = percent(as.numeric(bench_prop), accuracy = 0.01)))

t_bill_r <- 0.040430 / 12   # Rf anual 4.0430% pasada a mensual (mismo criterio que la clase: 0.016/12)
bench_ret <- 0.01008        # retorno mensual del portafolio de referencia
cov_mat <- cov(monthly_returns)

# Funciones de la clase ----
CML <- function(mean_retornos, var_cov, rf) {
  optimal_port <- (solve(var_cov) %*% (mean_retornos - rf)) / sum(solve(var_cov) %*% (mean_retornos - rf))
  exp_return <- t(optimal_port) %*% mean_retornos
  exp_risk <- sqrt(t(optimal_port) %*% var_cov %*% optimal_port)
  list(weigths = optimal_port, portfolio_return = exp_return, portfolio_risk = exp_risk,
       SR = (exp_return - rf) / exp_risk)
}
BL_bench_expected_returns <- function(bench_ret, bench_prop, rf, var_cov) {
  norm_fact <- (bench_ret - rf) / t(bench_prop %*% var_cov %*% bench_prop)
  list(BL_expected_returns = var_cov %*% bench_prop * as.double(norm_fact) + rf, normalize_factor = norm_fact)
}
pct <- label_percent(accuracy = 0.01)

# Practica II, punto 1: retornos esperados del portafolio de referencia ----
BL_exp_ret <- BL_bench_expected_returns(bench_ret, bench_prop, t_bill_r, cov_mat)
exp_ret <- BL_exp_ret$BL_expected_returns
cat("Factor normalizador:", as.double(BL_exp_ret$normalize_factor), "\n")
cat("Retorno del portafolio de referencia con estos retornos esperados:", pct(as.double(bench_prop %*% exp_ret)), "\n")
tabla1 <- tibble(Symbol = colnames(monthly_returns), `Peso referencia` = pct(as.numeric(bench_prop)),
                 `Retorno esperado (mensual)` = pct(as.numeric(exp_ret)))
print(tabla1)
# Chequeo: optimizar con estos retornos devuelve los pesos de referencia
print(round(cbind(CML = as.numeric(CML(exp_ret, cov_mat, t_bill_r)$weigths), referencia = as.numeric(bench_prop)), 4))

# Practica III, punto 2: Nvidia 100 pb por encima del valor de referencia ----
track_mat <- t(cov_mat / diag(cov_mat))          # matriz BLT: cuanto se mueve el retorno de i si sube 1 el de j
delta_2 <- setNames(rep(0, 10), colnames(monthly_returns)); delta_2["NVDA"] <- 0.0100
ret_2 <- exp_ret + track_mat %*% delta_2
port_2 <- CML(ret_2, cov_mat, t_bill_r)
tabla2 <- tibble(Symbol = colnames(monthly_returns), `Retorno referencia` = pct(as.numeric(exp_ret)),
                 `Retorno ajustado (NVDA +100 pb)` = pct(as.numeric(ret_2)),
                 `Peso referencia` = pct(as.numeric(bench_prop)), `Peso portafolio ajustado` = pct(as.numeric(port_2$weigths)))
print(tabla2)
cat("Retorno esperado del nuevo portafolio:", pct(as.double(port_2$portfolio_return)), " | referencia:", pct(bench_ret), "\n")

# Punto 3: matriz BLT ----
# Fila i, columna j = Cov(i, j) / Var(j): cuanto cambia el retorno esperado de i cuando sube 1 punto el de j.
print(round(track_mat, 3))

# Punto 4: tres opiniones (NVDA +100 pb, LLY +150 pb, AVGO -75 pb) ----
delta_4 <- setNames(rep(0, 10), colnames(monthly_returns))
delta_4[c("NVDA", "LLY", "AVGO")] <- c(0.0100, 0.0150, -0.0075)
ret_4 <- exp_ret + track_mat %*% delta_4
port_4 <- CML(ret_4, cov_mat, t_bill_r)
tabla4 <- tibble(Symbol = colnames(monthly_returns), `Retorno referencia` = pct(as.numeric(exp_ret)),
                 `Retorno con 3 opiniones` = pct(as.numeric(ret_4)),
                 `Peso referencia` = pct(as.numeric(bench_prop)), `Peso portafolio ajustado` = pct(as.numeric(port_4$weigths)))
print(tabla4)
cat("Retorno esperado del portafolio con 3 opiniones:", pct(as.double(port_4$portfolio_return)), "\n")

# Punto 5 y Practica IV: confianza gamma = 0.7 en las opiniones ----
gamma <- 0.7
ret_5 <- gamma * ret_4 + (1 - gamma) * exp_ret
port_5 <- CML(ret_5, cov_mat, t_bill_r)
tabla5 <- tibble(Symbol = colnames(monthly_returns), `Retorno referencia` = pct(as.numeric(exp_ret)),
                 `Retorno con 3 opiniones` = pct(as.numeric(ret_4)), `Retorno con confianza 0.7` = pct(as.numeric(ret_5)),
                 `Peso referencia` = pct(as.numeric(bench_prop)), `Peso 3 opiniones` = pct(as.numeric(port_4$weigths)),
                 `Peso con confianza 0.7` = pct(as.numeric(port_5$weigths)))
print(tabla5)
resumen <- c(Referencia = pct(bench_ret), `NVDA +100pb` = pct(as.double(port_2$portfolio_return)),
             `3 opiniones` = pct(as.double(port_4$portfolio_return)), `Confianza 0.7` = pct(as.double(port_5$portfolio_return)))
print(resumen)
