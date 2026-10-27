#' ---
#' title: "Código de la clase: modelo de Black-Litterman"
#' subtitle: "Técnicas de optimización 2026-2 · ejemplo de las diapositivas (10 acciones, Chapt_15_data.csv)"
#' output:
#'   html_document:
#'     toc: true
#'     toc_float: true
#'     code_folding: show
#' ---

##' @author Andres Salas
##' @description
##' Codigo para practica en clase modelo BL.

# Configuracion inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable()) setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, scales, xts, lubridate, TTR, timetk, PerformanceAnalytics, PortfolioAnalytics)
options(scipen = 999)

# 1. Preliminares ----

## 1.1. Carga de los precios ----
monthly_prices <- read_csv(file = "Data/Chapt_15_data.csv",
                           col_types =
                               cols(Date =
                                        col_date(format = "%d-%m-%y"))) %>% 
    timetk::tk_xts(date_var = Date)

## 2.1. Retornos ----
monthly_returns <- ROC(x = monthly_prices, na.pad = F)

head(monthly_returns)

## 1.3. Capitalizacion del mercado y libre de riesgo----

# Market Capitalization ($B)
market_cap <- c(1370, 1358, 1043, 1020, 257.356, 395.024, 312.879, 250.902, 336.433, 250.356) %>% 
`names<-`(colnames(monthly_returns))
market_cap

# Input: Current T-Bill rate
t_bill_r <- 0.016/12

# Proporcion del Benchmark
bench_prop <- market_cap / sum(market_cap)
bench_prop

# 1.4 Funcion para hallar el portafolio que define la CML ----
CML <- function(retornos = NULL, mean_retornos = NULL, var_cov = NULL, rf){
    
    ##'@description: Funcion para hallar los pesos del portafolio que define la linea de mercado de capitales con un retorno fijo dado
    ##'@param retornos Objeto con los retornos de los activos a emplear en el portafolio
    ##'@param rf Tasa libre de riesgo
    ##'@param mean_retornos Objeto con los retornos esperados de los activos a emplear en el portafolio
    ##'@param var_cov Objeto con la matriz de varianzas y covarianzas de los activos a emplear en el portafolio
    ##'@returns Objeto con los pesos del portafolio que define la CML, el retorno esperado del portafolio, el riesgo esperado del portafolio y el ratio de Sharpe
    
    if(!is.null(retornos)){
        mean_ret <- colMeans(retornos)
        cov_mat <- cov(retornos)
    } else{
        mean_ret <- mean_retornos
        cov_mat <- var_cov
    }
    
    optimal_port <- (solve(cov_mat) %*% (mean_ret - rf)) / sum( solve(cov_mat) %*% (mean_ret - rf))
    
    exp_return <- t(optimal_port) %*% mean_ret
    exp_risk <- sqrt(t(optimal_port) %*% cov_mat %*% optimal_port)
    SR <- (exp_return - rf) / exp_risk
    
    return(list(weigths = optimal_port,
                portfolio_return = exp_return,
                portfolio_risk = exp_risk,
                SR = SR))
}

# 2. Optimizacion simple (inocente) CML ----
optimal_port <- CML(retornos = monthly_returns, rf = t_bill_r)
optimal_port[["weigths"]]
optimal_port[["portfolio_return"]]
optimal_port[["portfolio_risk"]]
optimal_port[["SR"]]

# 3. Optimizacion BL ----

## 3.1 Opinión del mercado ----

# Vamos a asumir que el portafolio de referencias es el conformado por la proporción de los acciones del total de la capitalización del mercado.

bench_ret <- 0.01 # retorno anticipado de la referencia 1%
mean_ret <- colMeans(monthly_returns)
sd_ret <- apply(monthly_returns, 2, sd)
cov_mat <- cov(monthly_returns)

### 3.1.1. Calculo de los retornos esperados no normalizados ----
exp_ret_no_norm <- cov_mat %*% (bench_prop + t_bill_r)

# Comparación con el retorno esperado de la referencia
port_ret_no_norm <- bench_prop %*% exp_ret_no_norm

label_percent(accuracy = 0.01)(port_ret_no_norm)
label_percent(accuracy = 0.01)(bench_ret)

### 3.1.2. Calculo de los retornos esperados normalizados ----

BL_bench_expected_returns <- function(bench_ret, bench_prop, rf = 0, retornos = NULL, var_cov = NULL){
    ##'@description: Funcion para calcular los retornos esperados de los activos en el portafolio de referencia de BL
    ##'@param bench_ret Retorno anticipado de referencia
    ##'@param bench_prop Proporciones de los activos en el portafolio de referencia
    ##'@param rf Tasa libre de riesgo
    ##'@param retornos Objeto con los retornos de los activos a emplear en el portafolio
    ##'@param var_cov Objeto con la matriz de varianzas y covarianzas de los activos a emplear en el portafolio
    ##'@returns Objeto con los retornos esperados de los activos en el portafolio de referencia y el factor normalizador
    
    if(!is.null(retornos)){
        cov_mat <- cov(retornos)
    } else{
        cov_mat <- var_cov
    }
    
    # Normalizing Factor
    norm_fact <- (bench_ret - rf) / t(bench_prop %*% cov_mat %*% bench_prop)
    
    # Market Expected Returns
    exp_ret <- cov_mat %*% bench_prop * as.double(norm_fact) + rf
    
    return(list(BL_expected_returns = exp_ret,
                normalize_factor = norm_fact))
}

BL_exp_ret <- BL_bench_expected_returns(bench_ret = bench_ret,
                                        bench_prop = bench_prop,
                                        rf = t_bill_r,
                                        retornos = monthly_returns)

exp_ret <- BL_exp_ret[["BL_expected_returns"]]
NF <- BL_exp_ret[["normalize_factor"]]

# Comparación con el retorno esperado de la referencia
port_ret <- bench_prop %*% exp_ret

label_percent(accuracy = 0.01)(port_ret)
label_percent(accuracy = 0.01)(bench_ret)

### 3.1.3. Un chequeo adicional ----

# Resolver el problema con los retornos esperados iguales a los hallados
CML(mean_retornos = exp_ret, var_cov = cov_mat, rf = t_bill_r)[["weigths"]]

bench_prop %*% exp_ret

# Verificar que la suma es igual a 1
CML(mean_retornos = exp_ret, var_cov = cov_mat, rf = t_bill_r)[["weigths"]] %>% 
    sum()

# 4. Introduciendo opiniones ----

## 4.1. Aumentando el retorno esperado de Alphabet ----
delta_GOOG <- 0.007

## 4.2. Ajuste de los retornos a partir del cambio por opinion ----
opn_adj_ret <- exp_ret + delta_GOOG * cov_mat[,"GOOG"] / cov_mat["GOOG", "GOOG"]

comp_retornos <- data.frame(accion = colnames(monthly_returns),
                            opinion_google = label_percent(accuracy = 0.01)(opn_adj_ret),
                            benchmark = label_percent(accuracy = 0.01)(exp_ret))
print(comp_retornos)

## 4.3. Portafolio optimo ajustado por la opinion ----
opn_adj <- CML(mean_retornos = opn_adj_ret, var_cov = cov_mat, rf = t_bill_r)

comp_pesos <- data.frame(accion = colnames(monthly_returns),
                         opinion_google = label_percent(accuracy = 0.01)(opn_adj[["weigths"]]),
                         benchmark = label_percent(accuracy = 0.01)(bench_prop))
print(comp_pesos)

port_ret_adj <- opn_adj[["portfolio_return"]]

retornos <- c(Benchmark = label_percent(accuracy = 0.01)(bench_ret), 
              Adj_google = label_percent(accuracy = 0.01)(port_ret_adj))
retornos

# 5. Opiniones de varias acciones ----

## 5.1. The Black-Litterman Tracking Matrix ----
track_mat <- t(cov_mat/diag(cov_mat))
print(round(track_mat, 3))

## 5.2. Dos o mas opiniones ----

# Input: Vector de los deltas de las acciones de Alphabet y Disney
delta_port <- c(0.000, 0.000, 0.000, 0.007, 0.000, 0.000, 0.000, -0.004, 0.000, 0.000) %>% 
    `names<-`(colnames(monthly_returns))

## 5.3 Retornos ajustados para los deltas ----
opn_adj_ret_2 <- exp_ret + track_mat %*% delta_port

comp_retornos <- comp_retornos %>% 
    mutate(opinion_dos = label_percent(accuracy = 0.01)(opn_adj_ret_2), .after = 2)
print(comp_retornos)

## 5.4. Portafolio ajustado a las nuevas opiniones ----
opn_adj_2 <- CML(mean_retornos = opn_adj_ret_2, var_cov = cov_mat, rf = t_bill_r)

comp_pesos <- comp_pesos %>% 
    mutate(opinion_dos = label_percent(accuracy = 0.01)(opn_adj_2[["weigths"]]), .after = 2)
print(comp_pesos)

retornos <- c(retornos,
              Adj_dos = label_percent(accuracy = 0.01)(opn_adj_2[["portfolio_return"]]))
retornos

# 6. Portafolio a partir de la confianza de las opiniones ----

## 6.1. Definiendo el nivel de confianza ----
conf <- 0.6

## 6.2. Retornos ajustados entre la referencia del mercado y la opinion
conf_adj_ret <- conf * opn_adj_ret_2 + ( 1 - conf ) * exp_ret

comp_retornos <- comp_retornos %>% 
    mutate(confianza = label_percent(accuracy = 0.01)(conf_adj_ret))
print(comp_retornos)

## 6.3. Optimizacion para retornos ajustados por confianza
conf_adj <- CML(mean_retornos = conf_adj_ret, var_cov = cov_mat, rf = t_bill_r)

comp_pesos <- comp_pesos %>% 
    mutate(confianza = label_percent(accuracy = 0.01)(conf_adj[["weigths"]]))
print(comp_pesos)

retornos <- c(retornos,
              conf = label_percent(accuracy = 0.01)(conf_adj[["portfolio_return"]]))
retornos
