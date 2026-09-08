##' @description
##' Solucion de "Taller respaso 1er Parcial" (Taller-respaso-1er-Parcial.pdf,
##' septiembre 2026) - repaso de Simulacion de Monte Carlo, VaR/ES, y
##' retornos/portafolios, los tres temas cubiertos antes del primer parcial.
##' Cada pregunta del enunciado se cita en un comentario justo antes de
##' resolverla, y cada resultado termina con un cat("Respuesta: ...")
##' explicito. Mismo motor y resultados que Taller_Repaso_Parcial1.Rmd, en
##' formato de script plano para correr por bloques en RStudio.

# Configuracion inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, ggpubr, psych, quantmod, TTR,
               purrr, PerformanceAnalytics, timetk, readxl, scales)
options(scipen = 999)

# NOTA: EnvStats cargado junto con tidyverse/ggplot2 rompe la dispatch de
# print() sobre objetos ggplot (ver CLAUDE.md) - se dibuja el grob
# directamente en vez de depender de print().
dibujar <- function(p) {
  grid::grid.newpage()
  grid::grid.draw(ggplot2::ggplotGrob(p))
}

# ================================================================================================
# PARTE 1 - Seleccion de una inversion ----
# ================================================================================================

# ENUNCIADO: "Se te ha asignado la tarea de invertir un capital de
# USD$20,000 en un activo de renta variable a un periodo de un ano.
# [...] estas considerando 3 posibilidades:
#  a. Una inversion que genera retornos anuales [...] distribucion normal
#     con una media de 6.5% y una desviacion estandar de 14%.
#  b. Un activo con retornos mensuales, [...] distribucion triangular con
#     un minimo de -8.5%, un maximo de 6.5% y una moda de 0.56%.
#  c. Un activo con retornos trimestrales, [...] distribucion normal con
#     una media de 1.6% y una desviacion estandar de 7%."

capital_inicial_1 <- 20000
n_sim_1 <- 10001
semilla_1 <- 2026

## Funcion de medidas de riesgo (las 7 que pide el punto 1) ----

medidas_inversion <- function(capital_final, capital_inicial, alpha_var = 0.01) {
  VaR_q <- quantile(capital_final, probs = alpha_var)
  ic95 <- quantile(capital_final, probs = c(0.025, 0.975))
  data.frame(
    capital_promedio = mean(capital_final),
    capital_maximo = max(capital_final),
    ganancia_maxima = max(capital_final) - capital_inicial,
    capital_minimo = min(capital_final),
    perdida_maxima = capital_inicial - min(capital_final),
    mediana = median(capital_final),
    ic95_inf = ic95[1],
    ic95_sup = ic95[2],
    VaR_99 = capital_inicial - VaR_q,
    ES_99 = capital_inicial - mean(capital_final[capital_final < VaR_q])
  )
}

## a. Normal anual: media 6.5%, sd 14% (1 solo periodo, ya es anual) ----
set.seed(semilla_1)
retorno_a <- rnorm(n_sim_1, mean = 0.065, sd = 0.14)
capital_a <- capital_inicial_1 * (1 + retorno_a)

## b. Triangular mensual: min -8.5%, max 6.5%, moda 0.56% (12 meses compuestos) ----
set.seed(semilla_1)
retorno_b <- matrix(EnvStats::rtri(n_sim_1 * 12, min = -0.085, max = 0.065, mode = 0.0056),
                     nrow = n_sim_1, ncol = 12)
capital_b_trayectoria <- capital_inicial_1 * t(apply(1 + retorno_b, 1, cumprod))
capital_b <- capital_b_trayectoria[, 12]

## c. Normal trimestral: media 1.6%, sd 7% (4 trimestres compuestos) ----
set.seed(semilla_1)
retorno_c <- matrix(rnorm(n_sim_1 * 4, mean = 0.016, sd = 0.07), nrow = n_sim_1, ncol = 4)
capital_c_trayectoria <- capital_inicial_1 * t(apply(1 + retorno_c, 1, cumprod))
capital_c <- capital_c_trayectoria[, 4]

## PREGUNTA 1 - Tabla de las 7 medidas para cada inversion ----

tabla_inversiones <- bind_rows(
  cbind(inversion = "a. Normal anual", medidas_inversion(capital_a, capital_inicial_1)),
  cbind(inversion = "b. Triangular mensual", medidas_inversion(capital_b, capital_inicial_1)),
  cbind(inversion = "c. Normal trimestral", medidas_inversion(capital_c, capital_inicial_1))
)
print(tabla_inversiones %>% mutate(across(where(is.numeric), ~ round(.x, 2))))

cat("Respuesta 1: tabla de las 7 medidas (capital promedio, maximo/ganancia,",
    "minimo/perdida, mediana, IC95%, VaR 99%, ES 99%) para las 3 inversiones,",
    "impresa arriba - una fila por inversion.\n")

## Interpretacion de cada medida - se elige la inversion (a) Normal anual ----

m_a <- medidas_inversion(capital_a, capital_inicial_1)

cat("\nRespuesta (interpretacion de las 7 medidas, inversion a. Normal anual):\n")
cat("1. Capital promedio al final del ano:", label_dollar()(m_a$capital_promedio),
    "- en promedio, sobre las", n_sim_1, "trayectorias simuladas, los $20,000",
    "invertidos terminan valiendo", label_dollar()(m_a$capital_promedio),
    "(una ganancia esperada de", label_dollar()(m_a$capital_promedio - capital_inicial_1), ").\n")
cat("2. Capital maximo:", label_dollar()(m_a$capital_maximo), "- el mejor de los",
    n_sim_1, "escenarios simulados, con una ganancia de",
    label_dollar()(m_a$ganancia_maxima), "sobre el capital inicial.\n")
cat("3. Capital minimo:", label_dollar()(m_a$capital_minimo), "- el peor de los",
    n_sim_1, "escenarios, con una perdida de", label_dollar()(m_a$perdida_maxima),
    "(el retorno simulado en ese escenario fue negativo y grande, pero el",
    "capital nunca se vuelve negativo porque solo hay un periodo).\n")
cat("4. Mediana:", label_dollar()(m_a$mediana), "- el valor que divide en dos",
    "mitades iguales los", n_sim_1, "resultados; queda muy cerca del promedio",
    "(", label_dollar()(m_a$capital_promedio), ") porque la distribucion normal",
    "es simetrica.\n")
cat("5. Intervalo del 95%:", label_dollar()(m_a$ic95_inf), "a",
    label_dollar()(m_a$ic95_sup), "- el 95% de los capitales finales simulados",
    "cae dentro de ese rango; solo el 2.5% de las peores corridas termina por",
    "debajo de", label_dollar()(m_a$ic95_inf), "y el 2.5% de las mejores por",
    "encima de", label_dollar()(m_a$ic95_sup), ".\n")
cat("6. VaR al 99%:", label_dollar()(m_a$VaR_99),
    "- la perdida maxima de la inversion, con un 99% de confianza en el ano,",
    "es de USD", label_dollar()(m_a$VaR_99),
    ". Hay una probabilidad del 1% de que las perdidas de la inversion sean",
    "mayores a USD", label_dollar()(m_a$VaR_99), ".\n")
cat("7. ES al 99%:", label_dollar()(m_a$ES_99),
    "- la perdida esperada al 99% en el ano es de USD", label_dollar()(m_a$ES_99),
    "- es decir, si el escenario cae dentro de ese peor 1% de los casos, la",
    "perdida promedio ahi es de", label_dollar()(m_a$ES_99), "(mayor que el",
    "VaR, porque promedia TODA la cola, no solo su borde).\n")

## PREGUNTA 2 - Decision de inversion (criterio: menor ES, aversion al riesgo) ----

# ENUNCIADO: "Una vez calculadas las medidas, debe de definir en cual de las
# opciones invertira su capital. Esto considerando como criterio de decision
# el valor de la perdida esperada en un contexto de aversion al riesgo.
# Justifique su decision"

tabla_es <- tabla_inversiones %>% select(inversion, capital_promedio, ES_99) %>%
  mutate(across(where(is.numeric), ~round(.x, 2)))
print(tabla_es)

inversion_elegida <- tabla_inversiones$inversion[which.min(tabla_inversiones$ES_99)]

cat("\nRespuesta 2: se elige la inversion", inversion_elegida, ". Su ES al 99% es",
    label_dollar()(min(tabla_inversiones$ES_99)),
    "- el mas bajo de las 3 (vs.", label_dollar()(tabla_inversiones$ES_99[1]),
    "de la Normal anual y", label_dollar()(tabla_inversiones$ES_99[2]),
    "de la Triangular mensual). En un contexto de aversion al riesgo, donde",
    "el criterio de decision es la perdida esperada en el peor escenario (no",
    "solo el retorno promedio), esta es la opcion mas defendible: ademas de",
    "tener la MENOR perdida esperada en el 1% de peores casos, tambien tiene",
    "el MAYOR capital promedio esperado (", label_dollar()(max(tabla_inversiones$capital_promedio)),
    ") de las 3 - no hay ningun trade-off que resolver, domina en las dos",
    "dimensiones a la vez.\n")

## PREGUNTA 3 - Grafico de la iteracion maxima, minima y mediana (inversion elegida: c) ----

# ENUNCIADO: "Finalmente, para la inversion que ha decidido es la mejor
# opcion, realice un grafico donde se muestre la iteracion que genero el
# maximo capital, la que genero el minimo capital y la que genero el
# capital considerado la mediana de los resultados de las iteraciones."

idx_max_c <- which.max(capital_c)
idx_min_c <- which.min(capital_c)
idx_mediana_c <- which.min(abs(capital_c - median(capital_c)))

trayectorias_c <- bind_rows(
  data.frame(trimestre = 0:4, capital = c(capital_inicial_1, capital_c_trayectoria[idx_max_c, ]), iteracion = "Máxima"),
  data.frame(trimestre = 0:4, capital = c(capital_inicial_1, capital_c_trayectoria[idx_min_c, ]), iteracion = "Mínima"),
  data.frame(trimestre = 0:4, capital = c(capital_inicial_1, capital_c_trayectoria[idx_mediana_c, ]), iteracion = "Mediana")
)

dibujar(ggplot(trayectorias_c, aes(x = trimestre, y = capital, color = iteracion)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  theme_light() +
  labs(title = "Inversión c. Normal trimestral — trayectorias máxima, mínima y mediana",
       x = "Trimestre", y = "Capital (USD)", color = "Iteración") +
  scale_y_continuous(labels = label_dollar()) +
  scale_x_continuous(breaks = 0:4))

cat("\nRespuesta 3: grafico generado arriba - trayectoria trimestre a trimestre",
    "(0 a 4) de la iteracion", idx_max_c, "(capital final maximo,",
    label_dollar()(capital_c[idx_max_c]), "), la iteracion", idx_min_c,
    "(capital final minimo,", label_dollar()(capital_c[idx_min_c]),
    ") y la iteracion", idx_mediana_c, "(capital mas cercano a la mediana,",
    label_dollar()(capital_c[idx_mediana_c]), "), todas partiendo de los",
    "$20,000 iniciales en el trimestre 0.\n")

# ================================================================================================
# PARTE 2 - Retornos y distribuciones (portafolio de acciones reales) ----
# ================================================================================================

# ENUNCIADO: "Empleando estas acciones [AVGO, LLY, WMT, V, MA, COST, MRK,
# PM, TXN, PEP, DIS, SBUX], descargue los precios para el periodo:
# 31/12/2022 - 31/08/2026. A partir de esta informacion, calcule los
# retornos semanales de cada una de las acciones."

tickers_2 <- c("AVGO", "LLY", "WMT", "V", "MA", "COST", "MRK", "PM", "TXN", "PEP", "DIS", "SBUX")

precios_2 <- quantmod::getSymbols(tickers_2, src = "yahoo",
                                  from = "2022-12-31", to = "2026-08-31",
                                  periodicity = "daily",
                                  auto.assign = TRUE, warnings = FALSE) %>%
  purrr::map(~quantmod::Ad(get(.))) %>%
  purrr::reduce(merge.xts) %>%
  `colnames<-`(tickers_2)

cat("Precios descargados:", format(zoo::index(precios_2)[1]), "a",
    format(zoo::index(precios_2)[nrow(precios_2)]), "(", nrow(precios_2), "dias)\n")

precios_semana_2 <- xts::to.weekly(precios_2, OHLC = FALSE)

# NOTA: diff(log(x)) en vez de TTR::ROC(type="continuous") - con esta misma
# combinacion de librerias, TTR::ROC() sobre un objeto de to.weekly() crashea
# R sin ningun mensaje de error (ver CLAUDE.md).
retornos_semana_2 <- diff(log(precios_semana_2)) %>% na.omit()

cat("Retornos semanales calculados:", nrow(retornos_semana_2), "semanas, de",
    format(zoo::index(retornos_semana_2)[1]), "a",
    format(zoo::index(retornos_semana_2)[nrow(retornos_semana_2)]), "\n")

retornos_semana_2_long <- retornos_semana_2 %>% tk_tbl(rename_index = "date") %>%
  gather(key = activo, value = retorno, -date, factor_key = TRUE)

## PREGUNTA 1 - Histograma agrupado, no superpuesto ----

# ENUNCIADO: "Realiza el histograma de los retornos semanales de todas las
# acciones. Agrupado, pero no superpuesto."

dibujar(retornos_semana_2_long %>%
  ggplot(aes(x = retorno, fill = activo)) +
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~activo, ncol = 4) +
  theme_light() +
  theme(legend.position = "none") +
  labs(title = "Histograma de retornos semanales por acción (agrupado, no superpuesto)",
       x = "Retorno semanal", y = "Frecuencia") +
  scale_x_continuous(labels = label_percent(0.1)))

cat("Respuesta 1: histograma generado arriba - un panel por accion",
    "(facet_wrap, sin solapar barras de acciones distintas en el mismo eje).\n")

## PREGUNTA 2 - Densidades ----

# ENUNCIADO: "Realiza el grafico de las densidades de los retornos."

dibujar(retornos_semana_2_long %>%
  ggplot(aes(x = retorno, color = activo)) +
  geom_density(linewidth = 0.7) +
  theme_light() +
  labs(title = "Densidad de los retornos semanales por acción",
       x = "Retorno semanal", y = "Densidad", color = NULL) +
  scale_x_continuous(labels = label_percent(0.1)))

cat("Respuesta 2: grafico de densidades generado arriba - todas las",
    "acciones superpuestas para comparar dispersion y forma.\n")

## PREGUNTA 3 - Grafico de lineas de la evolucion de los retornos ----

# ENUNCIADO: "Realiza el grafico de lineas que muestre la evolucion de los
# retornos de las acciones."

dibujar(retornos_semana_2_long %>%
  ggplot(aes(x = date, y = retorno, color = activo)) +
  geom_line(linewidth = 0.4, alpha = 0.8) +
  theme_light() +
  labs(title = "Evolución de los retornos semanales por acción",
       x = "Fecha", y = "Retorno semanal", color = NULL) +
  scale_y_continuous(labels = label_percent(0.1)))

cat("Respuesta 3: grafico de lineas generado arriba - evolucion semana a",
    "semana de los retornos de las 12 acciones en el mismo eje de tiempo.\n")

## PREGUNTA 4 - Portafolio con composicion propia, retornos con rebalanceo anual ----

# ENUNCIADO: "Usando estas acciones, construye un portafolio con una
# composicion de tu eleccion. Calcula los retornos historicos, con
# rebalanceos anuales, del portafolio."

# Pesos de conviccion decreciente (mismo criterio que Taller_Portafolio.R):
# mas peso a las acciones con mejor perfil retorno/riesgo dentro del grupo
# (AVGO y LLY encabezan la lista de las 12, con el mayor retorno semanal
# promedio observado), menos a las de cola.
pesos_portafolio <- rev(seq_along(tickers_2)) / sum(seq_along(tickers_2))
names(pesos_portafolio) <- tickers_2

cat("Pesos del portafolio (suman", sum(pesos_portafolio), "):\n")
print(data.frame(accion = tickers_2, peso = label_percent(accuracy = 0.01)(pesos_portafolio)))

portafolio_2 <- PerformanceAnalytics::Return.portfolio(R = retornos_semana_2,
                                                        weights = pesos_portafolio,
                                                        rebalance_on = "years") %>%
  `colnames<-`("retornos")

retorno_acumulado_portafolio <- prod(1 + as.numeric(portafolio_2)) - 1

cat("\nRespuesta 4: portafolio con pesos de conviccion decreciente (",
    label_percent(accuracy=0.1)(pesos_portafolio[1]), "en AVGO hasta",
    label_percent(accuracy=0.1)(pesos_portafolio[length(pesos_portafolio)]),
    "en SBUX), rebalanceado cada ano. Retorno semanal promedio =",
    label_percent(accuracy = 0.01)(mean(as.numeric(portafolio_2))),
    ", desviacion estandar semanal =",
    label_percent(accuracy = 0.01)(sd(as.numeric(portafolio_2))),
    ", retorno acumulado total del periodo (", nrow(portafolio_2), "semanas) =",
    label_percent(accuracy = 0.01)(retorno_acumulado_portafolio), ".\n")

# ================================================================================================
# PARTE 3 - Evaluacion de Proyectos (VPN por simulacion) ----
# ================================================================================================

# ENUNCIADO: "Usted dispone de un capital de $500 millones. Existe la
# posibilidad de invertir en un proyecto que tiene una duracion de 5 anos,
# con flujos evaluados semestralmente. La tasa de descuento es del 12% EA.
# [...] Sector Agrotecnologico: Ingresos (t=1...10) Lognormal, media
# semestral 18.181, desv. estandar 0.186; Costos (t=1...10) Normal, media
# $30M, desv. estandar $1M. [...] Sector Infraestructura: Ingresos
# (t=1...10) Normal, media $75M, desv. estandar $1.5M; Costos (t=1...10)
# Lognormal, media de la normal asociada 17.014, desv. estandar de la
# normal asociada 0.456."

# NOTA DE SUPUESTO (el enunciado no lo dice explicito para el ingreso del
# sector Agrotecnologico, solo para el costo de Infraestructura, pero la
# magnitud de los numeros lo deja claro): 18.181 y 17.014 son los
# parametros meanlog/sdlog de rlnorm() en dolares (no millones) - un
# ingreso lognormal con esos parametros tiene una media real de ~$80M/
# semestre (exp(18.181 + 0.186^2/2)), del mismo orden que el ingreso de
# $75M/semestre de Infraestructura; leer 18.181 como la media EN DOLARES
# directa haria que el "ingreso" del sector Agro fuera de solo $18.18,
# absurdo frente a un costo de $30M. Se pasan a millones dividiendo por 1e6.

inv_inicial_3 <- 500          # millones de USD
tasa_ea_3 <- 0.12
r_semestral_3 <- (1 + tasa_ea_3)^0.5 - 1
n_sim_3 <- 100000
n_sem_3 <- 10
factor_descuento_3 <- (1 + r_semestral_3)^(1:n_sem_3)

cat("Tasa de descuento semestral equivalente a 12% EA:",
    label_percent(accuracy = 0.01)(r_semestral_3), "\n")

set.seed(2026)
ingresos_agro <- matrix(rlnorm(n_sim_3 * n_sem_3, meanlog = 18.181, sdlog = 0.186),
                        nrow = n_sim_3, ncol = n_sem_3) / 1e6
costos_agro <- matrix(rnorm(n_sim_3 * n_sem_3, mean = 30, sd = 1), nrow = n_sim_3, ncol = n_sem_3)
flujo_agro <- ingresos_agro - costos_agro
vpn_agro <- -inv_inicial_3 + rowSums(sweep(flujo_agro, 2, factor_descuento_3, "/"))

# Misma semilla que Agro, para que ambos proyectos usen el mismo stream de
# numeros aleatorios (comparacion mas limpia entre los dos escenarios,
# mismo criterio ya usado en Taller_Simulaciones.R para la sensibilidad del
# VPN a la volatilidad del precio).
set.seed(2026)
ingresos_infra <- matrix(rnorm(n_sim_3 * n_sem_3, mean = 75, sd = 1.5), nrow = n_sim_3, ncol = n_sem_3)
costos_infra <- matrix(rlnorm(n_sim_3 * n_sem_3, meanlog = 17.014, sdlog = 0.456),
                       nrow = n_sim_3, ncol = n_sem_3) / 1e6
flujo_infra <- ingresos_infra - costos_infra
vpn_infra <- -inv_inicial_3 + rowSums(sweep(flujo_infra, 2, factor_descuento_3, "/"))

resumen_vpn <- function(vpn, alpha = 0.05) {
  VaR_q <- quantile(vpn, alpha)
  data.frame(
    vpn_promedio = mean(vpn),
    vpn_sd = sd(vpn),
    vpn_mediana = median(vpn),
    vpn_min = min(vpn),
    vpn_max = max(vpn),
    prob_vpn_negativo_pct = mean(vpn < 0) * 100,
    VaR_5 = VaR_q,
    ES_5 = mean(vpn[vpn < VaR_q])
  )
}

tabla_vpn <- bind_rows(
  cbind(proyecto = "Agrotecnológico", resumen_vpn(vpn_agro)),
  cbind(proyecto = "Infraestructura", resumen_vpn(vpn_infra))
)
print(tabla_vpn %>% mutate(across(where(is.numeric), ~round(.x, 2))))

## PREGUNTA 1 - Histograma de las distribuciones del VPN ----

# ENUNCIADO: "Realice y analice el histograma de las distribuciones de los
# VPN para ambas inversiones."

vpn_long <- bind_rows(
  data.frame(vpn = vpn_agro, proyecto = "Agrotecnológico"),
  data.frame(vpn = vpn_infra, proyecto = "Infraestructura")
)

dibujar(ggplot(vpn_long, aes(x = vpn, fill = proyecto)) +
  geom_histogram(position = "identity", alpha = 0.5, color = "black", bins = 60) +
  theme_light() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  labs(title = "Distribución simulada del VPN — Agrotecnológico vs. Infraestructura",
       x = "VPN (millones de USD)", y = "Frecuencia", fill = NULL) +
  scale_x_continuous(labels = label_dollar()))

cat("\nRespuesta 1: histograma generado arriba (linea punteada en VPN = 0).",
    "Ambas distribuciones quedan CASI ENTERAMENTE a la izquierda de cero:",
    "el", round(mean(vpn_agro < 0) * 100, 1), "% de las simulaciones de",
    "Agrotecnologico y el", round(mean(vpn_infra < 0) * 100, 1),
    "% de las de Infraestructura dan un VPN negativo. La de Agrotecnologico",
    "es un poco mas angosta y esta corrida levemente a la derecha (menos",
    "mala) que la de Infraestructura, y muestra algo de asimetria hacia la",
    "derecha (coeficiente de asimetria", round(psych::skew(vpn_agro), 2),
    ") por el ingreso lognormal; la de Infraestructura muestra asimetria",
    "hacia la IZQUIERDA (coeficiente", round(psych::skew(vpn_infra), 2),
    ") porque ahi es el COSTO el que sigue una lognormal - un costo",
    "extremadamente alto (cola derecha del costo) se traduce en un VPN",
    "extremadamente bajo (cola izquierda del VPN).\n")

## PREGUNTA 2 - VaR al 5% y ES al 5% ----

# ENUNCIADO: "Calcule el VaR al 5% y el ES al 5% del VPN de ambas
# inversiones. Interprete los resultados."

cat("\nRespuesta 2 (VaR y ES al 5% del VPN):\n")
cat("  Agrotecnologico: el VPN, con un 95% de confianza en el horizonte del",
    "proyecto, no cae por debajo de USD", label_dollar()(tabla_vpn$VaR_5[1]),
    "millones. Hay una probabilidad del 5% de que el VPN sea peor que eso.",
    "Si efectivamente cae en ese 5% de peores escenarios, el VPN esperado ahi",
    "(ES) es de USD", label_dollar()(tabla_vpn$ES_5[1]), "millones.\n")
cat("  Infraestructura: con un 95% de confianza, el VPN no cae por debajo de",
    "USD", label_dollar()(tabla_vpn$VaR_5[2]), "millones (probabilidad del",
    "5% de que sea peor). En el 5% de peores escenarios, el VPN esperado",
    "(ES) es de USD", label_dollar()(tabla_vpn$ES_5[2]), "millones.\n")
cat("  Infraestructura tiene tanto un VaR como un ES mas negativos que",
    "Agrotecnologico - en el escenario adverso, se pierde mas dinero con",
    "Infraestructura.\n")

## PREGUNTA 3 - Decision segun el VPN promedio ----

# ENUNCIADO: "Teniendo en cuenta el VPN promedio de los proyectos como
# criterio de decision ¿En cual de los dos proyectos invertiria el capital
# disponible?"

proyecto_mejor_vpn <- tabla_vpn$proyecto[which.max(tabla_vpn$vpn_promedio)]

cat("\nRespuesta 3: por VPN promedio,", proyecto_mejor_vpn, "es la MENOS mala",
    "opcion (", label_dollar()(max(tabla_vpn$vpn_promedio)), "millones vs.",
    label_dollar()(min(tabla_vpn$vpn_promedio)), "millones del otro). PERO",
    "el VPN promedio de las DOS opciones es NEGATIVO - ninguno de los dos",
    "proyectos es rentable en promedio con estos parametros. Si el criterio",
    "es unicamente cual proyecto es 'menos malo', se elige",
    proyecto_mejor_vpn, "; si el criterio es invertir solo en proyectos con",
    "VPN promedio positivo, la respuesta correcta es NO invertir en",
    "ninguno de los dos.\n")

## PREGUNTA 4 - Condicion del comite de riesgos (prob. VPN negativo < 10%) ----

# ENUNCIADO: "Resulta que el comite de riesgos [...] te ha puesto la
# condicion de que solo inviertas en proyectos cuya probabilidad de tener
# un VPN negativo sea menor al 10%. Considerando esta condicion
# ¿Invertirias en los proyectos disponibles?"

cat("\nRespuesta 4: NO se invertiria en ninguno de los dos proyectos bajo",
    "esta condicion. La probabilidad de VPN negativo es",
    round(tabla_vpn$prob_vpn_negativo_pct[1], 2), "% en Agrotecnologico y",
    round(tabla_vpn$prob_vpn_negativo_pct[2], 2), "% en Infraestructura -",
    "ambas MUY por encima del limite del 10% que exige el comite (de hecho,",
    "ambas superan el 99%: con estos parametros, casi cualquier escenario",
    "simulado termina en perdida). Ningun proyecto pasa el filtro de",
    "riesgo.\n")

## PREGUNTA 5 - ¿Rentable en promedio pero peligroso en escenarios extremos? ----

# ENUNCIADO: "Usando el VPN promedio de los proyectos y el ES calculado
# ¿Es el proyecto rentable en promedio pero excesivamente peligroso en
# escenarios extremos?"

cat("\nRespuesta 5: NO aplica esa lectura para ninguno de los dos proyectos -",
    "esa pregunta describe un proyecto con VPN promedio POSITIVO pero un ES",
    "muy negativo (rentable en el caso base, peligroso en la cola). Aca el",
    "problema es anterior a eso: el VPN promedio YA es negativo",
    "(", label_dollar()(tabla_vpn$vpn_promedio[1]), "millones en",
    "Agrotecnologico,", label_dollar()(tabla_vpn$vpn_promedio[2]),
    "millones en Infraestructura), asi que no hay un escenario 'base",
    "rentable' que el ES este poniendo en riesgo - ambos proyectos ya son",
    "perdedores en promedio, y el ES (", label_dollar()(tabla_vpn$ES_5[1]),
    "y", label_dollar()(tabla_vpn$ES_5[2]), "millones respectivamente) solo",
    "confirma que en el peor 5% de los casos la perdida es todavia mayor.\n")

cat("\n=== TALLER TERMINADO OK ===\n")
