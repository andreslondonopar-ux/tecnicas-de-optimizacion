##' @description
##' Solucion del Taller VaR y ES (Taller-en-clase-MC.pdf, agosto 2026).
##' Cada pregunta del enunciado se cita en un comentario justo antes de
##' resolverla, y cada resultado termina con un cat("Respuesta: ...") explicito.
##' Mismo motor y resultados que VaR_ES_taller.Rmd, en formato de script
##' plano para correr por bloques en RStudio (Ctrl+Enter linea por linea, o
##' seleccionar cada seccion "----" y correrla completa).

# Configuracion inicial -----------------------------------------------------------------------
if (!require("rstudioapi")) install.packages("rstudioapi")
if (rstudioapi::isAvailable() && nzchar(rstudioapi::getActiveDocumentContext()$path)) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, EnvStats, scales, quantmod, xts)
options(scipen = 999)

# ============================================================================================
# EJERCICIO 1 - Simulacion inversion en Dow Inc. ----
# ============================================================================================

# ENUNCIADO: "Un amigo esta buscando oportunidades de inversion en algun
# activo de renta variable. No esta dispuesto a invertir en empresas
# tecnologicas, por lo que ha decidido centrar su atencion en Dow Inc.,
# (DOW). [...] El tiene planeado invertir un total de USD $10,000. [...]
# Usar los retornos semanales de la accion de Dow Inc. desde el 2023-08-01
# hasta el 2026-08-13 [...] Cada una de las simulaciones debe de tener
# 10,000 iteraciones. Las simulaciones deben de iniciar con una semilla de
# valor 8915. El ano tiene un total de 52 semanas."

## Datos: retornos semanales de Dow Inc. (DOW) ----

descargar_retornos_semanales <- function(ticker) {
  precio <- quantmod::getSymbols(ticker, src = "yahoo",
                                  from = "2023-08-01", to = "2026-08-13",
                                  periodicity = "weekly",
                                  auto.assign = FALSE, warnings = FALSE)
  ajustado <- quantmod::Ad(precio)
  # diff(log(x)) en vez de TTR::ROC - bug de indexacion ya documentado en CLAUDE.md
  na.omit(diff(log(ajustado)))
}

dow_retornos <- descargar_retornos_semanales("DOW")

media_dow <- mean(dow_retornos)
sd_dow <- sd(dow_retornos)
min_dow <- min(dow_retornos)
max_dow <- max(dow_retornos)

print(data.frame(
  accion = "DOW", n_semanas_historia = nrow(dow_retornos),
  retorno_semanal_promedio_pct = media_dow * 100,
  desv_estandar_pct = sd_dow * 100,
  retorno_min_pct = min_dow * 100,
  retorno_max_pct = max_dow * 100
))

## Funciones de simulacion y de riesgo ----

# simular_capital(): matriz de retornos semanales aleatorios (10,000 filas x
# 52 columnas, una por escenario) capitalizada por producto acumulado por
# fila - el mismo enfoque vectorizado ya usado en Taller_Simulaciones.R para
# 100,000 iteraciones, mucho mas rapido que llamar simulacion_activo() fila
# por fila. La triangular usa el retorno medio observado como aproximacion
# de la moda (misma convencion ya usada en el taller de Simulaciones).
# valor_riesgo(): calcula el VaR (percentil alpha del capital final) y el ES
# (promedio del capital final por debajo de ese percentil), ambos expresados
# como perdida en dolares.

simular_capital <- function(capital_inicial, tipo, n_sim, n_periodos, media,
                             sd = NULL, minimo = NULL, maximo = NULL, moda = NULL, seed) {
  set.seed(seed)
  if (tipo == "normal") {
    retornos <- matrix(rnorm(n_sim * n_periodos, mean = media, sd = sd), nrow = n_sim, ncol = n_periodos)
  } else {
    retornos <- matrix(EnvStats::rtri(n_sim * n_periodos, min = minimo, max = maximo, mode = moda),
                        nrow = n_sim, ncol = n_periodos)
  }
  capital_inicial * apply(1 + retornos, 1, prod)
}

valor_riesgo <- function(capital_final, capital_inicial, alpha) {
  VaR_capital <- quantile(capital_final, probs = alpha)
  ES_capital <- mean(capital_final[capital_final < VaR_capital])
  data.frame(
    valor_esperado = mean(capital_final),
    valor_maximo = max(capital_final),
    ganancia_maxima = max(capital_final) - capital_inicial,
    valor_minimo = min(capital_final),
    perdida_maxima_obs = capital_inicial - min(capital_final),
    VaR = capital_inicial - VaR_capital,
    ES = capital_inicial - mean(capital_final[capital_final < VaR_capital])
  )
}

## Simulaciones (10,000 iteraciones, semilla 8915) ----

capital_inicial <- 10000
n_semanas <- 52
n_sim <- 10000
semilla <- 8915
alpha <- 0.01  # 99% de confianza

final_dow_normal <- simular_capital(capital_inicial, "normal", n_sim, n_semanas,
                                     media = media_dow, sd = sd_dow, seed = semilla)
final_dow_tri <- simular_capital(capital_inicial, "triangular", n_sim, n_semanas,
                                  media = media_dow, minimo = min_dow, maximo = max_dow,
                                  moda = media_dow, seed = semilla)

tabla_dow <- bind_rows(
  cbind(distribucion = "Normal", valor_riesgo(final_dow_normal, capital_inicial, alpha)),
  cbind(distribucion = "Triangular", valor_riesgo(final_dow_tri, capital_inicial, alpha))
) %>% mutate(across(where(is.numeric), ~ label_dollar(accuracy = 0.01)(.x)))

print(tabla_dow)

## Preguntas - Dow Inc. ----

# PREGUNTA 1: "Cual es el valor esperado de la inversion al final del ano?"
cat("Respuesta 1: con retornos normales, el valor esperado es",
    label_dollar()(mean(final_dow_normal)),
    "; con retornos triangulares,", label_dollar()(mean(final_dow_tri)),
    "- ambos por debajo del capital inicial de $10,000, porque DOW tuvo un",
    "retorno semanal promedio negativo (", label_percent(accuracy = 0.01)(media_dow),
    ") en el periodo historico usado.\n")

# PREGUNTA 2: "Cual es el valor maximo de la inversion al final del ano?
# Cuanto representa en ganancias?"
cat("Respuesta 2: el maximo con la normal es", label_dollar()(max(final_dow_normal)),
    "(", label_dollar()(max(final_dow_normal) - capital_inicial), "de ganancia); con la",
    "triangular,", label_dollar()(max(final_dow_tri)),
    "(", label_dollar()(max(final_dow_tri) - capital_inicial), "de ganancia).\n")

# PREGUNTA 3: "Cual es el valor minimo de la inversion al final del ano?
# Cuanto representa en perdidas?"
cat("Respuesta 3: el minimo con la normal es", label_dollar()(min(final_dow_normal)),
    "(", label_dollar()(capital_inicial - min(final_dow_normal)), "de perdida); con la",
    "triangular,", label_dollar()(min(final_dow_tri)),
    "(", label_dollar()(capital_inicial - min(final_dow_tri)),
    "de perdida) - casi la totalidad del capital, el peor de los 10,000 escenarios.\n")

# PREGUNTA 4: "Cual es la maxima perdida posible con una confianza del 99%?"
# -> Esto es el VaR al 99%.
cat("Respuesta 4 (VaR al 99%): con la normal, la perdida maxima al 99% de confianza es",
    label_dollar()(capital_inicial - quantile(final_dow_normal, alpha)),
    "; con la triangular,", label_dollar()(capital_inicial - quantile(final_dow_tri, alpha)), "\n")

# PREGUNTA 5: "Cuanto es el valor esperado a perder de la inversion en el
# 1% de los casos?" -> Esto es el ES al 99%.
cat("Respuesta 5 (ES al 99%): con la normal,",
    label_dollar()(capital_inicial - mean(final_dow_normal[final_dow_normal < quantile(final_dow_normal, alpha)])),
    "; con la triangular,",
    label_dollar()(capital_inicial - mean(final_dow_tri[final_dow_tri < quantile(final_dow_tri, alpha)])),
    "- en ambos casos mas severo que el VaR de la pregunta anterior, como debe ser.\n")

# PREGUNTA 6: "Cual distribucion genera mejores resultados, considerando
# una politica de aversion al riesgo?"
cat("Respuesta 6: la normal domina en las dos dimensiones que le importan a alguien",
    "averso al riesgo - valor esperado mas alto y VaR/ES mas bajos (menos perdida",
    "potencial). No hay disyuntiva entre retorno y riesgo aca: la normal es",
    "simplemente mejor en ambos frentes para DOW en este periodo.\n")

## Repeticion con Eli Lilly and Company (LLY) - pregunta 7 ----

# PREGUNTA 7: "Repite el ejercicio usando la accion de Eli Lilly and
# Company, (LLY)."

lly_retornos <- descargar_retornos_semanales("LLY")

media_lly <- mean(lly_retornos)
sd_lly <- sd(lly_retornos)
min_lly <- min(lly_retornos)
max_lly <- max(lly_retornos)

final_lly_normal <- simular_capital(capital_inicial, "normal", n_sim, n_semanas,
                                     media = media_lly, sd = sd_lly, seed = semilla)
final_lly_tri <- simular_capital(capital_inicial, "triangular", n_sim, n_semanas,
                                  media = media_lly, minimo = min_lly, maximo = max_lly,
                                  moda = media_lly, seed = semilla)

tabla_lly <- bind_rows(
  cbind(distribucion = "Normal", valor_riesgo(final_lly_normal, capital_inicial, alpha)),
  cbind(distribucion = "Triangular", valor_riesgo(final_lly_tri, capital_inicial, alpha))
) %>% mutate(across(where(is.numeric), ~ label_dollar(accuracy = 0.01)(.x)))

print(tabla_lly)

cat("Respuesta 7: a diferencia de DOW, LLY tuvo un retorno semanal promedio positivo (",
    label_percent(accuracy = 0.01)(media_lly),
    "), asi que el valor esperado final queda por encima del capital inicial en ambas",
    "distribuciones. Igual que con DOW, la normal vuelve a dominar a la triangular en",
    "valor esperado y en VaR/ES - mismo patron, dos acciones distintas.\n")

# ============================================================================================
# EJERCICIO 2 - Eleccion de la mejor inversion ----
# ============================================================================================

# ENUNCIADO: "Te has ganado una rifa de COP $10,000,000! [...] Has decidido
# invertir tus ganancias ocasionales en algun activo de renta variable a un
# periodo de un ano. Tienes 3 opciones [...] Utilizando simulaciones con
# 100,000 iteraciones calcula para cada posible inversion: 1. El valor
# esperado de la inversion al final del ano. 2. El valor en riesgo con una
# confianza del 99%. 3. La perdida esperada en el 1% de los casos."
#   a. Activo con retornos ANUALES normales, media=8%, sd=15%.
#   b. Activo con retornos MENSUALES triangulares, min=-13.84%, max=21.49%,
#      media=4.76%.
#   c. Activo con retornos DIARIOS normales, media=0.102%, sd=1.79%.

# NOTA DE IMPLEMENTACION: para el activo (c), "un ano" se toma como 252
# dias habiles (convencion estandar en finanzas para retornos diarios).
# Para el (b), se compone mensualmente 12 veces; para el (a), un solo
# periodo anual.

capital_p2 <- 10000000  # COP
n_sim_p2 <- 100000

final_a <- simular_capital(capital_p2, "normal", n_sim_p2, 1,
                            media = 0.08, sd = 0.15, seed = semilla)
final_b <- simular_capital(capital_p2, "triangular", n_sim_p2, 12,
                            media = 0.0476, minimo = -0.1384, maximo = 0.2149,
                            moda = 0.0476, seed = semilla)
final_c <- simular_capital(capital_p2, "normal", n_sim_p2, 252,
                            media = 0.00102, sd = 0.0179, seed = semilla)

fmt_cop <- label_dollar(prefix = "COP $", big.mark = ".", decimal.mark = ",", accuracy = 1)

tabla_p2 <- bind_rows(
  cbind(activo = "a) Normal, anual", valor_riesgo(final_a, capital_p2, alpha)),
  cbind(activo = "b) Triangular, mensual x12", valor_riesgo(final_b, capital_p2, alpha)),
  cbind(activo = "c) Normal, diaria x252", valor_riesgo(final_c, capital_p2, alpha))
) %>%
  select(activo, valor_esperado, VaR, ES) %>%
  mutate(across(where(is.numeric), fmt_cop))

print(tabla_p2)

cat("Respuesta:\n")
cat("1. Valor esperado: (a)", fmt_cop(mean(final_a)), ", (b)", fmt_cop(mean(final_b)),
    ", (c)", fmt_cop(mean(final_c)), "\n")
cat("2. VaR al 99%: (a)", fmt_cop(capital_p2 - quantile(final_a, alpha)),
    ", (b)", fmt_cop(capital_p2 - quantile(final_b, alpha)),
    ", (c)", fmt_cop(capital_p2 - quantile(final_c, alpha)), "\n")
cat("3. ES en el 1% de los casos: (a)",
    fmt_cop(capital_p2 - mean(final_a[final_a < quantile(final_a, alpha)])),
    ", (b)", fmt_cop(capital_p2 - mean(final_b[final_b < quantile(final_b, alpha)])),
    ", (c)", fmt_cop(capital_p2 - mean(final_c[final_c < quantile(final_c, alpha)])), "\n")

## Recomendacion ----

cat("La opcion b) (triangular, retornos mensuales) domina a las otras dos en las tres",
    "metricas a la vez: mayor valor esperado y menor VaR y ES (menos perdida potencial",
    "en el peor 1% de escenarios). No hay que sacrificar retorno por seguridad ni",
    "viceversa - b) es simplemente mejor en ambos frentes.\n")

datos_p2 <- bind_rows(
  data.frame(capital_final = final_a, activo = "a) Normal anual"),
  data.frame(capital_final = final_b, activo = "b) Triangular mensual"),
  data.frame(capital_final = final_c, activo = "c) Normal diaria")
)

ggplot(datos_p2, aes(x = capital_final)) +
  geom_histogram(color = "black", fill = "steelblue3", bins = 60) +
  facet_wrap(~activo, scales = "free_x") +
  theme_light() +
  scale_x_continuous(labels = label_dollar(scale = 1e-6, suffix = "M", prefix = "COP $")) +
  labs(x = "Capital final (1 ano)", y = "Frecuencia",
       title = "Distribucion del capital final - 100,000 simulaciones por activo")
