library(gamlss)
library(readr)
library(dplyr)

# ==============================================================================
# 1. LEITURA E PREPARO (mesma base do modelo de referência do rebanho, n = 9514)
# ==============================================================================

raw <- read_csv("dados/dados_consolidados.csv", show_col_types = FALSE)

leite <- raw |>
  select(days_in_milk, milk_yield_yesterday, lactation_number) |>
  filter(!is.na(days_in_milk), !is.na(milk_yield_yesterday),
         !is.na(lactation_number), lactation_number != 0) |>
  as.data.frame()

# ==============================================================================
# 2. FAMÍLIAS COM A COVARIÁVEL days_in_milk (P-splines em todos os parâmetros)
# ==============================================================================

m_bcpeo <- gamlss(                                  # BCPEo — modelo de referência
  milk_yield_yesterday ~ pb(days_in_milk),
  sigma.fo = ~ pb(days_in_milk),
  nu.fo    = ~ pb(days_in_milk),
  tau.fo   = ~ pb(days_in_milk),
  family   = BCPEo,
  data     = leite,
  control  = gamlss.control(n.cyc = 100, c.crit = 0.01, trace = FALSE)
)
dir.create("resultados", showWarnings = FALSE)
saveRDS(m_bcpeo, "resultados/m_herd_BCPEo.rds")

m_bct <- gamlss(
  milk_yield_yesterday ~ pb(days_in_milk),
  sigma.fo = ~ pb(days_in_milk),
  nu.fo    = ~ pb(days_in_milk),
  tau.fo   = ~ pb(days_in_milk),
  family   = BCTo,
  data     = leite,
  method   = RS(60)
)

m_bccg <- gamlss(
  milk_yield_yesterday ~ pb(days_in_milk),
  sigma.fo = ~ pb(days_in_milk),
  nu.fo    = ~ pb(days_in_milk),
  family   = BCCG,
  data     = leite,
  method   = RS(60)
)

m_ga <- gamlss(
  milk_yield_yesterday ~ pb(days_in_milk),
  sigma.fo = ~ pb(days_in_milk),
  family   = GA,
  data     = leite,
  method   = RS(60)
)

m_ig <- gamlss(
  milk_yield_yesterday ~ pb(days_in_milk),
  sigma.fo = ~ pb(days_in_milk),
  family   = IG,
  data     = leite,
  method   = RS(60)
)

saveRDS(m_bct,  "resultados/m_bct.rds")
saveRDS(m_bccg, "resultados/m_bccg.rds")
saveRDS(m_ga,   "resultados/m_ga.rds")
saveRDS(m_ig,   "resultados/m_ig.rds")

# ==============================================================================
# 3. TABELA DE COMPARAÇÃO (parâmetros, gl, deviance global, AIC, BIC)
# ==============================================================================

modelos <- list(BCPEo = m_bcpeo, BCTo = m_bct, BCCG = m_bccg,
                GA = m_ga, IG = m_ig)
np <- c(BCPEo = 4, BCTo = 4, BCCG = 3, GA = 2, IG = 2)

tab <- data.frame(
  Distribution = names(modelos),
  Parameters   = np[names(modelos)],
  df    = sapply(modelos, function(m) m$df.fit),
  GDEV  = sapply(modelos, function(m) m$G.deviance),
  AIC   = sapply(modelos, function(m) AIC(m)),
  BIC   = sapply(modelos, function(m) AIC(m, k = log(m$N))),
  row.names = NULL
)
tab <- tab[order(tab$AIC), ]
tab$dAIC <- tab$AIC - min(tab$AIC)

write.csv(tab, "resultados/tabela_familias_aic.csv", row.names = FALSE)
cat("=== TABELA DE FAMILIAS (ordenada por AIC) ===\n")
print(format(tab, digits = 7), row.names = FALSE)

# ==============================================================================
# 4. TABELA CENTÍLICA (centiles.pred) EM DIM DE INTERESSE PRÁTICO
# ==============================================================================

# Marcos da lactação padrão de 305 dias e registro mensal (~30 d) — ICAR (2022)
dim_ref <- c(5, 30, 60, 90, 120, 150, 180, 210, 240, 270, 305)
centis  <- c(5, 10, 25, 50, 75, 90, 95)

cp <- centiles.pred(m_bcpeo, xname = "days_in_milk",
                    xvalues = dim_ref, cent = centis)
colnames(cp) <- c("DIM", paste0("C", centis))
cp[ , -1] <- round(cp[ , -1], 1)

write.csv(cp, "resultados/tabela_centis.csv", row.names = FALSE)
cat("\n=== TABELA CENTILICA (BCPEo) — producao de leite (L/dia) ===\n")
print(cp, row.names = FALSE)

cat("\nOK\n")
