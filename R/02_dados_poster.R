## Dados do pôster da 70ª RBras (CSV lidos pelo pgfplots em poster.tex)
## Rode a partir da raiz do repositório, depois de R/01_ajuste_familias.R.
suppressPackageStartupMessages({ library(gamlss); library(dplyr); library(readr) })

out <- "poster"

leite <- read_csv("dados/dados_consolidados.csv", show_col_types = FALSE) |>
  select(days_in_milk, milk_yield_yesterday, lactation_number) |>
  filter(!is.na(days_in_milk), !is.na(milk_yield_yesterday),
         !is.na(lactation_number), lactation_number != 0) |>
  as.data.frame()

m <- readRDS("resultados/m_herd_BCPEo.rds")
stopifnot(length(fitted(m)) == nrow(leite))

## Centis em cada DIM observado (os parâmetros dependem só do DIM)
par <- data.frame(DIM = leite$days_in_milk,
                  mu = fitted(m, "mu"), sigma = fitted(m, "sigma"),
                  nu = fitted(m, "nu"), tau = fitted(m, "tau")) |>
  distinct(DIM, .keep_all = TRUE) |>
  arrange(DIM)

cent <- c(5, 10, 25, 50, 75, 90, 95)
for (cc in cent)
  par[[paste0("C", cc)]] <- qBCPEo(cc / 100, par$mu, par$sigma, par$nu, par$tau)

## Até DIM 400: cobre a lactação típica (305 d) e evita a cauda com poucas vacas
centis <- par |> filter(DIM <= 400) |> select(DIM, starts_with("C"))
write_csv(mutate(centis, across(-DIM, \(x) round(x, 2))), file.path(out, "centis.csv"))

## Amostra das observações para o fundo do gráfico
set.seed(70)
pts <- leite |> filter(days_in_milk <= 400) |> slice_sample(n = 2500) |>
  transmute(DIM = days_in_milk, y = round(milk_yield_yesterday, 1))
write_csv(pts, file.path(out, "pontos.csv"))

## Comparação de famílias (todas com P-splines em todos os parâmetros)
fam <- read_csv("resultados/tabela_familias_aic.csv", show_col_types = FALSE) |>
  transmute(fam = Distribution, npar = Parameters, aic = round(AIC, 1),
            daic = round(dAIC, 1))
write_csv(fam, file.path(out, "familias.csv"))

## Números que vão no texto
at <- function(d) centis[which.min(abs(centis$DIM - d)), ]
print(rbind(at(5), at(30), at(60), at(150), at(305)))
pico <- centis[which.max(centis$C50), ]
cat("Pico da mediana:\n"); print(pico)
cat("n registros:", nrow(leite), " vacas: (ver dados)\n")
print(fam)
