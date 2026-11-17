## =====================================================================
## Figuras de publicação (centis, worm plot, parâmetros)
## Curvas de referência centílicas: produção de leite (L/dia) x DIM
## V3.1: legenda de cor à direita (sem conectores); fig3 = parâmetros.
## Roda a partir da raiz do projeto. NÃO reajusta modelos (carrega .rds).
## =====================================================================
suppressPackageStartupMessages({
  library(gamlss); library(gamlss.ggplots)
  library(dplyr); library(readr); library(ggplot2); library(patchwork)
})

out_dir <- "figuras"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

## --- Dados ------------------------------------------------------------
raw <- read_csv("dados/dados_consolidados.csv", show_col_types = FALSE)
leite <- raw |>
  select(days_in_milk, milk_yield_yesterday, lactation_number) |>
  filter(!is.na(days_in_milk), !is.na(milk_yield_yesterday),
         !is.na(lactation_number), lactation_number != 0) |>
  as.data.frame()

## --- Modelo ajustado (carregado) -------------------------------------
m_herd <- readRDS("resultados/m_herd_BCPEo.rds")

cent     <- c(5, 10, 25, 50, 75, 90, 95)
cent_lab <- paste0("C", cent)

## data.frame longo de centis ajustados em cada DIM observado
centdf <- function(m, dat) {
  mu <- fitted(m, "mu"); sg <- fitted(m, "sigma")
  nu <- fitted(m, "nu"); ta <- fitted(m, "tau")
  o    <- order(dat$days_in_milk)
  dimo <- dat$days_in_milk[o]
  do.call(rbind, lapply(cent, function(cc) {
    data.frame(
      DIM   = dimo,
      centil = factor(paste0("C", cc), levels = cent_lab),
      valor  = qBCPEo(cc / 100, mu = mu[o], sigma = sg[o], nu = nu[o], tau = ta[o])
    )
  }))
}

## --- Estética unificada ----------------------------------------------
## Paleta divergente colorblind-safe (RdBu); C50 em preto.
pal <- c(C5  = "#2166ac", C10 = "#4393c3", C25 = "#92c5de",
         C50 = "#000000",
         C75 = "#f4a582", C90 = "#d6604d", C95 = "#b2182b")

tema_pub <- theme_classic(base_size = 11, base_family = "serif") +
  theme(
    axis.title  = element_text(size = 11),
    axis.text   = element_text(size = 9, colour = "black"),
    axis.line   = element_line(colour = "black", linewidth = 0.4),
    axis.ticks  = element_line(colour = "black", linewidth = 0.4),
    plot.margin = margin(6, 6, 6, 6),
    plot.title  = element_text(size = 11, face = "plain", hjust = 0)
  )

## Painel de centis com legenda de cor à direita (cor por centil).
plot_centiles <- function(m, dat, titulo = NULL, x_max = NULL) {
  cd <- centdf(m, dat)
  if (is.null(x_max)) x_max <- max(cd$DIM)

  ## override.aes deixa C50 mais espessa na legenda
  lwd_override <- ifelse(cent_lab == "C50", 1.0, 0.5)

  ggplot() +
    geom_point(data = dat, aes(days_in_milk, milk_yield_yesterday),
               colour = "grey80", size = 0.18, alpha = 0.35) +
    geom_line(data = cd,
              aes(DIM, valor, colour = centil, linewidth = centil == "C50")) +
    scale_colour_manual(
      values = pal, name = NULL,
      guide  = guide_legend(
        override.aes = list(linewidth = lwd_override),
        keywidth  = unit(1.4, "lines"),
        keyheight = unit(0.75, "lines")
      )
    ) +
    scale_linewidth_manual(values = c(`FALSE` = 0.5, `TRUE` = 1.0), guide = "none") +
    scale_x_continuous(limits  = c(0, x_max),
                       expand  = expansion(mult = c(0.01, 0.02))) +
    labs(x = "Dias em lactação (DIM)", y = "Produção de leite (L/dia)",
         title = titulo) +
    tema_pub +
    theme(
      legend.position  = "right",
      legend.key       = element_blank(),
      legend.text      = element_text(size = 9, family = "serif"),
      legend.spacing.y = unit(0.2, "lines")
    )
}

## =====================================================================
## Figura 1 — Curvas centílicas do rebanho
## =====================================================================
fig1 <- plot_centiles(m_herd, leite)
ggsave(file.path(out_dir, "fig1_centis_rebanho.png"), fig1,
       width = 17, height = 10.5, units = "cm", dpi = 300)
cat("Fig1 salva.\n")

## =====================================================================
## Figura 2 — Worm plot do rebanho
## =====================================================================
png(file.path(out_dir, "fig2_wormplot_rebanho.png"),
    width = 12, height = 12, units = "cm", res = 300)
par(mar = c(4, 4, 1, 1), family = "serif", cex = 0.9)
wp(m_herd, ylim.all = 0.7)
dev.off()
cat("Fig2 salva.\n")

## =====================================================================
## Figura 3 — Funções suaves dos parâmetros da distribuição × DIM
## Usa gamlss.ggplots::fitted_terms() — saída padrão do pacote, com
## banda de confiança. Análogo à Fig 2 de Ossifo et al. (2025).
## =====================================================================
param_titles <- list(
  mu    = expression(bold("A.") ~ mu ~ "— mediana"),
  sigma = expression(bold("B.") ~ sigma ~ "— dispersão"),
  nu    = expression(bold("C.") ~ nu ~ "— assimetria"),
  tau   = expression(bold("D.") ~ tau ~ "— curtose")
)

param_plots <- lapply(names(param_titles), function(p) {
  ft <- fitted_terms(m_herd, what = p)
  ft[[1]] +
    labs(x     = "Dias em lactação (DIM)",
         y     = "Efeito suave (escala do preditor)",
         title = param_titles[[p]])
})

fig3 <- (param_plots[[1]] + param_plots[[2]]) /
        (param_plots[[3]] + param_plots[[4]])
ggsave(file.path(out_dir, "fig3_params_rebanho.png"), fig3,
       width = 17, height = 12, units = "cm", dpi = 300)
cat("Fig3 salva.\n")

## =====================================================================
## Números reais para o texto (impressão no console)
## =====================================================================
cat("\n================ NÚMEROS PARA O MANUSCRITO V3 ================\n")
cat(sprintf("n registros (rebanho) = %d | n vacas = %d | DIM range = %d-%d\n",
            nrow(leite),
            length(unique(raw$animal_number[!is.na(raw$animal_number)])),
            min(leite$days_in_milk), max(leite$days_in_milk)))

edf_par <- function(m) sapply(c("mu","sigma","nu","tau"), function(p)
  tryCatch(m[[paste0(p, ".df")]], error = function(e) NA))
cat("\n--- Modelo do rebanho (BCPEo) ---\n")
cat("AIC =", round(AIC(m_herd), 1), "\n")
cat("edf:", paste(names(edf_par(m_herd)), round(unlist(edf_par(m_herd)), 1),
                  sep = "=", collapse = "  "), "\n")

res <- tryCatch(resid(m_herd), error = function(e) NA)
if (!all(is.na(res))) {
  cat("\n--- Resíduos quantílicos (rebanho) ---\n")
  cat(sprintf("media=%.3f  var=%.3f  assimetria=%.3f  excesso_curtose=%.3f\n",
              mean(res), var(res),
              mean((res - mean(res))^3) / sd(res)^3,
              mean((res - mean(res))^4) / sd(res)^4 - 3))
}
cat("\nFiguras salvas em:", out_dir, "\n")
cat("=============================================================\n")
