# Beyond the average lactation curve

**Distributional regression for daily milk yield in Holstein cows**

Pôster apresentado na [70ª RBras](https://70rbras.org/) (IMECC/Unicamp, Campinas, 23 a 25 de novembro de 2026).

📄 **[Pôster em PDF](poster/poster.pdf)**

**João Paulo Assis Bonifácio** · Pedro Mambelli Fernandes · Geovane Fernandes da Silva ·
Gean Pereira Damaceno · João Vitor Andrade Alves de Souza · Nayara Maria Barbosa de Sousa ·
Marina de Arruda Camargo Danes · Luiz Ricardo Nakamura

Universidade Federal de Lavras (UFLA)

## Resumo

Curvas de lactação descrevem só a produção média, e por isso não dizem se uma vaca
produz pouco *para o seu estágio de lactação*. Modelamos a distribuição condicional
inteira da produção diária de leite em função dos dias em lactação (DEL) com GAMLSS,
com todos os parâmetros como P-splines do DEL, e resumimos o ajuste em curvas
centílicas, como as cartas de crescimento infantil.

- 9.514 registros diários de 62 vacas Holandesas em ordenha robótica (Fazenda Palmital, UFLA, Ijaci, MG), de abril de 2024 a janeiro de 2025.
- A distribuição Box-Cox power exponential (BCPE) teve o menor AIC entre as candidatas.
- No pico da lactação (60 DEL), a mediana é 31,5 L/dia e 90% das vacas ficam entre 17,8 e 46,8 L.

## Conteúdo

| arquivo | função |
|---|---|
| `R/01_ajuste_familias.R` | ajusta BCPE, Box-Cox *t*, BCCG, gama e inversa gaussiana; tabela de AIC e de centis |
| `R/02_dados_poster.R` | gera os CSV lidos pelos gráficos do pôster |
| `R/03_figuras.R` | figuras de publicação (centis, worm plot, parâmetros) |
| `poster/poster.tex` | pôster no modelo oficial da 70ª RBras (`pdflatex poster.tex`) |
| `poster/centis.csv`, `poster/familias.csv` | centis ajustados e comparação de famílias |

## Dados

Os dados brutos pertencem à Fazenda Palmital/UFLA e **não estão neste repositório**.
Os scripts esperam o arquivo em `dados/dados_consolidados.csv`. Por isso o
`poster.tex` não compila sem os dados: falta o `pontos.csv`, a amostra de
observações do fundo da Figura 1.

## Referências

- Rigby, R. A.; Stasinopoulos, D. M. (2005). Generalized additive models for location, scale and shape. *Journal of the Royal Statistical Society: Series C*, 54, 507–554.
- Rigby, R. A.; Stasinopoulos, D. M. (2004). Smooth centile curves for skew and kurtotic data modelled using the Box-Cox power exponential distribution. *Statistics in Medicine*, 23, 3053–3076.
