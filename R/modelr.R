
# Pacotes -----------------------------------------------------------------

# Carregar pacotes/dependências
library(wooldridge)  # CRAN v1.4-2
library(performance) # CRAN v0.8.0
library(broom)       # CRAN v0.7.12
library(see)         # CRAN v0.6.9
library(report)      # CRAN v0.5.1
library(parameters)  # CRAN v0.17.0
library(tibble)      # CRAN v3.1.6
library(ggplot2)     # CRAN v3.3.5
library(patchwork)   # CRAN v1.1.1
library(magrittr)    # CRAN v2.0.2



# Dados -------------------------------------------------------------------

# Carregar dataset
data("wage1")

# Inspecionar dataset
tibble::as_tibble(wage1)

# Estatísticas descritivas
parameters::describe_distribution(wage1)

# Visualização dos dados
p1 <- wage1 %>%
  ggplot2::ggplot() +
  ggplot2::aes(y = wage, x = educ) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

p2 <- wage1 %>%
  ggplot2::ggplot() +
  ggplot2::aes(y = wage, x = exper) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method = "lm")

p1 + p2


# Modelo de regressão linear ----------------------------------------------

# Estimar modelo
fit_modelo <- lm(lwage ~ educ + exper, data = wage1)

# Obter resultados
summary(fit_modelo)

# Obter resultados em formato "tidy"
broom::tidy(fit_modelo)
broom::glance(fit_modelo)

# Diagnosticar hipóteses do modelo
performance::check_model(fit_modelo)



# Reportando resultados ---------------------------------------------------

# Cria a representação da equação (em LaTeX)
equatiomatic::extract_eq(
  model = fit_modelo,
  swap_var_names = c(
    "log(wage)" = "log(Salario)",
    "educ"      = "Educacao",
    "exper"     = "Experiencia")
  )

# Cria um texto com interpretação automatizada dos resultados
report::report(fit_modelo)

# Cria uma tabela dos resultados
report::report_table(fit_modelo)

# Gráfico dos coeficientes
fit_modelo %>%
  parameters::parameters() %>%
  plot() +
  ggplot2::labs(title = "Coeficientes do modelo")


