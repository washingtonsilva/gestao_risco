# Descricao ---------------------------------------------------------------

## Autor: Prof. Washington S. Silva                                      
## Analise dos dados simulados dados do questionário de Araujo & Gomes (2021)
## Ultima alteração: 08/08/2022


# Pacotes utilizados ------------------------------------------------------

library(readr)
library(dplyr)
library(gtsummary)

# Importando os dados simulados -------------------------------------------

path1 <- "C:/Users/Usuario/Desktop/github/gestao_risco/dados_simulados/dados_categoricos.rds"
dados_categoricos <- read_rds(path1)
glimpse(dados_categoricos)

path2 <- "C:/Users/Usuario/Desktop/github/gestao_risco/dados_simulados/dados_numericos.rds"
dados_numericos <- read_rds(path2)
glimpse(dados_numericos)


# Tabelas -----------------------------------------------------------------

dados_grupo4 <-
  dados_categoricos %>% select(q4_1,
                               q4_2,
                               q4_3,
                               q4_4,
                               q4_5,
                               q4_6,
                               q4_7,
                               q4_8,
                               q4_9,
                               q4_10,
                               q4_11,
                               q4_12)

apply(dados_grupo4, 2, table)


dados_grupo4 %>% group_by(levels(dados_grupo4$q4_1)) %>% tally(sort = TRUE)
  
