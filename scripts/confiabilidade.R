
# Descricao ---------------------------------------------------------------

## Autor: Prof. Washington S. Silva                                      
## Analise dos dados simulados dados do questionário de Araujo & Gomes (2021)
## Ultima alteração: 08/08/2022


# Pacotes utilizados ------------------------------------------------------

library(readr)
library(dplyr)
library(gtsummary)


# Importando os dados simulados -------------------------------------------

path <- "C:/Users/Usuario/Desktop/github/gestao_risco/dados_simulados/dados_numericos.rds"
dados_numericos <- read_rds(path)
glimpse(dados_numericos)


dados_grupo4 <-
  dados_numericos %>% select(q4_1,
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

# calculate cronbach's alpha
cronbach <- psych::alpha(dados_grupo4)
cronbach


omeg <- ufs::scaleStructure(dados_grupo4)
omeg
