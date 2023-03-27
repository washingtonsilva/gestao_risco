# Descricao ---------------------------------------------------------------

## Autor: Prof. Washington S. Silva                                      
## Analise dos dados simulados dados do questionário de Araujo & Gomes (2021)
## Ultima alteração: 08/08/2022


# Pacotes utilizados ------------------------------------------------------

library(readr)
library(dplyr)
library(vtable)
library(DescTools)
library(gmodels)
library(janitor)
library(tidyr)

# Importando os dados simulados -------------------------------------------

path1 <- "dados_simulados/dados_numericos.rds"
dados_numericos <- read_rds(path1)
glimpse(dados_numericos)

path2 <- "dados_simulados/dados_categoricos.rds"
dados_categoricos <- read_rds(path2)
glimpse(dados_categoricos)


# Categoricos -------------------------------------------------------------

## Equivalente a Tabela 3 de Araujo e Gomes (2021): Sexo e Faixa Etaria

CrossTable(dados_categoricos$q1_1, dados_categoricos$q1_2, 
           digits = 1, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, format = "SPSS")

## Equivalente a Tabela 4 de Araujo e Gomes (2021): Formacao e Funcao

CrossTable(dados_categoricos$q1_5, dados_categoricos$q1_3, 
           digits = 1, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, format = "SPSS")

## Equivalente a Tabela 5 de Araujo e Gomes (2021): Tempo na instituicao

dados_categoricos %>%
  tabyl(q1_6)  %>%
  adorn_totals("row") %>% 
  adorn_rounding(digits = 1)

## Equivalente à Tab. 6 de Araujo e Gomes (2021): Tempo de Experiencia e 
## Participacao em curso de gestao de riscos

CrossTable(dados_categoricos$q1_6, dados_categoricos$q1_8, 
           digits = 2, prop.chisq = FALSE, prop.c = FALSE, 
           prop.r = FALSE, format = "SPSS")


## Equivalente à Tab. 7 de Araujo & Gomes (2021)

dados_catgrupo4 <-
  dados_categoricos %>% select(id, q4_1, q4_2, q4_3, q4_4, q4_5, q4_6, q4_7,
                               q4_8, q4_9, q4_10, q4_11, q4_12)

catlongo <- dados_catgrupo4 %>%
  pivot_longer(!id, names_to = "liker", values_to = "count")

t2 <- catlongo %>%
  tabyl(liker, count)
t2

t2 %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()



# Numericos ---------------------------------------------------------------

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

## Equivalente à Tab. 8 de Araujo & Gomes (2021)

questoes <- c(
  'Ignorar riscos relevantes',
  'Informacoes complexas',
  'Falta de confianca/entendimento',
  'Divergencias sobre o risco',
  'Estrutura institucional inadequada',
  'sistema de informacao ineficiente',
  'Renovacao do ciclo de GR',
  'Mapeamento dos processos',
  'Falta de engajamento',
  'Falta de capacitacao',
  'Excesso de demandas',
  'Recursos insuficientes'
)

## Equivalente a Tabela 8 de Araujo e Gomes (2021)

st(dados_grupo4, col.align = 'center', labels = questoes,
   summ = list(
     c('round(mean(x), 2)', 'pctile(x)[25]', 'round(median(x), 2)', 
       'pctile(x)[75]', 'Mode(x)','round(sd(x), 2)', 'round(mad(x), 2)')
   ),
   summ.names = list(
     c('Media', 'q1','Mediana', 'q3', 'Moda', 'Desvio-Padrao', 'DAM')
   ))



