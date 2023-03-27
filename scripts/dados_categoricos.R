# pacotes utilizados ------------------------------------------------------

library(readr)
library(janitor)
library(dplyr)
library(plyr)
library(writexl)


# importando o arquivo csv ------------------------------------------------

path_brutos <- 'dados_brutos/dados-24-03-23.csv'
dados <- read_csv(path_brutos, na = c("")) |> clean_names()
glimpse(dados)

# estruturando os dados ---------------------------------------------------

dados_processados <- dados |> select(-c(x1, x2, x3, x4, x34, x36))
glimpse(dados_processados)

## alterando os nomes das variaveis/colunas

dados_processados <- dados_processados |> dplyr::rename(
  q1_1 = x5,
  q1_2 = x6,
  q1_3 = x7,
  q1_4 = x8,
  q1_5 = x9,
  q1_6 = x10,
  q1_7 = x11,
  q1_8 = x12,
  q1_9 = x13,
  q2_1 = x14,
  q2_2 = x15,
  q2_3 = x16,
  q2_4 = x17,
  q2_5 = x18,
  q2_6 = x19,
  q2_7 = x20,
  q2_8 = x21,
  q2_9 = x22,
  q2_10 = x23,
  q2_11 = x24,
  q2_12 = x25,
  q2_13 = x26,
  q2_14 = x27,
  q2_15 = x28,
  q2_16 = x29,
  q2_17 = x30,
  q2_18 = x31,
  q3_1 = x32,
  q3_2 = x33,
  q3_4 = x35,
  q3_6 = x37,
  q3_7 = x38,
  q3_8 = x39,
  q3_9 = x40,
  q3_10 = x41,
  q3_11 = x42,
  q3_12 = x43,
  q3_13 = x44,
  q3_14 = x45,
  q3_15 = x46,
  q3_16 = x47,
  q3_17 = x48,
  q3_18 = x49,
  q3_19 = x50,
  q4_1 = x51,
  q4_2 = x52,
  q4_3 = x53,
  q4_4 = x54,
  q4_5 = x55,
  q4_6 = x56,
  q4_7 = x57,
  q4_8 = x58,
  q4_9 = x59,
  q4_10 = x60,
  q4_11 = x61,
  q4_12 = x62,
  q5_1 = x63,
  q5_2 = x64,
  q5_3 = x65,
  q5_4 = x66,
  q5_5 = x67,
  q5_6 = x68,
  q5_7 = x69,
  q5_8 = x70,
  q5_9 = x71,
  q5_10 = x72
)

names(dados_processados)


## excluindo pontos (.) do final das questoes em escala de likert (5 pontos)

dados_processados$q3_7 <- gsub('\\.', '', dados$q3_7)
dados_processados$q3_8 <- gsub('\\.', '', dados$q3_8)
dados_processados$q4_1 <- gsub('\\.', '', dados$q4_1)
dados_processados$q4_2 <- gsub('\\.', '', dados$q4_2)
dados_processados$q4_3 <- gsub('\\.', '', dados$q4_3)
dados_processados$q4_4 <- gsub('\\.', '', dados$q4_4)
dados_processados$q4_5 <- gsub('\\.', '', dados$q4_5)
dados_processados$q4_6 <- gsub('\\.', '', dados$q4_6)
dados_processados$q4_7 <- gsub('\\.', '', dados$q4_7)
dados_processados$q4_8 <- gsub('\\.', '', dados$q4_8)
dados_processados$q4_9 <- gsub('\\.', '', dados$q4_9)
dados_processados$q4_10 <- gsub('\\.', '', dados$q4_10)
dados_processados$q4_11 <- gsub('\\.', '', dados$q4_11)
dados_processados$q4_12 <- gsub('\\.', '', dados$q4_12)
dados_processados$q5_1 <- gsub('\\.', '', dados$q5_1)
dados_processados$q5_2 <- gsub('\\.', '', dados$q5_2)
dados_processados$q5_3 <- gsub('\\.', '', dados$q5_3)
dados_processados$q5_4 <- gsub('\\.', '', dados$q5_4)
dados_processados$q5_5 <- gsub('\\.', '', dados$q5_5)
dados_processados$q5_6 <- gsub('\\.', '', dados$q5_6)
dados_processados$q5_7 <- gsub('\\.', '', dados$q5_7)
dados_processados$q5_8 <- gsub('\\.', '', dados$q5_8)
dados_processados$q5_9 <- gsub('\\.', '', dados$q5_9)
dados_processados$q5_10 <- gsub('\\.', '', dados$q5_10)

## convertendo para a classe factor

meus_niveis <- c("Discordo totalmente", "Discordo parcialmente", 
                 "Nem concordo, nem discordo", "Concordo parcialmente", 
                 "Concordo totalmente")

dados_categoricos <- dados_processados |> 
  transmute(q1_1 = as.factor(q1_1), 
            q1_2 = as.factor(q1_2), 
            q1_3 = as.factor(q1_3), 
            q1_4 = as.factor(q1_4),
            q1_5 = as.factor(q1_5), 
            q1_6 = as.factor(q1_6), 
            q1_7 = as.factor(q1_7), 
            q1_8 = as.factor(q1_8),
            q1_9 = as.factor(q1_9), 
            q2_1 = as.factor(q2_1),  
            q2_2 = as.factor(q2_2), 
            q2_3 = as.factor(q2_2),
            q2_4 = as.factor(q2_2), 
            q2_5 = as.factor(q2_5), 
            q2_6 = as.factor(q2_6), 
            q2_7 = as.factor(q2_7),
            q2_8 = as.factor(q2_8), 
            q2_9 = as.factor(q2_9), 
            q2_10 = as.factor(q2_10), 
            q2_11 = as.factor(q2_11),
            q2_12 = as.factor(q2_12), 
            q2_13 = as.factor(q2_13), 
            q2_14 = as.factor(q2_14), 
            q2_15 = as.factor(q2_15),
            q2_16 = as.factor(q2_16), 
            q2_17 = as.factor(q2_17), 
            q2_18 = as.factor(q2_18), 
            q3_1 = as.factor(q3_1), 
            q3_2 = as.factor(q3_2), 
            q3_4 = as.factor(q3_4),  
            q3_6 = as.factor(q3_6),  
            q3_7 = factor(q3_7, levels = meus_niveis), 
            q3_8 = factor(q3_8, levels = meus_niveis),  
            q3_9 = as.factor(q3_9), 
            q3_10 = as.factor(q3_10), 
            q3_11 = as.factor(q3_11), 
            q3_12 = as.factor(q3_12), 
            q3_13 = as.factor(q3_13), 
            q3_14 = as.factor(q3_14), 
            q3_15 = as.factor(q3_15), 
            q3_16 = as.factor(q3_16), 
            q3_17 = as.factor(q3_17), 
            q3_18 = as.factor(q3_18), 
            q3_19 = as.factor(q3_19),
            q4_1 = as.factor(q4_1), 
            q4_2 = as.factor(q4_2), 
            q4_3 = as.factor(q4_3), 
            q4_4 = as.factor(q4_4),
            q4_5 = as.factor(q4_5), 
            q4_6 = as.factor(q4_6), 
            q4_7 = as.factor(q4_7), 
            q4_8 = as.factor(q4_8),
            q4_9 = as.factor(q4_9), 
            q4_10 = as.factor(q4_10), 
            q4_11 = as.factor(q4_11), 
            q4_12 = as.factor(q4_12),
            q5_1 = as.factor(q5_1), 
            q5_2 = as.factor(q5_2), 
            q5_3 = as.factor(q5_3), 
            q5_4 = as.factor(q5_4),
            q5_5 = as.factor(q5_5), 
            q5_6 = as.factor(q5_6), 
            q5_7 = as.factor(q5_7), 
            q5_8 = as.factor(q5_8),
            q5_9 = as.factor(q5_9), 
            q5_10 = as.factor(q5_10)) |>
  as.data.frame()

class(dados_categoricos)

glimpse(dados_categoricos)

## exclui as respostas dos campi do IFMG

dados_categoricos_if <- dados_categoricos |> slice(-(20:30))
glimpse(dados_categoricos_if)

## salva arquivo xlsx

path_xlsx <- 'dados_processados/dados_categoricos.xlsx'
write_xlsx(dados_categoricos_if, path_xlsx)
           

