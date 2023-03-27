

## Pacotes necessários

library(readxl)
library(readr)
library(dplyr)


## Importanto o arquivo .xlsx

dados_categoricos <- read_xlsx('dados_processados/dados_categoricos.xlsx')
glimpse(dados_categoricos)

## Criando a estrutura de os dados categóricos

dados_categoricos_if <- dados_categoricos %>%
  transmute(
    q1_1 = factor(
      q1_1,
      levels = c(
        "20-30 anos",
        "31-40 anos",
        "41-50 anos",
        "51-60 anos",
        "Mais de 60 anos"
      ),
      ordered = TRUE
    ),
    q1_2 = factor(q1_2),
    q1_3 = factor(q1_3),
    q1_4 = factor(q1_4),
    q1_5 = factor(
      q1_5,
      levels =  c(
        "Ensino fundamental",
        "Ensino médio",
        "Ensino superior",
        "Especialização",
        "Mestrado",
        "Doutorado"
      ),
      ordered = TRUE
    ),
    q1_6 = factor(
      q1_6,
      levels = c(
        "Até 5 anos",
        "6-10 anos",
        "11-15 anos",
        "16-20 anos",
        "Mais de 20 anos"
      ),
      ordered = TRUE
    ),
    q1_7 = factor(
      q1_7,
      levels = c(
        "Até 5 anos",
        "6-10 anos",
        "11-15 anos",
        "16-20 anos",
        "Mais de 20 anos"
      ),
      ordered = TRUE
    ),
    q1_8 = factor(q1_8),
    q1_9 = factor(q1_9),
    q2_1 = factor(q2_1),
    q2_2 = factor(q2_2),
    q2_3 = factor(q2_3),
    q2_4 = factor(q2_4),
    q2_5 = factor(q2_5),
    q2_6 = factor(q2_6),
    q2_7 = factor(q2_7),
    q2_8 = factor(q2_8),
    q2_9 = factor(q2_9),
    q2_10 = factor(q2_10),
    q2_11 = factor(q2_11),
    q2_12 = factor(q2_12),
    q2_13 = factor(q2_13),
    q2_14 = factor(q2_14),
    q2_15 = factor(q2_15),
    q2_16 = factor(q2_16),
    q2_17 = factor(q2_17),
    q2_18 = factor(q2_18),
    q3_1 = factor(q3_1),
    q3_2 = factor(q3_2),
    q3_4 = factor(q3_4),
    q3_6 = factor(q3_6),
    q3_7 = factor(
      q3_7,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q3_8 = factor(
      q3_8,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q3_9 = factor(q3_9),
    q3_10 = factor(q3_10),
    q3_11 = factor(q3_11),
    q3_12 = factor(q3_12),
    q3_13 = factor(q3_13),
    q3_14 = factor(q3_14),
    q3_15 = factor(q3_15),
    q3_16 = factor(q3_16),
    q3_17 = factor(q3_17),
    q3_18 = factor(q3_18),
    q3_19 = factor(q3_19),
    q4_1 = factor(
      q4_1,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_2 = factor(
      q4_2,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_3 = factor(
      q4_3,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_4 = factor(
      q4_4,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_5 = factor(
      q4_5,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_6 = factor(
      q4_6,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_7 = factor(
      q4_7,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_8 = factor(
      q4_8,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_9 = factor(
      q4_9,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_10 = factor(
      q4_10,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_11 = factor(
      q4_11,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q4_12 = factor(
      q4_12,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_1 = factor(
      q5_1,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_2 = factor(
      q5_2,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_3 = factor(
      q5_3,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_4 = factor(
      q5_4,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_5 = factor(
      q5_5,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_6 = factor(
      q5_6,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_7 = factor(
      q5_7,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_8 = factor(
      q5_8,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_9 = factor(
      q5_9,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    ),
    q5_10 = factor(
      q5_10,
      levels = c(
        "Discordo totalmente", 
        "Discordo parcialmente", 
        "Nem concordo, nem discordo", 
        "Concordo parcialmente", 
        "Concordo totalmente"),
      ordered = TRUE
    )
  )

glimpse(dados_categoricos_if)

## salava arquivo rds

path_categoricos <- 'dados_processados/dados_categoricos_if.rds'
write_rds(dados_categoricos_if, path_categoricos)


## Criando a estrutura para os dados numéricos

dados_numericos_if <- dados_categoricos_if %>%
  transmute(
    q1_1 = q1_1,
    q1_2 = as.integer(q1_2),
    q1_3 = as.integer(q1_3),
    q1_4 = q1_4,
    q1_5 = q1_5,
    q1_6 = q1_6,
    q1_7 = q1_7,
    q1_8 = as.integer(q1_8),
    q1_9 = q1_9,
    q2_1 = as.integer(q2_1),
    q2_2 = as.integer(q2_2),
    q2_3 = as.integer(q2_3),
    q2_4 = as.integer(q2_4),
    q2_5 = as.integer(q2_5),
    q2_6 = as.integer(q2_6),
    q2_7 = as.integer(q2_7),
    q2_8 = as.integer(q2_8),
    q2_9 = as.integer(q2_9),
    q2_10 = as.integer(q2_10),
    q2_11 = as.integer(q2_11),
    q2_12 = as.integer(q2_12),
    q2_13 = as.integer(q2_13),
    q2_14 = as.integer(q2_14),
    q2_15 = as.integer(q2_15),
    q2_16 = as.integer(q2_16),
    q2_17 = as.integer(q2_17),
    q2_18 = as.integer(q2_18),
    q3_1 = as.integer(q3_1),
    q3_2 = as.integer(q3_2),
    q3_4 = as.integer(q3_4),
    q3_6 = as.integer(q3_6),
    q3_7 = as.numeric(
      recode(
        q3_7,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q3_8 = as.numeric(
      recode(
        q3_8,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q3_9 = as.integer(q3_9),
    q3_10 = as.integer(q3_10),
    q3_11 = as.integer(q3_11),
    q3_12 = as.integer(q3_12),
    q3_13 = as.integer(q3_13),
    q3_14 = as.integer(q3_14),
    q3_15 = as.integer(q3_15),
    q3_16 = as.integer(q3_16),
    q3_17 = as.integer(q3_17),
    q3_18 = as.integer(q3_18),
    q3_19 = as.integer(q3_19),
    q4_1 = as.numeric(
      recode(
        q4_1,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_2 = as.numeric(
      recode(
        q4_2,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_3 = as.numeric(
      recode(
        q4_3,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_4 = as.numeric(
      recode(
        q4_4,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_5 = as.numeric(
      recode(
        q4_5,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_6 = as.numeric(
      recode(
        q4_6,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_7 = as.numeric(
      recode(
        q4_7,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_8 = as.numeric(
      recode(
        q4_8,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_9 = as.numeric(
      recode(
        q4_9,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_10 = as.numeric(
      recode(
        q4_10,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_11 = as.numeric(
      recode(
        q4_11,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q4_12 = as.numeric(
      recode(
        q4_12,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_1 = as.numeric(
      recode(
        q5_1,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_2 = as.numeric(
      recode(
        q5_2,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_3 = as.numeric(
      recode(
        q5_3,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_4 = as.numeric(
      recode(
        q5_4,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_5 = as.numeric(
      recode(
        q5_5,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_6 = as.numeric(
      recode(
        q5_6,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_7 = as.numeric(
      recode(
        q5_7,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_8 = as.numeric(
      recode(
        q5_8,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_9 = as.numeric(
      recode(
        q5_9,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
    q5_10 = as.numeric(
      recode(
        q5_10,
        "Discordo totalmente"  = 1,
        "Discordo parcialmente" = 2,
        "Nem concordo, nem discordo" = 3,
        "Concordo parcialmente" = 4,
        "Concordo totalmente" = 5
      )
    ),
  )

glimpse(dados_numericos_if)

## salava arquivo rds

path_numericos <- 'dados_processados/dados_numericos_if.rds'
write_rds(dados_numericos_if, path_numericos)




