

# pacotes usados

library(here)
library(readr)
library(readxl)
library(dplyr)


# importa a planilha de dados e inseri coluna id

dados <- read_xlsx(here::here('dados_dashboard/dados/dados_categoricos_if.xlsx'))
dados <- mutate(dados, id = 1:21)
glimpse(dados)

## converte classe das variáveis para factor

dados_analise <- dados |>
  transmute(
    id = id,
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

glimpse(dados_analise)

## inclui o texto das questoes

## salva dados_analise no formato rds

path_analise <- here::here('dados_dashboard/dados/dados_analise.rds')
write_rds(dados_analise, path_analise)

## Obs: o objeto dados_analise mantém os dados faltantes (NA)
## Obs: objeto dados_analise nao contem as questões com múltiplas respostas

