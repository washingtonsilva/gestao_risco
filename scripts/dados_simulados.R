
# Descricao ---------------------------------------------------------------

## Autor: Prof. Washington S. Silva                                      
## Simulando dados do questionário de Araujo & Gomes (2021)              
## para os institutos federais                                           
## Ultima alteração: 04/08/2022                                          


# Simulacao usando fabricatr::fabricate() --------------------------------------

## Pacotes utilizados

library(fabricatr)
library(dplyr)
library(readr)

## fixando a semente
set.seed(425884)

## institutos federais

id = c("IFB", "IFMT", "IFMS", "IFG", "IFGoiano", "IFAL", "IFBA",
       "IF Baiano", "IFCE", "IFMA", "IFPB", "IFPE", "IF Sertão PE",
       "IFPI", "IFRN", "IFS", "IFAC", "IFAP", "IFAM", "IFPA", "IFRO",
       "IFTO", "IFRR", "IFRS", "IFFarroupilha", "IFSUL", "IFPR",
       "IFSC", "IFC", "IFES", "IFRJ", "IFF", "IFMG", "IFNMG",
       "IFSUDESTEDEMINAS", "IFUSULDEMINAS", "IFTM", "IFSP")

## aplicando a funcao fabricate

dados <- fabricate(
  
  N  = 38,

  # GRUPO 1: PERFIL DOS SERVIDORES RESPONDENTE
  # questoes: q1_1, q1_2, q1_3, q1_4, q1_5, q1_6, q1_7, q1_8, q1_9
  
  # Categorica Ordinal - Faixa Etaria 
  q1_1 = draw_ordered(x = rnorm(N),
                      breaks = c(-6, -1, 0, 1),
                      break_labels = c("20-30 anos", 
                                       "31-40 anos",
                                       "41-50 anos", 
                                       "51-60 anos",
                                       "Mais de 60 anos")
                     ), 
  
  # Categorica Nominal Binaria - Genero 
  q1_2 = draw_binary(N= 38, prob = 0.4),   
    
  # Categorica Nominal Binaria - Funcao 
  q1_3 = draw_binary(N = 38, prob = 0.5),   
    
  # Categorica Nominal - Cargo que ocupa no momento
  q1_4 = draw_categorical(prob = c(0.2, 0.2, 0.2, 0.2, 0.2),
                          N = 38,
                          category_labels = c("CD-2", 
                                              "CD-3",
                                              "CD-4", 
                                              "FG-1",
                                              "FG-2")
                          ),
  
  # Categorica Ordinal - Formacao 
  q1_5 = draw_categorical(prob = c(0.05, 0.05, 0.6, 0.1, 0.1, 0.1),
                          N = 38,
                          category_labels = c("Ensino fundamental", 
                                              "Ensino médio",
                                              "Ensino superior", 
                                              "Especialização",
                                              "Mestrado",
                                              "Doutorado")
                          ),
  
  # Categorica Ordinal - Tempo na Instituicao 
  q1_6 = draw_ordered(x = rnorm(N),
                      breaks = c(-4, -1, 0, 1),
                      break_labels = c("Até 5 anos", 
                                       "6-10 anos",
                                       "11-15 anos", 
                                       "16-20 anos",
                                       "Mais de 20 anos")
                      ), 
    
  # Categorica Ordinal - Tempo no cargo que ocupa
  q1_7 = draw_ordered(x = rnorm(N),
                      breaks = c(-5, -1, 0, 1),
                      break_labels = c("Até 5 anos", 
                                       "6-10 anos",
                                       "11-15 anos", 
                                       "16-20 anos",
                                       "Mais de 20 anos")
                      ),
    
  # Categorica Nominal Binaria - Participou de curso sobre gestao de riscos?
  q1_8 = draw_binary(N = 38, prob = 0.8),  
    
  # Categorica Nominal -  Em qual instituto federal atua? 
  q1_9 =  draw_categorical(prob = rep(1/38, 38),
                           N = 38,
                           category_labels = id
                           ),
    
  
  # GRUPO 2: VISAO GERAL DO AMBIENTE DE CONTROLE INTERNO E GERENCIAMENTO DE 
  #          RISCOS
  # questoes: q2_1, q2_2, q2_3, q2_4, q2_5, q2_6, q2_7, q2_8, q2_9, q2_10
  #           q2_11, q2_12, q2_13, q2_14, q2_15, q2_16, q2_17, q2_18  
  
  # Categorica Nominal Binaria 
  q2_1 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_2 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_3 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_4 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_5 = draw_binary(N = 38, prob = 0.5),
     
  # Categorica Nominal Binaria 
  q2_6 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_7 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_8 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_9 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  # 2 niveis: sim, nao
  q2_10 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_11 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_12 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  # 2 niveis: sim, nao
  q2_13 = draw_binary(N = 38, prob = 0.5) ,
    
  # Categorica Nominal Binaria 
  q2_14 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_15 = draw_binary(N = 38, prob = 0.5), 
    
  # Categorica Nominal Binaria 
  q2_16 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_17 = draw_binary(N = 38, prob = 0.5),
    
  # Categorica Nominal Binaria 
  q2_18 = draw_binary(N = 38, prob = 0.5),
    
    
  # GRUPO 3: IDENTIFICAÇÃO/PERCEPÇÃO DOS PRINCIPAIS RISCOS DENTRO DO CONTEXTO 
  #          DAS ETAPAS DA EXECUÇÃO DA GESTÃO DE RISCO
  # q3_3 e q3_5 nao foram simuladas
  # questoes: q3_1, q3_2, q3_4, q3_6, q3_7, q3_8, q3_9, q3_10, q3_11, q3_12,
  #           q3_13, q3_14, q3_15, q3_16, q3_17, q3_18, q3_19  
  
  # Categorica Nominal Binaria 
  q3_1 = draw_binary(N = 38, prob = 0.5),    
    
  # Categorica Nominal Binaria 
  q3_2 = draw_binary(N = 38, prob = 0.5),    
    
  # Categorica Nominal -  Tecnicas utilizadas para IDENTIFICACAO de riscos?
  # q3_3 =  
  
  # Categorica Nominal Binaria 
  q3_4 = draw_binary(N = 38, prob = 0.5),    
  
  # Categorica Nominal -  Tecnicas utilizadas para AVALIACACO de riscos?
  # q3_5 = draw_binary(N = 38, prob = 0.5),    
  
  # Categorica Nominal Binaria 
  q3_6 = draw_binary(N = 38, prob = 0.5),    
  
  # Likert 5 pontos
  q3_7 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
  
  # Likert 5 pontos
  q3_8 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
  
  # Categorica Nominal Binaria 
  q3_9 = draw_binary(N = 38, prob = 0.5),   
    
  # Categorica Nominal Binaria 
  q3_10 = draw_binary(N = 38, prob = 0.5),  
  
  # Categorica Nominal Binaria 
  q3_11 = draw_binary(N = 38, prob = 0.5),  
  
  # Categorica Nominal Binaria 
  q3_12 = draw_binary(N = 38, prob = 0.5),   
  
  # Categorica Nominal Binaria 
  q3_13 = draw_binary(N = 38, prob = 0.5),   
  
  # Categorica Nominal Binaria 
  q3_14 = draw_binary(N = 38, prob = 0.5),   
  
  # Categorica Nominal Binaria 
  q3_15 = draw_binary(N = 38, prob = 0.5),   
  
  # Categorica Nominal Binaria 
  q3_16 = draw_binary(N = 38, prob = 0.5), 
  
  # Categorica Nominal Binaria 
  q3_17 = draw_binary(N = 38, prob = 0.5),    
  
  # Categorica Nominal Binaria 
  q3_18 = draw_binary(N = 38, prob = 0.5), 
  
  # Categorica Nominal Binaria 
  q3_19 = draw_binary(N = 38, prob = 0.5),  
  
  # GRUPO 4: DESAFIOS PARA IMPLANTAÇÃO DE UM SISTEMA DE GERENCIAMENTO DE RISCOS 
  #          NOS INSTITUTUOS FEDERAIS
  # questoes: q4_1, q4_2, q4_3, q4_4, q4_5, q4_6, q4_7, q4_8, q4_9, q4_10, 
  #           q4_11, q4_12
  
  # Likert 5 pontos
  q4_1 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_2 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
  
  # Likert 5 pontos
  q4_3 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_4 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
  
  # Likert 5 pontos
  q4_5 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),

  # Likert 5 pontos
  q4_6 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_7 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_8 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_9 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_10 = draw_ordered(x = rnorm(N),
                        breaks = c(-Inf, -2, -1, 0, 1, Inf),
                        break_labels = c("Discordo totalmente", 
                                         "Discordo parcialmente", 
                                         "Nem concordo, nem discordo", 
                                         "Concordo parcialmente", 
                                         "Concordo totalmente")),
    
  # Likert 5 pontos
  # 5 niveis =   
  q4_11 = draw_ordered(x = rnorm(N),
                       breaks = c(-Inf, -2, -1, 0, 1, Inf),
                       break_labels = c("Discordo totalmente", 
                                        "Discordo parcialmente", 
                                        "Nem concordo, nem discordo", 
                                        "Concordo parcialmente", 
                                        "Concordo totalmente")),
    
  # Likert 5 pontos
  q4_12 = draw_ordered(x = rnorm(N),
                       breaks = c(-Inf, -2, -1, 0, 1, Inf),
                       break_labels = c("Discordo totalmente", 
                                        "Discordo parcialmente", 
                                        "Nem concordo, nem discordo", 
                                        "Concordo parcialmente", 
                                        "Concordo totalmente")),
    
  # GRUPO 5: Impactos/contribuições decorrentes da gestão dos riscos nas UF.
  # questoes: q5_1, q5_2, q5_3, q5_4, q5_5, q5_6, q5_7, q5_8, q5_9, q5_10

  # Likert 5 pontos
  q5_1 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_2 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_3 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_4 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_5 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_6 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_7 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_8 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_9 = draw_ordered(x = rnorm(N),
                      breaks = c(-Inf, -2, -1, 0, 1, Inf),
                      break_labels = c("Discordo totalmente", 
                                       "Discordo parcialmente", 
                                       "Nem concordo, nem discordo", 
                                       "Concordo parcialmente", 
                                       "Concordo totalmente")),
    
  # Likert 5 pontos
  q5_10 = draw_ordered(x = rnorm(N),
                       breaks = c(-Inf, -2, -1, 0, 1, Inf),
                       break_labels = c("Discordo totalmente", 
                                        "Discordo parcialmente", 
                                        "Nem concordo, nem discordo", 
                                        "Concordo parcialmente", 
                                        "Concordo totalmente"))
)

glimpse(dados)


# Dados Simulados: Categoricos --------------------------------------------

dados_categoricos <- dados %>%
  transmute(
    id = ID,
    q1_1 = q1_1,
    q1_2 = as.factor(recode(q1_2, `0` = "Masculino", `1` = "Feminino")),
    q1_3 = as.factor(recode(q1_3, `0` = "Docente", `1` = "Tecnico-Admninstrativo")),
    q1_4 = q1_4,
    q1_5 = q1_5,
    q1_6 = q1_6,
    q1_7 = q1_7,
    q1_8 = as.factor(recode(q1_8, `0` = "Não", `1` = "Sim")),
    q1_9 = q1_9,
    q2_1 = as.factor(recode(q2_1, `0` = "Não", `1` = "Sim")),
    q2_2 = as.factor(recode(q2_2, `0` = "Não", `1` = "Sim")),
    q2_3 = as.factor(recode(q2_3, `0` = "Não", `1` = "Sim")),
    q2_4 = as.factor(recode(q2_4, `0` = "Não", `1` = "Sim")),
    q2_5 = as.factor(recode(q2_5, `0` = "Não", `1` = "Sim")),
    q2_6 = as.factor(recode(q2_6, `0` = "Não", `1` = "Sim")),
    q2_7 = as.factor(recode(q2_7, `0` = "Não", `1` = "Sim")),
    q2_8 = as.factor(recode(q2_8, `0` = "Não", `1` = "Sim")),
    q2_9 = as.factor(recode(q2_9, `0` = "Não", `1` = "Sim")),
    q2_10 = as.factor(recode(q2_10, `0` = "Não", `1` = "Sim")),
    q2_11 = as.factor(recode(q2_11, `0` = "Não", `1` = "Sim")),
    q2_12 = as.factor(recode(q2_12, `0` = "Não", `1` = "Sim")),
    q2_13 = as.factor(recode(q2_13, `0` = "Não", `1` = "Sim")),
    q2_14 = as.factor(recode(q2_14, `0` = "Não", `1` = "Sim")),
    q2_15 = as.factor(recode(q2_15, `0` = "Não", `1` = "Sim")),
    q2_16 = as.factor(recode(q2_16, `0` = "Não", `1` = "Sim")),
    q2_17 = as.factor(recode(q2_17, `0` = "Não", `1` = "Sim")),
    q2_18 = as.factor(recode(q2_18, `0` = "Não", `1` = "Sim")),
    q3_1 = as.factor(recode(q3_1, `0` = "Não", `1` = "Sim")),
    q3_2 = as.factor(recode(q3_2, `0` = "Não", `1` = "Sim")),
    q3_4 = as.factor(recode(q3_4, `0` = "Não", `1` = "Sim")),
    q3_6 = as.factor(recode(q3_6, `0` = "Não", `1` = "Sim")),
    q3_7 = q3_7,
    q3_8 = q3_8,
    q3_9 = as.factor(recode(q3_9, `0` = "Não", `1` = "Sim")),
    q3_10 = as.factor(recode(q3_10, `0` = "Não", `1` = "Sim")),
    q3_11 = as.factor(recode(q3_11, `0` = "Não", `1` = "Sim")),
    q3_12 = as.factor(recode(q3_12, `0` = "Não", `1` = "Sim")),
    q3_13 = as.factor(recode(q3_13, `0` = "Não", `1` = "Sim")),
    q3_14 = as.factor(recode(q3_14, `0` = "Não", `1` = "Sim")),
    q3_15 = as.factor(recode(q3_15, `0` = "Não", `1` = "Sim")),
    q3_16 = as.factor(recode(q3_16, `0` = "Não", `1` = "Sim")),
    q3_17 = as.factor(recode(q3_17, `0` = "Não", `1` = "Sim")),
    q3_18 = as.factor(recode(q3_18, `0` = "Não", `1` = "Sim")),
    q3_19 = as.factor(recode(q3_19, `0` = "Não", `1` = "Sim")),
    q4_1 = q4_1,
    q4_2 = q4_2,
    q4_3 = q4_3,
    q4_4 = q4_4,
    q4_5 = q4_5,
    q4_6 = q4_6,
    q4_7 = q4_7,
    q4_8 = q4_8,
    q4_9 = q4_9,
    q4_10 = q4_10,
    q4_11 = q4_11,
    q4_12 = q4_12,
    q5_1 = q5_1,
    q5_2 = q5_2,
    q5_3 = q5_3,
    q5_4 = q5_4,
    q5_5 = q5_5,
    q5_6 = q5_6,
    q5_7 = q5_7,
    q5_8 = q5_8,
    q5_9 = q5_9,
    q5_10 = q5_10
  )

glimpse(dados_categoricos)

sapply(dados_categoricos, function(x) sum(is.na(x)))

path1 <- "G:/Meu Drive/professor/mpa/orientacoes/turma2021/juliano_mendonca/rfiles/declare_design/dados_categoricos.rds"
write_rds(dados_categoricos, path1)



# Dados Simulados: Numericos ----------------------------------------------

dados_numericos <- dados %>%
  transmute(
    id = ID,
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
    q3_7 = as.numeric(recode(q3_7, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q3_8 = as.numeric(recode(q3_8, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
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
    q4_1 = as.numeric(recode(q4_1, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_2 = as.numeric(recode(q4_2, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_3 = as.numeric(recode(q4_3, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_4 = as.numeric(recode(q4_4, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_5 = as.numeric(recode(q4_5, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_6 = as.numeric(recode(q4_6, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_7 = as.numeric(recode(q4_7, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_8 = as.numeric(recode(q4_8, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_9 = as.numeric(recode(q4_9, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q4_10 = as.numeric(recode(q4_10, "Discordo totalmente"  = 1, 
                                     "Discordo parcialmente" = 2,
                                     "Nem concordo, nem discordo" = 3,
                                     "Concordo parcialmente" = 4,
                                     "Concordo totalmente" = 5)),
    q4_11 = as.numeric(recode(q4_11, "Discordo totalmente"  = 1, 
                                     "Discordo parcialmente" = 2,
                                     "Nem concordo, nem discordo" = 3,
                                     "Concordo parcialmente" = 4,
                                     "Concordo totalmente" = 5)),
    q4_12 = as.numeric(recode(q4_12, "Discordo totalmente"  = 1, 
                                     "Discordo parcialmente" = 2,
                                     "Nem concordo, nem discordo" = 3,
                                     "Concordo parcialmente" = 4,
                                     "Concordo totalmente" = 5)),
    q5_1 = as.numeric(recode(q5_1, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_2 = as.numeric(recode(q5_2, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_3 = as.numeric(recode(q5_3, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_4 = as.numeric(recode(q5_4, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_5 = as.numeric(recode(q5_5, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_6 = as.numeric(recode(q5_6, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_7 = as.numeric(recode(q5_7, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_8 = as.numeric(recode(q5_8, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_9 = as.numeric(recode(q5_9, "Discordo totalmente"  = 1, 
                                   "Discordo parcialmente" = 2,
                                   "Nem concordo, nem discordo" = 3,
                                   "Concordo parcialmente" = 4,
                                   "Concordo totalmente" = 5)),
    q5_10 = as.numeric(recode(q5_10, "Discordo totalmente"  = 1, 
                                     "Discordo parcialmente" = 2,
                                     "Nem concordo, nem discordo" = 3,
                                     "Concordo parcialmente" = 4,
                                     "Concordo totalmente" = 5)),
  )

glimpse(dados_numericos)

sapply(dados_numericos, function(x) sum(is.na(x)))

path2 <- "G:/Meu Drive/professor/mpa/orientacoes/turma2021/juliano_mendonca/rfiles/declare_design/dados_numericos.rds"
write_rds(dados_numericos, path2)

