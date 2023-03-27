
# Descricao ---------------------------------------------------------------

## Autor: Prof. Washington S. Silva                                      
## Analise dos dados simulados dados do questionário de Araujo & Gomes (2021)
## Ultima alteração: 08/08/2022


# Pacotes utilizados ------------------------------------------------------

library(readr)
library(dplyr)
library(likert)
library(ggplot2)
library(scales) 


# Importando os dados simulados -------------------------------------------

path <- "C:/Users/Usuario/Desktop/github/gestao_risco/dados_simulados/dados_categoricos.rds"
dados_categoricos <- read_rds(path)
glimpse(dados_categoricos)


# Grupo 3 -----------------------------------------------------------------

## selecionando as questoes com respostas na escala likert
dados_grupo3_likert <- dados_categoricos %>% select(q3_7, q3_8)  
glimpse(dados_grupo3_likert)

## selecionando as questoes com respostas categoricas binarias
dados_grupo3_binarias <- dados_categoricos %>%
  select(q3_1, q3_2, q3_4, q3_6, q3_9, q3_10, q3_11, q3_12,
         q3_13, q3_14, q3_15, q3_16, q3_17, q3_18, q3_19)  
glimpse(dados_grupo3_binarias)

## graficos para as questoes com respostas na escala likert

### alterando o nome das questoes/variaveis tipo likert
names(dados_grupo3_likert) <- c(q3_7 = "Você concorda que existe contribuição 
                                        dos auditores de órgãos de controle 
                                        (TCU/CGU) para gerenciar os riscos da 
                                        sua instituição?",
                                q3_8 = "Você concorda que existe contribuição 
                                        dos auditores internos para o processo 
                                        de gestão de riscos da sua instituição?")

### criando estrutura de dados para o pacote likert 
grupo3_likert <- likert(dados_grupo3_likert)

### grafico de barras empilhadas basico via pacote likert
g1 <- plot(grupo3_likert, 
           type = "bar", 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = TRUE,
           plot.percent.high = TRUE,
           plot.percent.neutral = TRUE,
           plot.percents = FALSE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           ylabel = "Porcentagem",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = FALSE,
           zero.print = TRUE)

### definindo diretorio para salvar os graficos g1, g2, g3 e g4
setwd("C:/Users/Usuario/Desktop/github/gestao_risco/figs")

### salvando g1
ggsave(filename = "fig01.png",
       plot = g1,
       width = 13,
       height = 9,
       dpi = 600,
       units = "in")

### grafico de barras empinhadas com porcentagems por nivel via pacote likert
g2 <- plot(grupo3_likert, 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = FALSE,
           plot.percent.high = FALSE,
           plot.percent.neutral = TRUE,
           plot.percents = TRUE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           ylabel = "Porcentagem",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = FALSE,
           zero.print = TRUE)

### salvando g2
ggsave(filename = "fig02.png",
       plot = g2,
       width = 13,
       height = 9,
       dpi = 600,
       units = "in")


## graficos para as questoes com respostas categoricas binarias


plotdata <- dados_grupo3_binarias %>%
  group_by(q3_1) %>%
  summarize(n = n()) %>%
  mutate(pct = n/sum(n),
         lbl = scales::percent(pct))


# Grupo 4 -----------------------------------------------------------------

### selecionando as questoes do Grupo 4
dados_grupo4 <- dados_categoricos %>%
                    select(q4_1, q4_2, q4_3, q4_4, q4_5, q4_6, q4_7, q4_8, 
                           q4_9, q4_10, q4_11, q4_12)  
glimpse(dados_grupo4)

names(dados_grupo4) <- c(q4_1 = "Existe a possibilidade de se ignorar 
                                   riscos importantes na instituição?",
                         q4_2 = "Pode-se lidar de maneira inadequada 
                                  com a incerteza resultante de informações 
                                  incompletas ou da complexidade do próprio 
                                   risco?",
                         q4_3 = "Pode haver falta de confiança ou entendimento 
                                 entre os servidores envolvidos?",
                         q4_4 = "Pode haver divergências em torno da gravidade 
                                 percebida de um risco ou de estratégias 
                                 adotadas para administrá-lo?",
                         q4_5 = "Pode haver estruturas e sistemas 
                                 institucionais inadequados para a gestão de 
                                 riscos na Instituição?",
                         q4_6 = "A Instituição não dispõe de sistema de 
                                 informação capaz de gerenciar riscos ou o 
                                 mesmo é ineficiente?",
                         q4_7 = "Pode haver dificuldade na renovação do ciclo 
                                 de gerenciamento de riscos, tendo em vista 
                                 que o mesmo deve ser contínuo?",
                         q4_8 = "A falta  de mapeamento  dos processos na 
                                 Instituição  pode ser decisiva  para a 
                                 implantação eficaz da gestão de riscos na 
                                 Instituição?",
                         q4_9 = "A falta de engajamento dos servidores 
                                 envolvidos pode ser um fator comprometedor 
                                 para a gestão de riscos na Instituição?",
                         q4_10 = "A falta de capacitação de servidores ainda 
                                  constitui um fator limitante para o sucesso 
                                  da gestão de riscos na Instituição?",
                         q4_11 = "O excesso de demandas atuais pode 
                                  comprometer o sucesso da implantação de 
                                  gestão de riscos na Instituição?",
                         q4_12 = "A administração aloca recursos suficientes 
                                  e apropriados para a gestão de riscos?"
                           )


## Analise via pacote likert 

grupo4_likert <- likert(dados_grupo4)

### grafico de barras empilhadas basico
g1 <- plot(grupo4_likert, 
           type = "bar", 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = TRUE,
           plot.percent.high = TRUE,
           plot.percent.neutral = TRUE,
           plot.percents = FALSE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           ylabel = "Porcentagem",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = FALSE,
           zero.print = TRUE)

### definindo diretorio para salvar os graficos g1, g2, g3 e g4
setwd("C:/Users/Usuario/Desktop/github/gestao_risco/figs")

### salvando g1
ggsave(filename = "fig01.png",
       plot = g1,
       width = 13,
       height = 9,
       dpi = 600,
       units = "in")

# grafico de barras empinhadas com porcentagems por nivel
g2 <- plot(grupo4_likert, 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = FALSE,
           plot.percent.high = FALSE,
           plot.percent.neutral = TRUE,
           plot.percents = TRUE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           ylabel = "Porcentagem",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = FALSE,
           zero.print = TRUE)

# salvando g2
ggsave(filename = "fig02.png",
       plot = g2,
       width = 13,
       height = 9,
       dpi = 600,
       units = "in")


## graficos para as questoes com respostas na escala likert




# Grupo 5 -----------------------------------------------------------------

### selecionando as questoes do Grupo 5
dados_grupo5 <- dados_categoricos %>%
  select(q5_1, q5_2, q5_3, q5_4, q5_5, q5_6, q5_7, q5_8, q5_9, q5_10) 

glimpse(dados_grupo5)

names(dados_grupo5) <- c(q5_1 = "Os riscos identificados são capazes de 
                                 comprometer a implementação do plano 
                                 estratégico da Instituição?",
                         q5_2 = "Com a implantação do sistema de gestão 
                                 de riscos, as leis e os regulamentos 
                                 aplicáveis estão sendo cumpridos com mais 
                                   facilidade (compliance)?",
                         q5_3 = "Os riscos da organização estão dentro dos 
                                 seus critérios de risco, vale dizer, 
                                 dentro do apetite a risco definido e das 
                                 variações aceitáveis no desempenho ou 
                                 tolerâncias a risco estabelecidas?",
                         q5_4 = "A gestão de riscos é capaz de promover a 
                                 redução de custos por meio do desenvolvimento 
                                 de sinergias entre unidades de negócios e 
                                 departamentos?",
                         q5_5 = "A gestão de riscos melhora a transparência 
                                 para as partes interessadas, reduzindo o 
                                 escrutínio regulatório e despesas com 
                                 litígios?",
                         q5_6 = "A gestão de riscos pode promover a 
                                 integridade e prevenir a improbidade, os 
                                   desvios e a corrupção?",
                         q5_7 = "A gestão de riscos mostrou ser capaz de 
                                 reduzir surpresas e prejuízos operacionais 
                                   na Instituição?",
                         q5_8 = "A gestão de riscos pode auxiliar na 
                                 identificação de problemas atuais e 
                                 emergentes da Instituição, gerando maior 
                                 confiabilidade?",
                         q5_9 = "A gestão de riscos auxilia a monitorar a 
                                 adequação e a eficácia do controle interno?",
                         q5_10 = "A gestão de risco pode ser capaz de permitir 
                                  estabilidade orçamentária, prevenindo 
                                  surpresas ao elaborar o orçamento?"
                           )


## Analise via pacote likert 

grupo5_likert <- likert(dados_grupo5)

### grafico de barras empilhadas basico
g3 <- plot(grupo5_likert, 
           type = "bar", 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = TRUE,
           plot.percent.high = TRUE,
           plot.percent.neutral = TRUE,
           plot.percents = FALSE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           ylabel = "Porcentagem",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = FALSE,
           zero.print = TRUE)

### salvando g1
ggsave(filename = "fig03.png",
       plot = g3,
       width = 13,
       height = 9,
       dpi = 600,
       units = "in")

# grafico de barras empinhadas com porcentagems por nivel
g4 <- plot(grupo5_likert, 
           low.color = "#D8B365",
           high.color = "#5AB4AC",
           neutral.color = "grey90",
           neutral.color.ramp = "white",
           colors = NULL,
           plot.percent.low = FALSE,
           plot.percent.high = FALSE,
           plot.percent.neutral = TRUE,
           plot.percents = TRUE,
           text.size = 3,
           text.color = "black",
           centered = TRUE,
           include.center = TRUE,
           ordered = TRUE,
           wrap = 50,
           wrap.grouping = 50,
           legend = "Resposta",
           ylabel = "Porcentagem",
           legend.position = "bottom",
           panel.arrange = "v",
           panel.strip.color = "#F0F0F0",
           digits = 2,
           drop0trailing = FALSE,
           zero.print = TRUE)

# salvando g2
ggsave(filename = "fig04.png",
       plot = g4,
       width = 13,
       height = 9,
       dpi = 600,
       units = "in")

