# Descricao ---------------------------------------------------------------

## Autor: Prof. Washington S. Silva                                      
## Analise dos dados simulados dados do questionário de Araujo & Gomes (2021)
## Ultima alteração: 08/08/2022


# Pacotes utilizados ------------------------------------------------------

library(readr)
library(dplyr)
library(ggplot2)
library(scales)
library(cluster) # for gower similarity and pam
library(Rtsne) # for t-SNE plot


# Importando os dados simulados -------------------------------------------

path <- "C:/Users/Usuario/Desktop/github/gestao_risco/dados_simulados/dados_categoricos.rds"
dados_categoricos <- read_rds(path)
glimpse(dados_categoricos)


dados_grupo4 <- dados_categoricos %>%
  select(q1_9, q4_1, q4_2, q4_3, q4_4, q4_5, q4_6, q4_7, q4_8, 
         q4_9, q4_10, q4_11, q4_12)
glimpse(dados_grupo4)

## calculo da distancia de Gower

dist_gower <- daisy(dados_grupo4[, -1],
                    metric = "gower",
                    type = list(ordratio = 12))

summary(dist_gower)

gower_matriz <- as.matrix(dist_gower)


# Output most similar pair

dados_grupo4[
  which(gower_matriz == min(gower_matriz[gower_matriz != min(gower_matriz)]),
        arr.ind = TRUE)[1, ],]

# Output most dissimilar pair

dados_grupo4[
  which(gower_matriz == max(gower_matriz[gower_matriz != max(gower_matriz)]),
        arr.ind = TRUE)[1, ], ]


# Selecting the number of clusters

## Calculate silhouette width for many k using PAM

sil_width <- c(NA)

for(i in 2:10){
  
  pam_fit <- pam(dist_gower,
                 diss = TRUE,
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
}


# Plot sihouette width (higher is better)

plot(1:10, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:10, sil_width)


# Cluster Interpretation

## Via Descriptive Statistics

pam_fit <- pam(gower_matriz, diss = TRUE, k = 7)

pam_results <- dados_grupo4 %>%
  dplyr::select(-q1_9) %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

pam_results$the_summary

dados_grupo4[pam_fit$medoids, ]


## Via Visualization

sne_obj <- Rtsne(gower_matriz, is_distance = TRUE, resolution = 0.5, perplexity = 2)
# tsne_obj <- Rtsne(gower_matriz, is_distance = TRUE)


tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering),
         name = dados_grupo4$q1_9)

ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))


tsne_data %>%
  filter(X > 15 & X < 25,
         Y > -15 & Y < -10) %>%
  left_join(dados_grupo4, by = "name") %>%
  collect %>%
  .[["name"]]