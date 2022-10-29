# Vitor Marques Barbosa
# Mineração de Dados - UFU


# Bibliotecas
library(here)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(rpart)
library(ggplot2)
library(utils)
library(skimr)
library(baguette)
library(doFuture)


# Lendo Arquivo CSV
ideb_escolas <- read_csv("ideb_escolas.csv")
View(ideb_escolas)

# Removendo Valores Nulos
na.omit(ideb_escolas)

# Modelagem
ideb_escolas$ano <- as.numeric(ideb_escolas$ano)
ideb_escolas$sigla_uf <- as.factor(ideb_escolas$sigla_uf)


df <- select(ideb_escolas, -c (id_municipio, ensino, projecao)) %>%
  filter( taxa_aprovacao != "S/R", indicador_rendimento != "S/R", ideb != "S/R", sigla_uf == "MG" )



df$taxa_aprovacao                <- as.factor(df$taxa_aprovacao)
df$indicador_rendimento          <- as.factor(df$indicador_rendimento)
df$nota_saeb_matematica          <- as.factor(df$nota_saeb_matematica)
df$nota_saeb_lingua_portuguesa   <- as.factor(df$nota_saeb_lingua_portuguesa)
df$nota_saeb_media_padronizada   <- as.factor(df$nota_saeb_media_padronizada)
df$rede                          <- as.factor(df$rede)
df$anos_escolares                <- as.factor(df$anos_escolares)
df$ideb                          <- as.numeric(df$ideb)


#----------------------------------------------------------------------------------------






# dados de treino e teste
testDf <- head(df,50)

dataset_split <- initial_split(testDf, prop = 0.7)
A_treino  <- training(dataset_split)
A_teste   <- testing(dataset_split)


# Checando se o split deu certo
summary(testDf)
summary(A_treino)
summary(A_teste)


# Primeiro Algoritmo
ml_1 <- rpart(`anos_escolares` ~., data = A_treino, control = rpart.control(cp=0))

A_teste$resultado <- predict(ml_1, teste)

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------



# Preparação para o Segundo Algoritmo
reamostragem <- bootstraps(A_treino, times = 5) 

receita <- recipe(anos_escolares ~ . , testDf)

juice(prep(receita)) $>$ skim()
juice(prep(receita)) $>$ glimpse()


# Segundo Algoritmo - Bagging: Decisão por Votação


arv_decisao <- decision_tree(mode = "classification", tree_depth = tune(), min_n = 1) #CART alta variancia
knn <- nearest_neighbor(mode = "classification", neighbors = tune()) # alta variancia ponderação por vizinhos
bag_de_arv <- bag_tree(mode = "classification", tree_depth = tune(), min_n = 1) # bagging de CART
floresta <- rand_forest(mode = "classification", mtry = tune(), min_n = 1, trees = 10) # trees = 1000 #bagging "generica" sem poda



combinacoes <- workflow_set(
  preproc = list(receita),
  models = list("arvore" = arv_decisao,
                "knn" = knn,
                "bag_de_arv" = bag_de_arv,
                "floresta" = floresta))


res <- combinacoes |> workflow_map(resamples = reamostragem, grid = 5, verbose = T)          

autoplot(res, metric = "roc_auc")


























#--------------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------------

# Vamos fazer a modelagem para o segundo Dataset
# Separando os dados de treino e teste.

view(dataset_B)



# Transformando as colunas em factor
dataset_B$taxa_aprovacao                <- as.factor(dataset_B$taxa_aprovacao)
dataset_B$indicador_rendimento          <- as.factor(dataset_B$indicador_rendimento)
dataset_B$nota_saeb_matematica          <- as.factor(dataset_B$nota_saeb_matematica)
dataset_B$nota_saeb_lingua_portuguesa   <- as.factor(dataset_B$nota_saeb_lingua_portuguesa)
dataset_B$nota_saeb_media_padronizada   <- as.factor(dataset_B$nota_saeb_media_padronizada)
#--------------------------------------------------------------------------------------------------------

# Separando em dados de treino e teste
dataset_split_2 <- initial_split(dataset_B, prop = 0.7)
B_treino  <- training(dataset_split_2)
B_teste   <- testing(dataset_split_2)


# Checando se o split deu certo
summary(dataset_B)
summary(B_treino)
summary(B_teste)
#--------------------------------------------------------------------------------------------------------

# Construção do 1ª Algoritmo de Classificação

ml_2 <- rpart(`ideb` ~., data = B_treino, control = rpart.control(cp=0))


