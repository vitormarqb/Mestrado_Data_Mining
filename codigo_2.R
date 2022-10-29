library(here)
library(tidyverse)
library(tidymodels)
library(dplyr)
library(rpart)
library(ggplot2)
library(utils)
library(skimr)
library(rpart)

ideb_dados <- read_csv("ideb_escolas.csv")

ideb_dados$ano <- as.numeric(ideb_dados$ano)
ideb_dados$sigla_uf <- as.factor(ideb_dados$sigla_uf)



df <- ideb_dados $>$ filter(ano >= 2019) $>$ filter((sigla_uf == "MG") | (sigla_uf == "SP")) $>$filter( taxa_aprovacao != "S/R", indicador_rendimento != "S/R", ideb != "S/R", projecao != "S/R" )
df$taxa_aprovacao                <- as.factor(df$taxa_aprovacao)
df$indicador_rendimento          <- as.factor(df$indicador_rendimento)
df$nota_saeb_matematica          <- as.factor(df$nota_saeb_matematica)
df$nota_saeb_lingua_portuguesa   <- as.factor(df$nota_saeb_lingua_portuguesa)
df$nota_saeb_media_padronizada   <- as.factor(df$nota_saeb_media_padronizada)
df$ensino <- as.factor(df$ensino)
df$rede <- as.factor(df$rede)
df$anos_escolares <- as.factor(df$anos_escolares)
df$ideb <- as.numeric(df$ideb)
df$projecao <- as.factor(df$projecao)

df$id_municipio <- NULL
df$id_escola <- NULL
View(df)


testDf <- head(df,50)


dataset_split <- initial_split(testDf, prop = 0.7)
A_treino  <- training(dataset_split)
A_teste   <- testing(dataset_split)


library(rpart)
modelo <- rpart(`ideb` ~., data=A_treino, control=rpart.control(cp=0))

testDf$pred <- predict(modelo,testDf)

View(testDf)



