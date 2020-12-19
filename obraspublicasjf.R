library(readxl)
library(lubridate)
library(stringr)
library(dplyr)
library(ggplot2)
library(plm)
library(haven)
library(tidyr)
library(reshape2)
library(geobr)
library(xlsx)
library(psych)
library(readr)

#OBRAS PÚBLICAS



segsem2020 <- read_excel("C:/Users/Matheus/Desktop/jotaefe/segsemestre2020.xlsx", 
                               col_types = c("text", "numeric", "text", 
                                                  "date", "date", "numeric", "numeric", 
                                             "text"))%>%
  #tirando valores sem identificação
  filter(Objeto != is.na(Objeto), `Valor total` != is.na(`Valor total`))%>%
  #gerando dummies para obras fora do prazo, atrasadas pela execução financeira e concluidas
  mutate(foradoprazo = case_when(`Data prevista para término ou prazo de execução` < '2020-09-08'
         & `Situação atual da obra` != 'Concluída' ~1),
         concluida = case_when(`Situação atual da obra` == 'Concluída' ~1),
         atrasada = case_when(`Data prevista para término ou prazo de execução` < '2020-12-31'
                             & `Percentual de execução financeira`< 0.5 ~1))%>%
  replace_na(list(foradoprazo =0 ,concluida = 0, atrasada = 0))%>%
  mutate(obra=1)


#estatísticas
segsem2020%>%
  count(atrasada, foradoprazo)


#ja devia ter acabado ne rs

obras2020%>%
  count()










