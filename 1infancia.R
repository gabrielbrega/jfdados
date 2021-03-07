#carregando bibliotecas
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


#abrindo a base
primeira_infancia_municipios <- read_csv("primeira_infancia_municipios.csv", 
                                     col_types = cols(id_municipio = col_character()))


#base para comparacao em minas - jf, bh,betim, contagem, uberlandia, montes claros

primeirainf_zonadamata <- primeira_infancia_municipios%>%
  mutate(ano = as.numeric(ano))%>%
  filter(id_municipio == '3136702' |id_municipio == '3106200'|
           id_municipio == '3170206'| id_municipio == '3118601'|
           id_municipio == '3143302'| id_municipio == '3106705')%>%
  mutate(nomemun = case_when(id_municipio == '3136702' ~ 'Juiz de Fora',
         id_municipio == '3106200' ~ 'Belo Horizonte',
         id_municipio == '3170206' ~ 'Uberlândia',
         id_municipio == '3118601' ~ 'Contagem',
         id_municipio == '3106705' ~ 'Betim',
         id_municipio == '3143302' ~ 'Montes Claros'))%>%
  mutate(jf = case_when(id_municipio == '3136702' ~'1' ))%>%
  replace_na(list(jf = 0))%>%
  pivot_wider(
              names_from = nomemun,
              values_from = taxa_liquida_mat_pre_escola)

rio::export(primeirainf_zonadamata, "primeirainfanciapronto3.csv")

#plotando a matricula na pre escola nas maiores cidades de minas

ggplot(primeirainf_zonadamata, aes(x = ano ,y = taxa_bruta_mat_pre_escola, color =nomemun )) + 
  geom_line(size = 1.9)+
  labs(x = "Ano",
       y = "Taxa de Matrícula (em %)",
       title = "Avanço das taxas de matrícula na pré-escola em Juiz de Fora")+
  theme_classic()

#plotando a matricula na pre escola nas maiores cidades de minas - gerando contraste

ggplot(primeirainf_zonadamata, aes(x = ano ,y = taxa_bruta_mat_pre_escola, color =jf )) + 
  geom_line(size = 1.9)+
  labs(x = "Ano",
       y = "Taxa de Matrícula (em %)",
       title = "Avanço das taxas de matrícula na pré-escola em Juiz de Fora")+
  theme_classic()




#base so para juiz de fora
primeirainf_jf <- primeira_infancia_municipios%>%
  filter(id_municipio == '3136702',
         ano> 2015)
  

#evolução da matricula na pre-escola só juiz de fora

ggplot(primeirainf_jf, aes(x = ano , group = 1)) + 
  geom_line(aes(y = taxa_bruta_mat_pre_escola, color = "Taxa Bruta"), size=1.9)+
  geom_line(aes(y = taxa_liquida_mat_pre_escola, color = "Taxa Líquida"), size=1.9)+
  labs(x = "Ano",
       y = "Taxa de Matrícula (em %)",
       title = "Avanço das taxas de matrícula na pré-escola em Juiz de Fora")+
  scale_color_manual(name = "Tipo de Taxa",
                     values = c( "Taxa Bruta" = "blue", "Taxa Líquida"  = "red"),
                     labels = c("Taxa Bruta", "Taxa Líquida"))+
  theme_classic()









