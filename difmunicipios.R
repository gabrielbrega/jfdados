
library(tidyverse)


base_fundamental <- rio::import("escolasjf2017.csv")%>%
  filter(!ideb == is.na(ideb),
         ensino == "fundamental")

base_medio <- rio::import("escolasjf.csv")%>%
  filter(!ideb == is.na(ideb), 
    ensino == "medio")%>%
  pivot_wider(names_from = rede  ,
              values_from = c(nota_saeb_matematica, nota_saeb_lingua_portuguesa, ideb))

rio::export(base_medio, "basemediojf2.csv")

########################### DIFERENTES MUNICIPIOS

basemun <- rio::import("pormunicipioescola.csv")%>%
  mutate(nomemun = case_when(id_municipio == '3136702' ~ 'Juiz de Fora',
                             id_municipio == '3106200' ~ 'Belo Horizonte',
                             id_municipio == '3170206' ~ 'Uberlândia',
                             id_municipio == '3118601' ~ 'Contagem',
                             id_municipio == '3106705' ~ 'Betim',
                             id_municipio == '3143302' ~ 'Montes Claros'))
basemunpriv <- basemun%>%
  filter(rede == "privada")

basemunpub <- basemun%>%
  filter(rede == "publica")


rio::export(basemunpub, "basemunpub.csv")
rio::export(basemunpriv, "basemunpriv.csv")
