#biblioteca
library(tidyverse)
library(rio)

#importando
bf <- import("bfbeneficiarios.csv")%>%
  mutate(anomes = case_when(mes %in% 1:9 ~ str_c("0",mes,"-",ano),
                            mes %in% 10:12 ~ str_c(mes,"-", ano)))%>%
  filter(ano>= 2005)

# base serie temporal
export(bf, "seriebeneficiariosjf.csv")

#foco na pandemics

bf_pandemics <- bf%>%
  filter(ano %in% 2019:2020)

export(bf_pandemics, "bfbenefpandemia.csv")
