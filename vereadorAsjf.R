#bibliotecarios
library(tidyverse)
library(DBI)
library(bigrquery)

#diretorio
setwd("C://Users/Matheus/Desktop/jotaefe/jfcoisas")


# pegando as coisas do bd+
bq_auth(path = "chavebigquery/My Project 55562-2deeaad41d85.json")
id_projeto <- "double-voice-305816"

con <- dbConnect(
  bigrquery::bigquery(),
  billing = id_projeto,
  project = "basedosdados"
)

# Executando a query
query <- 'SELECT tse.ano, tse.id_municipio_tse, tse.sigla_partido, tse.resultado, tse.votos, tse.id_candidato_bd,tse.turno, tse.cargo
FROM `basedosdados.br_tse_eleicoes.resultados_candidato_municipio` as tse
WHERE tse.id_municipio_tse = 47333'

vereadoresjf <- dbGetQuery(con, query)

#query dos sexos
query2 <- 'SELECT cand.genero, cand.idade, cand.id_candidato_bd, cand.ano FROM `basedosdados.br_tse_eleicoes.candidatos` as cand
WHERE id_municipio_tse = 47333'

sexovereadoresjf <- dbGetQuery(con, query2)

# base conjunta --------------------------

vereadoresfinal <- vereadoresjf%>%
  inner_join(sexovereadoresjf, by = c("id_candidato_bd", "ano"))%>%
  mutate(candidato=1,
         margarida = case_when())


# candidatas
qtasmulheres <- vereadoresfinal%>%
  filter(turno == 1)%>%
  group_by(ano, genero)%>%
  summarize(totaldecandidatos = sum(candidato))%>%
  pivot_wider(names_from = genero,
              values_from = totaldecandidatos)

rio::export(qtasmulheres, "znumerodecandmulheres.csv")

# eleitas

qtasmulhereseleitas <- vereadoresfinal%>%
  filter(turno ==1,
         resultado == "eleito" | resultado == "eleito por qp" | resultado == "eleito por media")%>%
  group_by(ano, genero,cargo)%>%
  summarize(totaldecandidatos = sum(candidato))%>%
  pivot_wider(names_from = genero,
              values_from = totaldecandidatos)

rio::export(qtasmulhereseleitas, "zmulhereseleitas.csv")

margarida <- vereadoresfinal%>%
  filter(turno ==2, cargo == "prefeito")
  
  


#n de votos
numerodevotos <- vereadoresfinal%>%
  filter(cargo == 'prefeito')%>%
  group_by(ano, genero, turno)%>%
  summarize(totaldevotos = sum(votos))%>%
  pivot_wider(names_from = c(genero,turno),
              values_from = totaldevotos)

rio::export(numerodevotos, "zvotos.csv")

# partido com mais mulheres
partidomulheres <- vereadoresjf2%>%
  filter(mulher == 1,
         ano %in% c(2016,2020))%>%
  group_by(sigla_partido, ano)%>%
  summarise(totaldecandidatas = sum(candidato))



















