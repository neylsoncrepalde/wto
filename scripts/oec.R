# OEC data
# International Trade
# Script: Neylson Crepalde

library(readr)
library(descr)
library(igraph)

dados <- read_tsv("D:/Neylson Crepalde/OEC/year_origin_destination_sitc_rev2.tsv")

head(dados)
freq(dados$sitc)
length(levels(factor(dados$sitc)))
View(dados[1:50,])

which(dados$origin == "civ")
which(dados$dest == "civ")

#for (row in 1:nrow(dados$origin))

#rede_binaria <- graph_from_edgelist(as.matrix(unique(dados[,2:3])), 
#                                   directed = T)
# Nao esta valendo pq preciso separar por volume e por export import
