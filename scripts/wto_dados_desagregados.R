# WTO -- Dados desagregados por pais
# Script GIARS
# Neylson e Italo

library(magrittr)
library(readr)
library(gdata)


path <- "C:/Users/giars/Documents/Neylson/WTO//2012/" #o caminho do diretorio (ajustar)
files <- list.files(path = path, pattern='[.]xls')    #lista os nomes dos arquivos
sites <- paste0(path,files)                           #cola as duas informacoes para realizar o loop


rede <- data.frame()
contador <- 1

for (i in 1:length(sites)){
print(contador)

#importando os dados
#E necessario ter o PERL instalado. Consultar https://www.perl.org/get.html
df <- read.xls(sites[i], stringsAsFactors=F)
#View(df)

#pais em questao
pais <- df[1,1]

#separando as colunas de interesse - lacos e pesos
dados <- df[42:46,c(1,6,8,13)]
#dados

#limpando os dados
dados[[2]] %<>% gsub(" ","",.) %>% as.numeric
dados[[4]] %<>% gsub(" ","",.) %>% as.numeric
dados[[1]] %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.)
dados[[3]] %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.)
#dados


# montando a rede
rede.out <- data.frame(pais, dados[[1]], weights=dados[[2]])
rede.in  <- data.frame(dados[[3]], pais, weights=dados[[4]])
names(rede.out) <- c("out","in","weight")
names(rede.in) <- c("out","in","weight")
rede     <- rbind(rede, rede.out, rede.in)

contador <- contador+1
}


#rede <- na.omit(rede)
library(igraph)
g <- graph_from_edgelist(as.matrix(rede[,1:2]), directed = T)
g <- delete.vertices(g, V(g)[37]) #deleta um no NA
E(g)$weight = rede[[3]]
plot(g, edge.width=as.numeric(E(g)$weight)/10, edge.arrow.size=.2, vertex.size=4, vertex.label.cex=.7,edge.curved=T)