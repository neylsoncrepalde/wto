# WTO -- Dados desagregados por pais
# Script GIARS
# Neylson e Italo

library(magrittr)
library(gdata)
library(igraph)

path <- "C:/Users/giars/Documents/Neylson/WTO/2012/" #o caminho do diretorio (ajustar)
files <- list.files(path = path, pattern='[.]xls')    #lista os nomes dos arquivos
sites <- paste0(path,files)                           #cola as duas informacoes para realizar o loop

# Criando os data frames vazios para preencher e o contador
rede.out <- data.frame()
rede.in <- data.frame()
contador <- 1

##############################################################
# Efetuando a coleta de dados via for loop
for (i in 1:length(sites)){
print(contador)

#importando os dados
#E necessario ter o PERL instalado. Consultar https://www.perl.org/get.html
df <- read.xls(sites[i], stringsAsFactors=F)
#View(df)

#pais em questao e limpeza
pais <- df[1,1]
pais %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.) %>% gsub("\\s","",.)

#separando as colunas de interesse - lacos e pesos
dados <- df[42:46,c(1,6,8,13)]
#dados

#limpando os dados
dados[[2]] %<>% gsub(" ","",.) %>% as.numeric
dados[[4]] %<>% gsub(" ","",.) %>% as.numeric
dados[[1]] %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.) %>% gsub("\\s","",.)
dados[[3]] %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.) %>% gsub("\\s","",.)
#dados


# montando a lista de lacos
rede.out1 <- data.frame(pais, dados[[1]], weights=dados[[2]])
rede.in1  <- data.frame(dados[[3]], pais, weights=dados[[4]])
rede.out  <- rbind(rede.out, rede.out1)
rede.in   <- rbind(rede.in, rede.in1)

#capturando os atributos
  #colnames(attr)[1] <- "pais"
  #attr.name <- df[3:8,1]
  #attr.values <- df[3:8,6]
  #attr.values %<>% gsub(" ","",.) %>% as.numeric

contador <- contador+1
}
##############################################################


# Rede exportacao
rede.out <- na.omit(rede.out)
g.out <- graph_from_edgelist(as.matrix(rede.out[,1:2]), directed = T)
E(g.out)$weight = rede.out[[3]]
plot(g.out, edge.arrow.size=.2, vertex.size=4,vertex.label.cex=.7,edge.curved=T, main="Exports")

# Rede importacao
rede.in <- na.omit(rede.in)
g.in <- graph_from_edgelist(as.matrix(rede.in[,1:2]), directed = T)
E(g.in)$weight = rede.in[[3]]
plot(g.in, edge.arrow.size=.2, vertex.size=4, vertex.label.cex=.7,edge.curved=T, main="Imports")
