# WTO -- Dados desagregados por pais
# Script GIARS
# Neylson e Italo

library(magrittr)
library(gdata)
library(igraph)

my.to.numeric <- function(x){
  library(magrittr)
  x %<>% as.character %>% as.numeric
  return(x)
}

path <- "~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/2010/" #o caminho do diretorio (ajustar)
files <- list.files(path = path, pattern='[.]xls')    #lista os nomes dos arquivos
sites <- paste0(path,files)                           #cola as duas informacoes para realizar o loop

# Criando os data frames vazios para preencher e o contador
rede.out <- data.frame()
rede.in <- data.frame()
atributos <- data.frame()
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
  attr.values <- df[c(3:8,32,33,35),6]
  attr.values %<>% gsub(" ","",.) %>% as.numeric
  
  attr <- c()
  attr[1] <- pais
  attr[2:10] <- attr.values
  atrib <- data.frame()
  atrib <- rbind(atrib, attr)
  atrib[,1] %<>% as.character
  atrib[,2:10] %<>% apply(., 2, my.to.numeric)
  names(atrib) <- 1:10
  atributos <- rbind(atributos, atrib)
  
  contador <- contador+1
}
colnames(atributos)[1] <- "Country"
attr.name <- df[c(3:8,32,33,35),1]
colnames(atributos)[2:10] <- attr.name
##############################################################


# Rede exportacao
rede.out <- na.omit(rede.out)
g.out <- graph_from_edgelist(as.matrix(rede.out[,1:2]), directed = T)
E(g.out)$weight = rede.out[[3]]
V(g.out)$name[72] = "CotedIvoire"

# Atributos
name1 <- V(g.out)$name
nomes.df <- data.frame(name1, 1:length(name1))
nomes.df <- nomes.df[order(nomes.df$name1),]
atributos$Country[33] <- "CotedIvoire"
atributos <- atributos[order(atributos$Country),]
nomes.df.merge <- merge(nomes.df, atributos, by.x = "name1", by.y = "Country", all.x = T, incomparables = NA)
atributos.order <- nomes.df.merge[order(nomes.df.merge$X1.length.name1.),]
names(atributos.order)[2] <- "id"
atributos.order$name1 %<>% as.character

#Verificando quais países não tem informações
sem.na <- atributos.order %>% na.omit(.) %>% .[[2]]
excluir <- which(atributos.order[[2]] %in% sem.na ==F)

#Retirando os países
atributos.order %<>% na.omit
g.out <- delete_vertices(g.out, excluir)

#Adicionando atributos
V(g.out)$populacao <- atributos.order[[3]]
V(g.out)$GDP       <- atributos.order[[4]]
V(g.out)$balance   <- atributos.order[[6]]
V(g.out)$tradepercapita <- atributos.order[[7]]
V(g.out)$tradeGDP  <- atributos.order[[8]]
V(g.out)$exports   <- atributos.order[[9]]
V(g.out)$imports   <- atributos.order[[10]]
V(g.out)$share     <- atributos.order[[11]]
plot(g.out, edge.arrow.size=.2, vertex.size=4,vertex.label.cex=.7,edge.curved=T, main="Exports")

# Rede importacao
rede.in <- na.omit(rede.in)
g.in <- graph_from_edgelist(as.matrix(rede.in[,1:2]), directed = T)
E(g.in)$weight = rede.in[[3]]
plot(g.in, edge.arrow.size=.2, vertex.size=4, vertex.label.cex=.7,edge.curved=T, main="Imports")
