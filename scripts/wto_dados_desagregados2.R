# Rede WTO tempo 2

path <- "/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/2011/" #o caminho do diretorio (ajustar)
files <- list.files(path = path, pattern='[.]xls')    #lista os nomes dos arquivos
sites <- paste0(path,files)                           #cola as duas informacoes para realizar o loop


# Criando os data frames vazios para preencher e o contador
rede.out2 <- data.frame()
rede.in2 <- data.frame()
atributos2 <- data.frame()
contador <- 1

##############################################################
# Efetuando a coleta de dados via for loop
for (i in 1:length(sites)){
  print(contador)
  
  #importando os dados
  #E necessario ter o PERL instalado. Consultar https://www.perl.org/get.html
  df <- read.xls(sites[i], stringsAsFactors=F, encoding="UTF-8")
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
  rede.out2  <- rbind(rede.out2, rede.out1)
  rede.in2   <- rbind(rede.in2, rede.in1)
  
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
  atributos2 <- rbind(atributos2, atrib)
  
  contador <- contador+1
}
colnames(atributos2)[1] <- "Country"
attr.name <- df[c(3:8,32,33,35),1]
colnames(atributos2)[2:10] <- attr.name
##############################################################


# Rede exportacao
rede.out2 <- na.omit(rede.out2)
g.out2 <- graph_from_edgelist(as.matrix(rede.out2[,1:2]), directed = T)
E(g.out2)$weight = rede.out2[[3]]
V(g.out2)$name[71] <- "CotedIvoire"

# Atributos
name2 <- V(g.out2)$name
nomes.df2 <- data.frame(name2, 1:length(name2))
nomes.df2 <- nomes.df2[order(nomes.df2$name2),]
atributos2$Country[33] <- "CotedIvoire"
atributos2 <- atributos2[order(atributos2$Country),]
nomes.df.merge2 <- merge(nomes.df2, atributos2, by.x = "name2", by.y = "Country", all.x = T, incomparables = NA)
atributos.order2 <- nomes.df.merge2[order(nomes.df.merge2$X1.length.name2.),]
names(atributos.order2)[2] <- "id"
atributos.order2$name2 %<>% as.character

#Adicionando atributos
V(g.out2)$populacao <- atributos.order2[[3]]
V(g.out2)$GDP       <- atributos.order2[[4]]
V(g.out2)$balance   <- atributos.order2[[6]]
V(g.out2)$tradepercapita <- atributos.order2[[7]]
V(g.out2)$tradeGDP  <- atributos.order2[[8]]
V(g.out2)$exports   <- atributos.order2[[9]]
V(g.out2)$imports   <- atributos.order2[[10]]
V(g.out2)$share     <- atributos.order2[[11]]

plot(g.out2, edge.arrow.size=.2, vertex.size=4,vertex.label.cex=.7,edge.curved=T, main="Exports")

# Rede importacao
rede.in2 <- na.omit(rede.in2)
g.in2 <- graph_from_edgelist(as.matrix(rede.in2[,1:2]), directed = T)
E(g.in2)$weight = rede.in2[[3]]
V(g.in2)$name[36] <- "CotedIvoire"
plot(g.in2, edge.arrow.size=.2, vertex.size=4, vertex.label.cex=.7,edge.curved=T, main="Imports")
