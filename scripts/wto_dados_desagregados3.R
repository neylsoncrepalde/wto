# Rede WTO tempo 3

path <- "/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/2014/" #o caminho do diretorio (ajustar)
files <- list.files(path = path, pattern='[.]xls')    #lista os nomes dos arquivos
sites <- paste0(path,files)                           #cola as duas informacoes para realizar o loop


# Criando os data frames vazios para preencher e o contador
rede.out3 <- data.frame()
rede.in3 <- data.frame()
atributos3 <- data.frame()
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
  rede.out3  <- rbind(rede.out3, rede.out1)
  rede.in3   <- rbind(rede.in3, rede.in1)
  
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
  atributos3 <- rbind(atributos3, atrib)
  
  contador <- contador+1
}
colnames(atributos3)[1] <- "Country"
attr.name <- df[c(3:8,32,33,35),1]
colnames(atributos3)[2:10] <- attr.name
##############################################################


# Rede exportacao
rede.out3 <- na.omit(rede.out3)
g.out3 <- graph_from_edgelist(as.matrix(rede.out3[,1:2]), directed = T)
E(g.out3)$weight = rede.out3[[3]]
V(g.out3)$name[12] <- "Curacao"
V(g.out3)$name[47] <- "CotedIvoire"

# Atributos
name3 <- V(g.out3)$name
nomes.df3 <- data.frame(name3, 1:length(name3))
nomes.df3 <- nomes.df3[order(nomes.df3$name3),]
atributos3$Country[35] <- "CotedIvoire"
atributos3$Country[43] <- "Curacao"
atributos3 <- atributos3[order(atributos3$Country),]
nomes.df.merge3 <- merge(nomes.df3, atributos3, by.x = "name3", by.y = "Country", all.x = T, incomparables = NA)
atributos.order3 <- nomes.df.merge3[order(nomes.df.merge3$X1.length.name3.),]
names(atributos.order3)[2] <- "id"
atributos.order3$name3 %<>% as.character

#Verificando quais países não tem informações
sem.na <- atributos.order3 %>% na.omit(.) %>% .[[2]]
excluir <- which(atributos.order3[[2]] %in% sem.na ==F)

#Retirando os países
atributos.order3 %<>% na.omit
g.out3 <- delete_vertices(g.out3, excluir)

#Adicionando atributos
V(g.out3)$populacao <- atributos.order3[[3]]
V(g.out3)$GDP       <- atributos.order3[[4]]
V(g.out3)$balance   <- atributos.order3[[6]]
V(g.out3)$tradepercapita <- atributos.order3[[7]]
V(g.out3)$tradeGDP  <- atributos.order3[[8]]
V(g.out3)$exports   <- atributos.order3[[9]]
V(g.out3)$imports   <- atributos.order3[[10]]
V(g.out3)$share     <- atributos.order3[[11]]

plot(g.out3, edge.arrow.size=.2, vertex.size=4,vertex.label.cex=.7,edge.curved=T, main="Exports")

# Rede importacao
rede.in3 <- na.omit(rede.in3)
g.in3 <- graph_from_edgelist(as.matrix(rede.in3[,1:2]), directed = T)
E(g.in3)$weight = rede.in3[[3]]
V(g.in3)$name[36] <- "CotedIvoire"
V(g.in3)$name[59] <- "Curacao"
plot(g.in3, edge.arrow.size=.2, vertex.size=4, vertex.label.cex=.7,edge.curved=T, main="Imports")
