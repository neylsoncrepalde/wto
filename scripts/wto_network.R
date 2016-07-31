#Trabalhando com os dados WTO desagregados por produto
#Neylson Crepalde
#GIARS

library(magrittr)
library(bit64)
library(data.table)
library(descr)
library(ggplot2)
library(igraph)
library(intergraph)
library(statnet)

keep = c(5,6,9,10,11,12,15,16)
dados <- as.data.frame(fread("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/ne2_e.csv",
               select = keep))

dados <- dados[dados$Country_desc!="World",]
USA <- dados[dados$Country_code=="US" & dados$Indicator_desc == "Total merchandise",]

ggplot(USA[USA$Partner_Country_desc=="China",], aes(x=Year, y=Value/1000000))+geom_path()+
  geom_point()

#############################
#Extraindo as redes por ano
dados.2000 <- dados[dados$Year==2000,]
dados.2001 <- dados[dados$Year==2001,]
dados.2002 <- dados[dados$Year==2002,]
dados.2003 <- dados[dados$Year==2003,]
dados.2004 <- dados[dados$Year==2004,]
dados.2005 <- dados[dados$Year==2005,]
dados.2006 <- dados[dados$Year==2006,]
dados.2007 <- dados[dados$Year==2007,]
dados.2008 <- dados[dados$Year==2008,]
dados.2009 <- dados[dados$Year==2009,]
dados.2010 <- dados[dados$Year==2010,]
dados.2011 <- dados[dados$Year==2011,]
dados.2012 <- dados[dados$Year==2012,]
dados.2013 <- dados[dados$Year==2013,]
dados.2014 <- dados[dados$Year==2014,]

##################################
#Montando as redes
g.2000 <- graph_from_edgelist(as.matrix(dados.2000[,c(2,6)]), directed = T)
g.2000
plot(g.2000, vertex.label.cex=.8, edge.curved=T, edge.arrow.size=.3)
