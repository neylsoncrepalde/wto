# WTO Temporal analysis
# ANALYSIS
# Neylson Crepalde

#source("C:/Users/neyls/Documents/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados.R")
#source("C:/Users/neyls/Documents/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados2.R")
#source("C:/Users/neyls/Documents/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados3.R")

library(statnet)
library(intergraph)
library(ndtv)
library(ineq)

########################
atributos[,11] <- 2010
names(atributos)[11] <- "Year"
atributos2[,11] <- 2011
names(atributos2)[11] <- "Year"
atributos3[,11] <- 2014
names(atributos3)[11] <- "Year"

############################
# Calculando o GINI e plotando a Lorenz Curve
ineq(atributos[[10]], type = "Gini")
plot(Lc(atributos[[10]]), col="darkred", lwd=2)

#######################
n.out <- network(as.matrix(get.adjacency(g.out)),directed=T)
n.out %e% "weight" <- rede.out[,3]
n.out2 <- asNetwork(g.out2)
n.out3 <- asNetwork(g.out3)
plot(n.out, main="WTO network, t1")


wto.out <- networkDynamic(network.list = list(n.out, n.out2, n.out3),
                          vertex.pid = "vertex.names", create.TEAs = T)

render.animation(wto.out, render.par = list(tween.frames=1, show.time=T))
filmstrip(wto.out, frames=4, displaylabels=F)
proximity.timeline(wto.out)

formation <- formula(~edges+gwesp(1,fixed=T)+gwodegree(1)+edges.ageinterval)
dissolution <- formation
#tempfit <- stergm(wto.out, formation, dissolution, estimate = "CMLE")

