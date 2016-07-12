# WTO Temporal analysis
# ANALYSIS
# Neylson Crepalde

source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados2.R")
source("/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados3.R")

library(statnet)
library(intergraph)
library(ndtv)
library(ineq)
library(ggplot2)

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
ineq(atributos2[[10]], type="Gini")
ineq(atributos3[[10]], type="Gini")

par(mfrow=c(1,2))
plot(Lc(atributos[[10]]), col="darkred", lwd=2, main="WTO - Exports 2010")
legend("topleft", "Gini = 0.8410")
plot(Lc(atributos2[[10]]), col="darkred", lwd=2, main="WTO - Exports 2011")
legend("topleft", "Gini = 0.8248")

par(mfrow=c(1,1))
plot(Lc(atributos3[[10]]), col="darkred", lwd=2, main="WTO - Exports 2014")
legend("topleft", "Gini = 0.8396")


# Analisando o Gini
gini11 <- ineq(atributos[[8]], type = "Gini")
gini12 <- ineq(atributos2[[8]], type = "Gini")
gini13 <- ineq(atributos3[[8]], type = "Gini")
gini1 <- c(gini11,gini12,gini13)
gini21 <- ineq(atributos[[10]], type = "Gini")
gini22 <- ineq(atributos2[[10]], type = "Gini")
gini23 <- ineq(atributos3[[10]], type = "Gini")
gini2 <- c(gini21,gini22,gini23)

data <- data.frame(gini1,gini2, year=c(2010,2011,2014))
data %<>% reshape2::melt(.,id="year")
ggplot(data=data, aes(x=year, y=value, color=variable))+geom_line(lwd=1)+geom_point(size=3)+
  ylim(.8,.85)+labs(x="",y="Gini Coefficient",title="World Exports")+
  scale_color_discrete(name="",
                     labels=c("Merchandise Exports, f.o.b. (million US$)",
                              "Share in world total exports"))+
  theme_bw()+theme(legend.position="top",legend.text=element_text(size=12))


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

