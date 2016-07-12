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

#############################
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

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

# plotando a Lc com ggplot
Lc1 <- Lc(atributos[[10]])
p1 <- Lc1$p
L1 <- Lc1$L
gg1 <- ggplot(data=NULL,aes(x=p1,y=L1))+geom_line(lwd=1,color="darkred")+geom_abline(intercept=0,slope=1)+theme_bw()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 1)+
  scale_y_continuous(breaks=seq(from=0,to=1,by=.1))+scale_x_continuous(breaks=seq(from=0,to=1,by=.1))+
  labs(title="Lorenz Curve - 2010",x="Share in world total exports",y="")+
  annotate("text",x=.6,y=.3,label="Gini = 0.8410",size=5)

Lc2 <- Lc(atributos2[[10]])
p2 <- Lc2$p
L2 <- Lc2$L
gg2 <- ggplot(data=NULL,aes(x=p2,y=L2))+geom_line(lwd=1,color="darkred")+geom_abline(intercept=0,slope=1)+theme_bw()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 1)+
  scale_y_continuous(breaks=seq(from=0,to=1,by=.1))+scale_x_continuous(breaks=seq(from=0,to=1,by=.1))+
  labs(title="Lorenz Curve - 2011",x="Share in world total exports",y="")+
  annotate("text",x=.6,y=.3,label="Gini = 0.8248",size=5)

Lc3 <- Lc(atributos3[[10]])
p3 <- Lc3$p
L3 <- Lc3$L
gg3 <- ggplot(data=NULL,aes(x=p3,y=L3))+geom_line(lwd=1,color="darkred")+geom_abline(intercept=0,slope=1)+theme_bw()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 1)+
  scale_y_continuous(breaks=seq(from=0,to=1,by=.1))+scale_x_continuous(breaks=seq(from=0,to=1,by=.1))+
  labs(title="Lorenz Curve - 2014",x="Share in world total exports",y="")+
  annotate("text",x=.6,y=.3,label="Gini = 0.8396",size=5)

multiplot(gg1,gg2,gg3,cols=3)

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
  scale_color_discrete(name="",labels=c("Merchandise Exports, f.o.b. (million US$)",
                              "Share in world total exports"))+
  theme_light()+theme(legend.position="top",legend.text=element_text(size=12))


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

