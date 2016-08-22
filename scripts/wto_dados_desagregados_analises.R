# WTO Temporal analysis
# ANALYSIS
# Neylson Crepalde

source("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados.R")
source("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados2.R")
source("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/wto_dados_desagregados3.R")

library(statnet)
library(intergraph)
library(ndtv)
library(ineq)
library(ggplot2)
library(reshape2)
library(texreg)
library(gdata)

################################
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
atributos.order %<>% .[,-2]
atributos.order2 %<>% .[,-2]
atributos.order3 %<>% .[,-2]

atributos.order[,11] <- 2010
names(atributos.order)[11] <- "Year"
atributos.order2[,11] <- 2011
names(atributos.order2)[11] <- "Year"
atributos.order3[,11] <- 2014
names(atributos.order3)[11] <- "Year"

############################
# Calculando o GINI e plotando a Lorenz Curve
ineq(atributos[[10]], type = "Gini")
ineq(atributos2[[10]], type="Gini")
ineq(atributos3[[10]], type="Gini")

par(mfrow=c(1,2))
plot(Lc(atributos[[10]]), col="darkred", lwd=2, main="WTO - Exports 2010")
legend("topleft", "Gini = 0.8410", bty = "n")
plot(Lc(atributos2[[10]]), col="darkred", lwd=2, main="WTO - Exports 2011")
legend("topleft", "Gini = 0.8248", bty="n")

par(mfrow=c(1,1))
plot(Lc(atributos3[[10]]), col="darkred", lwd=2, main="WTO - Exports 2014")
legend("topleft", "Gini = 0.8396",bty="n")

# plotando a Lc com ggplot
Lc1 <- Lc(atributos[[10]])
p1 <- Lc1$p
L1 <- Lc1$L
gg1 <- ggplot(data=NULL,aes(x=p1,y=L1))+geom_line(lwd=1,color="darkred")+geom_abline(intercept=0,slope=1)+theme_bw()+
  geom_hline(yintercept = 0)+geom_vline(xintercept = 1)+
  scale_y_continuous(breaks=seq(from=0,to=1,by=.1))+scale_x_continuous(breaks=seq(from=0,to=1,by=.1))+
  labs(title="Lorenz Curve - 2010",x="Share in world total exports",y="")+
  annotate("text",x=.6,y=.3,label="Gini = 0.8287",size=5)

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
  scale_color_discrete(name="",
                       labels=c("Merchandise Exports, f.o.b. (million US$)",
                                "Share in world total exports"))+
  theme_light()+theme(legend.position="top",legend.text=element_text(size=12))

###############################
#analisando o comportamento de algumas variaveis
names(atributos.order)[1:7] <- c("name","Population (thousands)","GDP (million current US$)","GDP (million current PPP US$)",
                                 "Current account balance (million US$)","Trade per capita (US$, last 3 years)",
                                 "Trade to GDP ratio (last 3 years)")
names(atributos.order2)[1:7] <- c("name","Population (thousands)","GDP (million current US$)","GDP (million current PPP US$)",
                                  "Current account balance (million US$)","Trade per capita (US$, last 3 years)",
                                  "Trade to GDP ratio (last 3 years)")
names(atributos.order3)[1:7] <- c("name","Population (thousands)","GDP (million current US$)","GDP (million current PPP US$)",
                                  "Current account balance (million US$)","Trade per capita (US$, last 3 years)",
                                  "Trade to GDP ratio (last 3 years)")

atributos.order.completo <- rbind(atributos.order,atributos.order2,atributos.order3)
atributos.EURO <- atributos.order.completo[atributos.order.completo$name=="EuropeanUnion",]
atributos.USA  <- atributos.order.completo[atributos.order.completo$name=="UnitedStates",]
atributos.CHINA<- atributos.order.completo[atributos.order.completo$name=="China",]

EURO.melt  <- melt(atributos.EURO,id="Year")
USA.melt   <- melt(atributos.USA,id="Year")
CHINA.melt <- melt(atributos.CHINA,id="Year")

# European Union
EU1 <- ggplot(data=EURO.melt[c(4:6),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+ylab("")+theme(legend.position="top")

EU2 <- ggplot(data=EURO.melt[c(22:27),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("Exports (million US$)","Imports (million US$)"))+theme(legend.position="top")+ylab("")

EU3 <- ggplot(data=EURO.melt[7:12,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("GDP (million US$)","GDP (million PPP US$)"))+theme(legend.position="top")+ylab("")

EU4 <- ggplot(data=EURO.melt[13:15,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+theme(legend.position="top")+ylab("")

EU5 <- ggplot(data=EURO.melt[16:18,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+theme(legend.position="top")+ylab("")

EU6 <- ggplot(data=EURO.melt[c(19:21,28:30),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("Trade GDP ratio","Share"))+theme(legend.position="top")+ylab("")

multiplot(EU1,EU2,EU3,EU4,EU5,EU6, cols=3)

# United States
USA1 <- ggplot(data=USA.melt[c(4:6),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+ylab("")+theme(legend.position="top")

USA2 <- ggplot(data=USA.melt[c(22:27),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("Exports (million US$)","Imports (million US$)"))+theme(legend.position="top")+ylab("")

USA3 <- ggplot(data=USA.melt[7:12,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("GDP (million US$)","GDP (million PPP US$)"))+theme(legend.position="top")+ylab("")

USA4 <- ggplot(data=USA.melt[13:15,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+theme(legend.position="top")+ylab("")

USA5 <- ggplot(data=USA.melt[16:18,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+theme(legend.position="top")+ylab("")

USA6 <- ggplot(data=USA.melt[c(19:21,28:30),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("Trade GDP ratio","Share"))+theme(legend.position="top")+ylab("")

multiplot(USA1,USA2,USA3,USA4,USA5,USA6, cols=3)

# China
CHINA1 <- ggplot(data=CHINA.melt[c(4:6),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+ylab("")+theme(legend.position="top")

CHINA2 <- ggplot(data=CHINA.melt[c(22:27),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("Exports (million US$)","Imports (million US$)"))+theme(legend.position="top")+ylab("")

CHINA3 <- ggplot(data=CHINA.melt[7:12,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("GDP (million US$)","GDP (million PPP US$)"))+theme(legend.position="top")+ylab("")

CHINA4 <- ggplot(data=CHINA.melt[13:15,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+theme(legend.position="top")+ylab("")

CHINA5 <- ggplot(data=CHINA.melt[16:18,], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="")+theme(legend.position="top")+ylab("")

CHINA6 <- ggplot(data=CHINA.melt[c(19:21,28:30),], aes(x=Year, y=as.numeric(value), color=variable))+geom_path()+geom_point(size=1.5)+
  scale_color_discrete(name="",labels=c("Trade GDP ratio","Share"))+theme(legend.position="top")+ylab("")

multiplot(CHINA1,CHINA2,CHINA3,CHINA4,CHINA5,CHINA6, cols=3)


#######################
# Pegando a interseção entre as redes wto

intersecao1 = intersect(atributos.order$name1, atributos.order2$name2)
intersecao2 = intersect(atributos.order$name1, atributos.order3$name3)
intersecao.final = intersect(intersecao1, intersecao2)

g.intersect = intersection(g.out, g.out2, g.out3, keep.all.vertices = F)
names.int = V(g.intersect)$name
excluir.g.out = which(atributos.order[[1]] %in% names.int == F)
excluir.g.out2 = which(atributos.order2[[1]] %in% names.int == F)
excluir.g.out3 = which(atributos.order3[[1]] %in% names.int == F)

g.out.int = delete_vertices(g.out, excluir.g.out)
g.out2.int = delete_vertices(g.out2, excluir.g.out2)
g.out3.int = delete_vertices(g.out3, excluir.g.out3)

#Estamos chegando no mesmo resultado?
which(V(g.out3.int)$name %in% V(g.out.int)$name == F)
sort(V(g.out.int)$name) == sort(V(g.out3.int)$name)
#sim

#####################
# Acrescentando atributos do banco mundial
dados <- read.xls("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/seminario_giars/wto_atributos.xls",
                  stringsAsFactors = F, header=T)

# Substituindo os zeros por NA
dados <- as.data.frame(sapply(dados[,1:10], function(x) car::recode(x, "NA=0"), simplify = F), stringsAsFactors = F)
View(dados)
names(dados)
V(g.out3.int)$PIB.CeT            <- dados$PIB.CeT
V(g.out3.int)$Media.de.artigos   <- dados$Media.de.artigos
V(g.out3.int)$Patentes           <- dados$Patentes
V(g.out3.int)$Gasto.Educ         <- dados$Gasto.Educ
V(g.out3.int)$Cresc.valor.agreg  <- dados$Cresc.valor.agreg
V(g.out3.int)$Baixas             <- dados$Baixas
V(g.out3.int)$Homicidios         <- dados$Homicidios
V(g.out3.int)$Pobreza.ext        <- dados$Pobreza.ext
V(g.out3.int)$GINI               <- dados$GINI

###############################
#modelando as redes com TERGM
n.out <- asNetwork(g.out.int)
n.out2 <- asNetwork(g.out2.int)
n.out3 <- asNetwork(g.out3.int)
par(mfrow=c(1,3))
#plot(n.out, main="WTO network, t1", edge.col="grey52")
#plot(n.out2, main="WTO network, t2", edge.col="grey52")
#plot(n.out3, main="WTO network, t3", edge.col="grey52")
par(mfrow=c(1,1))

wto.out <- networkDynamic(network.list = list(n.out, n.out2, n.out3),
                          vertex.pid = "vertex.names", create.TEAs = T)

render.animation(wto.out, render.par = list(tween.frames=1, show.time=T))
#filmstrip(wto.out, frames=4, displaylabels=F)
proximity.timeline(wto.out)

#########
# ERGM n.out3
model = formula(n.out3~edges+mutual+gwesp(1,fixed=T)+gwidegree(1, fixed=T)+
                  ctriple+idegreepopularity+
                  nodecov("PIB.CeT")+nodecov("Gasto.Educ")+nodecov("GINI")+
                  nodecov("Homicidios"))

summary.statistics(model)

fit <- ergm(model, control=control.ergm(parallel = 8, parallel.type = "PSOCK"))
summary(fit)

gof <- gof(fit)
par(mfrow=c(1,4))
plot(gof)
par(mfrow=c(1,1))


#################
#Verificando os atributos da rede temporal
#wto.out %v% "populacao.active"
summary.statistics(wto.out~edges+mutual+gwesp(1.3,fixed=F)+gwidegree(1, fixed=F)+
                     gwodegree(1, fixed=F)+m2star+ctriple+
                     nodecov("exports.active")+nodecov("imports.active")+
                     nodecov("GDP.active")+nodecov("populacao.active"))

########################################################
#Montando o modelo
formation <- formula(~edges+mutual+gwesp(1.3,fixed=T)+gwidegree(1, fixed=T)+
                       gwodegree(1, fixed=T)+ctriple+istar(4)+
                       nodeocov("share")+nodeocov("tradepercapita"))

dissolution <- formula(~edges+mutual+gwesp(1.3,fixed=T)+gwidegree(1, fixed=T)+
                         gwodegree(1, fixed=T)+ctriple)

#STERGM
system.time(
  tempfit.par <- stergm(list(n.out, n.out2, n.out3), formation, dissolution, estimate = "CMLE",
                        control=control.stergm(parallel=8,parallel.type="PSOCK"))
)

#summary(tempfit.par)

#texreg(tempfit.par, single.row = T, digits = 3)
system.time(
  gof.temp <- gof(tempfit.par)
)

par(mfrow=c(1,4))
plot(gof.temp)
par(mfrow=c(1,1))
