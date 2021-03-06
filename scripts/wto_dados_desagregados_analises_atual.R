# WTO Analysis 1 (multinom / gamma)
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

bound <- read.csv("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/WTO Tarifas/bound.csv",
                  stringsAsFactors = F, header=T)
library(dplyr)
names(dados)[1] <- "pais"
names(bound)[1] <- "pais"
dados.comp <- left_join(dados, bound)

idh <- read.csv("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/idh/idh.csv", stringsAsFactors = F)
names(idh)[1] <- "pais"
names(idh)[2] <- "idh"
dados.comp <- left_join(dados.comp, idh)

dados <- dados.comp[,2:12]

# Substituindo os NA por zeros e transformando o banco em "dados" novamente
#dados.comp <- as.data.frame(sapply(dados.comp[,1:12], function(x) car::recode(x, "NA=0"), simplify = F), stringsAsFactors = F)
#dados <- as.data.frame(sapply(dados.comp[,2:12], function(x) x+0.1, simplify = F), stringsAsFactors = F)
rownames(dados) <- dados.comp$pais
#View(dados)
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
V(g.out3.int)$Bound              <- dados$simple.average.final.bound
V(g.out3.int)$IDH                <- dados$idh

#==================================
# Blockmodelling
############################
#Deletando a Uniao Europeia
#g.out3.int = delete_vertices(g.out3.int, 'EuropeanUnion')
#############################

############################
#Deletando os países da UE
ue_paises = c('Austria','Belgium','Bulgaria','Croatia','Cyprus','CzechRepublic',
              'Denmark','Estonia','Finland','France','Germany','Greece','Hungary',
              'Ireland','Italy','Latvia','Lithuania','Luxembourg','Malta','Netherlands',
              'Poland','Portugal','Romania','SlovakRepublic','Slovenia','Spain','Sweden',
              'UnitedKingdom')

g.semUE = g.out3.int

for (pais in ue_paises){
  if (pais %in% V(g.semUE)$name == F){
    next
  }
  else{
    g.semUE = delete_vertices(g.semUE, pais)
  }
}
g.semUE

##################################
#BLOCKMODELING só com UE

#library(mixer)
#mix <- mixer(as.matrix(get.adjacency(g.semUE)), qmin = 3, qmax = 6, directed = T)
#bm.output <- getModel(mix)
#bm.output$Pis # Class connectivity matrix
#plot(mix)

#grupos <-c()

#for (i in 1:ncol(bm.output$Taus)){
#  grupos[i] <- which.max(bm.output$Taus[,i])
#}

#plot(g.semUE, vertex.label.cex=.7, vertex.color=grupos+2, edge.arrow.size=.3,
#     vertex.size = 4 ,layout=layout_with_fr)
#title(main="Blockmodeling")

#write.csv(grupos, "~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/seminario_giars/blocos.csv", row.names = F)

#Reduzindo a rede aos blocos
grupos <- read.csv("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/seminario_giars/blocos.csv", header=T)
grupos <- grupos$x

V(g.semUE)$bloco <- grupos

g.blocos <- contract.vertices(g.semUE, as.factor(V(g.semUE)$bloco),
                              vertex.attr.comb = list(inst=toString, "ignore"))
V(g.blocos)$bloco
V(g.blocos)$name <- levels(as.factor(grupos))
indeg.blocos <- igraph::degree(g.blocos, mode = "in")

plot(g.blocos, edge.arrow.size=.3, layout=layout_in_circle, 
     vertex.label=V(g.blocos)$name, vertex.size=indeg.blocos/3, 
     vertex.color=as.numeric(V(g.blocos)$name)+2)

# A china ficou sozinho no bloco 5, UE e EUA ficaram no grupo 4.
# Para estimações talvez seja interessante colocar os big three num mesmo bloco
# representando o centro.




# Regressão linear
#indeg <- igraph::degree(g.out3.int, mode="in")
#dados <- cbind(dados, indeg+0.1)
#names(dados)[11] <- "indeg"

const <- constraint(g.semUE)
#dados2 <- left_join(dados, const)
#names(dados)[12] <- "constraint"

#Arrumar esse modelo

fit1 <- glm(indeg~., data=dados, family = Gamma(link="log"))
summary(fit1)
par(mfrow=c(2,2))
plot(fit1)
par(mfrow=c(2,2))
texreg(fit1, caption="GLM - Gamma - Response: In Degree", caption.above = T,
       center=F, digits = 3, single.row = T)
#Modelo mais ou menos...

# Ajustando modelo logístico multinomial
library(nnet)
dados$country = rownames(dados)
bd.grupos = as.data.frame(cbind(V(g.semUE)$name, grupos), stringsAsFactors = F)
names(bd.grupos) = c("country","bloco")
bd.grupos$bloco %<>% as.numeric
dados2 <- left_join(dados, bd.grupos)

#Ajeitando os blocos
dados2$bloco[6] <- 1 #Colocando a china junto com os BRICS
periferia <- which(dados2$bloco == 3)
dados2$bloco[periferia] <- 2 #Juntando a periferia
levels(factor(dados2$bloco))

nas = which(is.na(dados2$bloco))
dados2$bloco[nas] = 4
#View(dados2[,c(11,12,13)])

dados2$idh[13] = 0.747252859936566 
dados2$idh[17] = 0.885027260021308
dados2$idh[20] = 0.762252400296525
dados2$idh[32] = 0.890262992237101
dados2$idh[36] = 0.462221996949664
dados2$idh[37] = 0.781675314080213
dados2$idh[38] = 0.823921956812207
dados2$idh[39] = 0.837273977255894
dados2$idh[68] = 0.870090820750014
dados2$idh[69] = 0.916079907698891
dados2$idh[72] = 0.923327926047681
dados2$idh[73] = 0.94387728002259
dados2$idh[81] = 0.860835015350877
dados2$idh[82] = 0.876124307433439
dados2$idh[84] = 0.882727786825495
dados2$idh[86] = 0.888107436522597
dados2$idh[88] = 0.906698191736312
dados2$idh[94] = 0.86518718080572
dados2$idh[97] = 0.817539869013173
dados2$idh[98] = 0.828350453189915
dados2$idh[99] = 0.915541561984203
dados2$idh[102] = 0.873022984102699
dados2$idh[104] = 0.521214355954627
dados2$idh[105] = 0.655273514725433
dados2$idh[113] = 0.839421883128433
dados2$idh[114] = 0.891851626668635
dados2$idh[115] = 0.818766009538997
dados2$idh[117] = 0.693302690570275
dados2$idh[120] = 0.727494941192669
dados2$idh[121] = 0.838978340089051
dados2$idh[125] = 0.921793511865192
dados2$idh[127] = 0.842677827910993
dados2$idh[128] = 0.83009529677395
dados2$idh[130] = 0.792797419961585
dados2$idh[135] = 0.906816460156634
dados2$idh[136] = 0.880280840350184
dados2$idh[137] = 0.843572119093826

#colocando o IDH com variação de .1
dados2$idh <- dados2$idh*100

#colocando o constraint com variação de .1
bd.const = as.data.frame(cbind(V(g.semUE)$name, const), stringsAsFactors = F)
names(bd.const) = c("country","const")
dados3 = left_join(dados2, bd.const)
head(dados3[,12:14])
nas = which(is.na(dados3$const))
dados3$const[nas] = dados3$const[2]

dados3$const %<>% as.numeric

dados3$const <- dados3$const * 100
##############################################
# Tirando a UE para análise
dados4 = dados3[-2,]
names(dados4)

# Atribuindo 5 ao bound dos paises UE
lista = c()
for (pais in ue_paises){
  y = which(dados4$country == pais)
  lista = c(lista, y)
}
#View(dados4)

dados4$simple.average.final.bound[lista] = 5.0

# Fitando o modelo multinomial
#####################################
#dados4$Cresc.valor.agreg[dados4$Cresc.valor.agreg == 0] = NA
#dados4$Patentes[dados4$Patentes == 0] = NA
dados$Baixas


#fit.multi1 <- multinom(factor(bloco, levels = c(2,1,4))~.-country, data=dados4)
#fit.multi2 <- multinom(factor(bloco, levels = c(2,1,4))~.-country-Patentes, data=dados4)
fit.multi3 <- multinom(factor(bloco, levels = c(2,1,4))~.-country-Patentes-Media.de.artigos, data=dados4)
#fit.multi4 <- multinom(factor(bloco, levels = c(2,1,4))~.-country-Patentes-Media.de.artigos-Baixas, data=dados4)
#fit.multi5 <- multinom(factor(bloco, levels = c(2,1,4))~.-country-Patentes-Media.de.artigos-GINI, data=dados4)
#fit.multi6 <- multinom(factor(bloco, levels = c(2,1,4))~.-country-Patentes-Media.de.artigos-Baixas-GINI, data=dados4)
# Melhor modelo é o fit.multi3

summary(fit.multi3)

screenreg(fit.multi3)

#texreg(list(fit.multi1,fit.multi2,fit.multi3), 
#          caption="Multinomial Logistic Model", caption.above = T,
#       center=F, digits = 3, single.row = T)
#texreg(list(fit.multi4,fit.multi5,fit.multi6,fit.multi7), 
#       caption="Multinomial Logistic Model", caption.above = T,
#       center=F, digits = 3, single.row = T)
texreg(fit.multi3, 
       caption="Multinomial Logistic Model", caption.above = T,
       center=F, digits = 3, single.row = T)



coef.prob <- function(x){
  or <- (exp(x) - 1)*100
  cat("Coef transformed to Probability\n")
  return(or)
}

library(xtable)
x.coef <- xtable(cbind(t(exp(coef(fit.multi3))) ,t(coef.prob(coef(fit.multi3)))), 
                 caption="Exponentials and Percentages", digits=2)
print(x.coef)

# Computando pseudo R2
library(pscl)
pR2(fit.multi3)

#yhat <- predict(fit.multi, type = "probs")
names(dados4)
dados4$bloco %<>% as.factor
levels(dados4$bloco) <- c("BRICS", "Periphery", "US + EU")
dados4$simple.average.final.bound = dados3$simple.average.final.bound[-2]


b1 <- ggplot(dados4, aes(x=factor(bloco), y=PIB.CeT))+geom_boxplot()+labs(x="")+theme_light()
b2 <- ggplot(dados4, aes(x=factor(bloco), y=Media.de.artigos))+geom_boxplot()+labs(x="")+theme_light()
b3 <- ggplot(dados4, aes(x=factor(bloco), y=Patentes))+geom_boxplot()+labs(x="")+theme_light()
b4 <- ggplot(dados4, aes(x=factor(bloco), y=Gasto.Educ))+geom_boxplot()+labs(x="")+theme_light()
b5 <- ggplot(dados4, aes(x=factor(bloco), y=Cresc.valor.agreg))+geom_boxplot()+labs(x="")+theme_light()
b6 <- ggplot(dados4, aes(x=factor(bloco), y=Baixas))+geom_boxplot()+labs(x="")+theme_light()

multiplot(b1,b2,b3,b4,b5,b6, cols = 3)

b7 <- ggplot(dados4, aes(x=factor(bloco), y=Homicidios))+geom_boxplot()+labs(x="")+theme_light()
b8 <- ggplot(dados4, aes(x=factor(bloco), y=Pobreza.ext))+geom_boxplot()+labs(x="")+theme_light()
b9 <- ggplot(dados4, aes(x=factor(bloco), y=GINI))+geom_boxplot()+labs(x="")+theme_light()
b10 <- ggplot(dados4, aes(x=factor(bloco), y=simple.average.final.bound))+geom_boxplot()+labs(x="")+theme_light()
b11 <- ggplot(dados4, aes(x=factor(bloco), y=idh))+geom_boxplot()+labs(x="")+theme_light()
b12 <- ggplot(dados4, aes(x=factor(bloco), y=const))+geom_boxplot()+labs(x="")+theme_light()

multiplot(b7,b8,b9,b10,b11,b12, cols = 3)
