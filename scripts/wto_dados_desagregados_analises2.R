# WTO - Analises2
# modelos ERGM para os dados dos 3 anos
# Neylson Crepalde

#####################
names(dados)
dados %<>% .[,-c(12,13)]
dados <- cbind(dados, V(g.out3.int)$name)
names(dados)[12] <- "pais"

dados <- as.data.frame(sapply(dados[,1:12], function(x) car::recode(x, "NA=0"), simplify = F), stringsAsFactors = F)

name2.int <- as.data.frame(V(g.out2.int)$name, stringsAsFactors = F)
names(name2.int)[1] <- "pais"
dados2 <- left_join(name2.int, dados)

#Colocando atributos na rede2
#View(dados)
names(dados2)
V(g.out2.int)$PIB.CeT            <- dados2$PIB.CeT
V(g.out2.int)$Media.de.artigos   <- dados2$Media.de.artigos
V(g.out2.int)$Patentes           <- dados2$Patentes
V(g.out2.int)$Gasto.Educ         <- dados2$Gasto.Educ
V(g.out2.int)$Cresc.valor.agreg  <- dados2$Cresc.valor.agreg
V(g.out2.int)$Baixas             <- dados2$Baixas
V(g.out2.int)$Homicidios         <- dados2$Homicidios
V(g.out2.int)$Pobreza.ext        <- dados2$Pobreza.ext
V(g.out2.int)$GINI               <- dados2$GINI
V(g.out2.int)$Bound              <- dados2$simple.average.final.bound
V(g.out2.int)$IDH                <- dados2$idh

#Colocando atributos na rede1
name1.int <- as.data.frame(V(g.out.int)$name, stringsAsFactors = F)
names(name1.int)[1] <- "pais"
dados1 <- left_join(name1.int, dados)


#View(dados)
names(dados1)
V(g.out.int)$PIB.CeT            <- dados1$PIB.CeT
V(g.out.int)$Media.de.artigos   <- dados1$Media.de.artigos
V(g.out.int)$Patentes           <- dados1$Patentes
V(g.out.int)$Gasto.Educ         <- dados1$Gasto.Educ
V(g.out.int)$Cresc.valor.agreg  <- dados1$Cresc.valor.agreg
V(g.out.int)$Baixas             <- dados1$Baixas
V(g.out.int)$Homicidios         <- dados1$Homicidios
V(g.out.int)$Pobreza.ext        <- dados1$Pobreza.ext
V(g.out.int)$GINI               <- dados1$GINI
V(g.out.int)$Bound              <- dados1$simple.average.final.bound
V(g.out.int)$IDH                <- dados1$idh

#Colocando atributos na rede3
name3.int <- as.data.frame(V(g.out3.int)$name, stringsAsFactors = F)
names(name3.int)[1] <- "pais"
dados3 <- left_join(name3.int, dados)


#View(dados)
names(dados3)
V(g.out3.int)$PIB.CeT            <- dados3$PIB.CeT
V(g.out3.int)$Media.de.artigos   <- dados3$Media.de.artigos
V(g.out3.int)$Patentes           <- dados3$Patentes
V(g.out3.int)$Gasto.Educ         <- dados3$Gasto.Educ
V(g.out3.int)$Cresc.valor.agreg  <- dados3$Cresc.valor.agreg
V(g.out3.int)$Baixas             <- dados3$Baixas
V(g.out3.int)$Homicidios         <- dados3$Homicidios
V(g.out3.int)$Pobreza.ext        <- dados3$Pobreza.ext
V(g.out3.int)$GINI               <- dados3$GINI
V(g.out3.int)$Bound              <- dados3$simple.average.final.bound
V(g.out3.int)$IDH                <- dados3$idh


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

#########
# ERGM n.out3
model3 = formula(n.out3~edges+mutual+gwesp(1.6,fixed=T)+gwidegree(1, fixed=T)+
                   gwodegree(1.4, fixed=T)+istar(4)+balance+transitive+
                   nodeicov("PIB.CeT")+nodeicov("Gasto.Educ")+nodeicov("GINI")+
                   nodeicov("populacao")+nodeicov("Bound")+nodeicov("IDH"))

summary.statistics(model3)

fit3 <- ergm(model3, control=control.ergm(parallel = 8, parallel.type = "PSOCK", 
                                          main.method="Stepping"))
summary(fit3)

gof3 <- gof(fit3)
par(mfrow=c(1,4))
plot(gof3)
par(mfrow=c(1,1))
texreg(fit3, center=F,caption.above=T,caption="ERGM - 2014",digits=3)

#############################
# ERGM 2
model2 = formula(n.out2~edges+mutual+gwesp(1.6,fixed=T)+gwidegree(1, fixed=T)+
                   gwodegree(1.4, fixed=T)+istar(4)+balance+transitive+
                   nodeicov("PIB.CeT")+nodeicov("Gasto.Educ")+nodeicov("GINI")+
                   nodeicov("populacao")+nodeicov("Bound")+nodeicov("IDH"))

summary.statistics(model2)

fit2 <- ergm(model2, control=control.ergm(parallel = 8, parallel.type = "PSOCK", 
                                          main.method="Stepping"))
summary(fit2)

gof2 <- gof(fit2)
par(mfrow=c(1,4))
plot(gof2)
par(mfrow=c(1,1))

texreg(list(fit2,fit3), center=F,caption.above=T,digits=3,
       caption="ERGM - 2011, 2014",
       custom.model.names=c("ERGM - 2011","ERGM - 2014"))

#################################
# ERGM 1
model1 = formula(n.out~edges+mutual+gwesp(1.6,fixed=T)+gwidegree(1, fixed=T)+
                   gwodegree(1.4, fixed=T)+istar(4)+balance+transitive+
                   nodeicov("PIB.CeT")+nodeicov("Gasto.Educ")+nodeicov("GINI")+
                   nodeicov("populacao")+nodeicov("Bound")+nodeicov("IDH"))

summary.statistics(model1)

fit1 <- ergm(model1, control=control.ergm(parallel = 8, parallel.type = "PSOCK", 
                                          main.method="Stepping"))
summary(fit1)

gof1 <- gof(fit1)
par(mfrow=c(1,4))
plot(gof1)
par(mfrow=c(1,1))

texreg(list(fit1,fit2,fit3), center=F,caption.above=T,digits=3,
       caption="ERGM - 2010, 2011, 2014",
       custom.model.names=c("ERGM - 2010","ERGM - 2011","ERGM - 2014"))


#################
wto.out <- networkDynamic(network.list = list(n.out, n.out2, n.out3),
                          vertex.pid = "vertex.names", create.TEAs = T)

render.animation(wto.out, render.par = list(tween.frames=1, show.time=T))
#filmstrip(wto.out, frames=4, displaylabels=F)
proximity.timeline(wto.out)

#Verificando os atributos da rede temporal
#wto.out %v% "populacao.active"
summary.statistics(wto.out~edges+mutual+gwesp(1.3,fixed=F)+gwidegree(1, fixed=F)+
                     gwodegree(1, fixed=F)+m2star+ctriple+
                     nodecov("exports.active")+nodecov("imports.active")+
                     nodecov("GDP.active")+nodecov("populacao.active"))

########################################################
#Montando o modelo
######### Parei aqui
formation <- formula(~edges+mutual+gwesp(1.6,fixed=T)+gwidegree(1, fixed=T)+
                       gwodegree(1.4, fixed=T)+
                       nodeicov("PIB.CeT")+nodeicov("Gasto.Educ")+nodeicov("GINI")+
                       nodeicov("populacao")+nodeicov("Bound")+nodeicov("IDH"))

dissolution <- formula(~edges+mutual+gwesp(1.6,fixed=T)+gwidegree(1, fixed=T)+
                         gwodegree(1.4, fixed=T))

########################################
# STERGM
########################################

tempfit.par <- stergm(list(n.out, n.out2, n.out3), formation, dissolution, estimate = "CMLE",
                      control=control.stergm(parallel=8,parallel.type="PSOCK"))


summary(tempfit.par)

#texreg(tempfit.par, center=F,caption.above=T,digits=3,
#       caption="Temporal ERGM (2010, 2011, 2014)",
#       custom.model.names=c("TERGM"), single.row = T)

gof.temp <- gof(tempfit.par)

par(mfrow=c(1,4))
plot(gof.temp)
par(mfrow=c(1,1))
