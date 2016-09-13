#WTO - Analises
# modelos ERGM para os dados dos 3 anos
# Neylson Crepalde

#####################
names(dados)
dados %<>% .[,-c(11,12)]
dados <- cbind(dados, V(g.out3.int)$name)
names(dados)[11] <- "pais"

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

