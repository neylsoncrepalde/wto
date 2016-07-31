#Trabalhando com os dados WTO desagregados por produto
#Neylson Crepalde
#GIARS

library(bit64)
library(data.table)
library(descr)
library(ggplot2)

keep = c(5,6,9,10,11,12,15,16)
dados <- fread("~/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/ne2_e.csv",
              stringsAsFactors = T, select = keep)
USA <- dados[dados$Country_code=="US" & dados$Indicator_desc == "Agricultural products",]

names(dados)
head(dados)


ggplot(USA[USA$Partner_Country_desc=="China"], aes(x=Year, y=Value/1000000))+geom_path()+
  geom_point()
