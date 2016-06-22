# Dados longitudinais WTO
# GIARS
# Neylson e Ítalo

library(magrittr) #mais funções de programação
library(readr)    #pacote para ler os dados 
library(descr)    #roda medidas descritivas

dados <- read_csv("C:/Users/giars/Documents/Neylson/WTO/ne2_e.csv")
#View(dados)

class(dados$Partner_Country_desc)

dados$Partner_Country_desc %<>% as.factor
dados$Indicator_desc %<>% as.factor


freq(dados$Partner_Country_desc)
freq(dados$Indicator_desc)
descr(dados$Value)
