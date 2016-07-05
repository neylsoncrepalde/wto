# Tentando com APPLY
# Neylson

library(magrittr)
library(gdata)
library(igraph)

path <- "/home/neylson/Documentos/Neylson Crepalde/Doutorado/GIARS/wto/2010/" #o caminho do diretorio (ajustar)
files <- list.files(path = path, pattern='[.]xls')    #lista os nomes dos arquivos
sites <- paste0(path,files)                           #cola as duas informacoes para realizar o loop

# Criando os data frames vazios para preencher e o contador
rede.out <- data.frame()
rede.in <- data.frame()
contador <- 1

##############################################################
# Efetuando a coleta de dados via for loop
lexlsgrafo <- function(x){
  
  #importando os dados
  #E necessario ter o PERL instalado. Consultar https://www.perl.org/get.html
  df <- read.xls(sites[x], stringsAsFactors=F)
  #View(df)
  
  #pais em questao e limpeza
  pais <- df[1,1]
  pais %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.) %>% gsub("\\s","",.)
  
  #separando as colunas de interesse - lacos e pesos
  dados <- df[42:46,c(1,6,8,13)]
  #dados
  
  #limpando os dados
  dados[[2]] %<>% gsub(" ","",.) %>% as.numeric
  dados[[4]] %<>% gsub(" ","",.) %>% as.numeric
  dados[[1]] %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.) %>% gsub("\\s","",.)
  dados[[3]] %<>% gsub("[[:digit:]]","",.) %>% gsub("[[:punct:]]","",.) %>% gsub("\\s","",.)
  #dados
  
  
  # montando a lista de lacos
  rede.out1 <- data.frame(pais, dados[[1]], weights=dados[[2]])
  rede.in1  <- data.frame(dados[[3]], pais, weights=dados[[4]])
  rede.out  <- rbind(rede.out, rede.out1)
  rede.in   <- rbind(rede.in, rede.in1)
  
}

lexlsgrafo(sites[1])
