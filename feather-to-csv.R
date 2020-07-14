#verifica (e instala) pacotes requeridos
show('Testando se pacotes necessários estão instalados...')
if("feather" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote feather...')
  install.packages("feather")
}
if("tcltk" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote tcltk...')
  install.packages("tcltk")
}

#carrega pacotes requeridos
show('Carregando pacote feather...')
library(feather)
show('Carregando pacote tcltk...')
library(tcltk)

#remove variáveis antigas
show('Removendo lixo...')
rm(list = ls())

#pega o local dos arquivos
show('Selecionando origem dos dados...')
origem <- tk_choose.files(multi = TRUE, filters = matrix(c(
  "feather files", ".feather"
), 1,2, byrow = TRUE))

if(length(origem) == 0){
  show('Você não selecionou arquivos de origem!')
  quit()
}


#pega o diretório onde salvar os arquivos
show('Selecionando destino dos dados...')
destino <- tk_choose.dir()

if(length(destino) == 0){
  show('Você não selecionou um arquivo de destino!')
  quit()
}

setwd(destino)


#faz o loop nos arquivos de origem

for (arquivo_origem in origem) {
  show(paste("Convertendo",arquivo_origem, sep = " "))
  
  vetor = unlist(strsplit(basename(arquivo_origem), split = "[.]"))
  
  csv = paste(vetor[1], 'csv', sep = '.')
  
  dados = read_feather(arquivo_origem)
  
  write.csv2(dados, file = csv, sep = ';', dec = ',', row.names = FALSE)
  
}

show("Fim!")