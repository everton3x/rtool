#remove vari?veis antigas
show('Removendo lixo...')
rm(list = ls())

#verifica (e instala) pacotes requeridos
show('Testando se pacotes necess?rios est?o instalados...')
if("feather" %in% rownames(installed.packages()) == FALSE){
  show('Instalando pacote feather...')
  install.packages("feather")
}


#carrega pacotes requeridos
show('Carregando pacote feather...')
library(feather)

#configura??es

arquivos = c('bal_desp.feather', 'bal_rec.feather', 'bal_ver.feather', 'empenho.feather', 'liquidac.feather', 'pagament.feather', 'receita.feather', 'bver_enc.feather')

dir2018pm = file.path(getwd(), '2018pm')
dir2018cm = file.path(getwd(), '2018cm')
dir2019pm = file.path(getwd(), '2019pm')
dir2019cm = file.path(getwd(), '2019cm')
dir2020pm = file.path(getwd(), '2020pm')
dir2020cm = file.path(getwd(), '2020cm')

destino = file.path(getwd(), 'agregado')

#fim das configura??es, pode rodar o script

for (arquivo in arquivos) {
  #pm 2018
  setwd(dir2018pm)
  if(file.exists(arquivo)){
    dados2018pm = read_feather(arquivo)
    linhas = nrow(dados2018pm)
    dados2018pm$exercicio_base = seq(2018,2018,length.out=linhas)
  }
  
  #cm 2018
  setwd(dir2018cm)
  if(file.exists(arquivo)){
    dados2018cm = read_feather(arquivo)
    linhas = nrow(dados2018cm)
    dados2018cm$exercicio_base = seq(2018,2018,length.out=linhas)
  }
  
  #pm 2019
  setwd(dir2019pm)
  if(file.exists(arquivo)){
    dados2019pm = read_feather(arquivo)
    linhas = nrow(dados2019pm)
    dados2019pm$exercicio_base = seq(2019,2019,length.out=linhas)
  }
  
  #cm 2019
  setwd(dir2019cm)
  if(file.exists(arquivo)){
    dados2019cm = read_feather(arquivo)
    linhas = nrow(dados2019cm)
    dados2019cm$exercicio_base = seq(2019,2019,length.out=linhas)
  }
  
  #pm 2020
  setwd(dir2020pm)
  if(file.exists(arquivo)){
    dados2020pm = read_feather(arquivo)
    linhas = nrow(dados2020pm)
    dados2020pm$exercicio_base = seq(2020,2020,length.out=linhas)
  }
  
  #cm 2020
  setwd(dir2020cm)
  if(file.exists(arquivo)){
    dados2020cm = read_feather(arquivo)
    linhas = nrow(dados2020cm)
    dados2020cm$exercicio_base = seq(2020,2020,length.out=linhas)
  }
  
  #une os dados
  
  agregado = rbind(dados2018pm, dados2018cm, dados2019pm, dados2019cm, dados2020pm, dados2020cm)
  
  setwd(destino)
  
  write_feather(agregado, arquivo)
}

print('Fim!')