#mescla os dados de empenho, liquidação e pagamento para formar uma tabela de dados da movimentação dos empenhos

#requerimentos
library(feather)
library(sqldf)

#configurações
basedir = file.path(getwd(), '2020cm')
empenho = read_feather(file.path(basedir, 'empenho.feather'))
liquidac = read_feather(file.path(basedir, 'liquidac.feather'))
pagament = read_feather(file.path(basedir, 'pagament.feather'))

empenhos = sqldf("SELECT numero_empenho, uniorcam, funcao, subfuncao, programa, projativ, rubrica, recurso_vinculado, contrapartida_recurso_vinculado, data, credor, caracteristica_peculiar_despesa, historico registro_preco, numero_licitacao, ano_licitacao, forma_contratacao, base_legal, identificador_funcionario, licitacao_compartilhada, cnpj_gerenciador, complemento_recurso_vinculado from empenho group by numero_empenho order by numero_empenho asc")

movemp = data.frame()
for (numero in empenhos$numero_empenho) {
  item = subset(empenhos, numero_empenho == numero)
  empenhado = sqldf(paste("select sum(valor) as valor from empenho where sinal = '+' and numero_empenho = '",numero,"'", sep = ''))
  estornado = sqldf(paste("select sum(valor) as valor from empenho where sinal = '-' and numero_empenho = '",numero,"'", sep = ''))
  if(is.na(estornado$valor)){
    estornado$valor = 0
  }
  item$empenhado = empenhado$valor - estornado$valor
  
  liquidado = sqldf(paste("select sum(valor) as valor from liquidac where sinal = '+' and numero_empenho = '", numero, "'", sep = ''))
  estornado = sqldf(paste("select sum(valor) as valor from liquidac where sinal = '-' and numero_empenho = '", numero, "'", sep = ''))
  if(is.na(estornado$valor)){
    estornado$valor = 0
  }
  item$liquidado = liquidado$valor - estornado$valor
  
  pago = sqldf(paste("select sum(valor) as valor from pagament where sinal = '+' and numero_empenho = '", numero, "'", sep = ''))
  estornado = sqldf(paste("select sum(valor) as valor from pagament where sinal = '-' and numero_empenho = '", numero, "'", sep = ''))
  if(is.na(estornado$valor)){
    estornado$valor = 0
  }
  item$pago = pago$valor - estornado$valor
  
  movemp = rbind(movemp, item)
}

movemp$empenhado[is.na(movemp$empenhado)] = 0
movemp$liquidado[is.na(movemp$liquidado)] = 0
movemp$pago[is.na(movemp$pago)] = 0

write_feather(movemp, file.path(basedir, 'movemp.feather'))