require(readxl)
require(tidyverse)
require(lubridate)

cadastro = read_xlsx('cadastro.xlsx')
cadastro$Plantio = as.Date(cadastro$Plantio, format="%Y-%m-%d")
cadastro$anoPlantio = year(cadastro$Plantio)
producao = read_xlsx('tabela_producao.xlsx')

idadeCorte = c(5, 6, 7, 8)

vet = function(
  talhao, 
  idadeCorte, 
  producao, 
  cadastro, 
  receitaM3 = 70, 
  taxa = 0.08){

  volCorte = producao %>%
    filter(Sitio == as.numeric(cadastro[cadastro$Talhao == talhao,'Sitio'])) %>%
    filter(MatGen == as.character(cadastro[cadastro$Talhao == talhao,'MatGen'])) %>%
    filter(Idade == idadeCorte) %>%
    select(m3ha)

  receitaPresente = (as.numeric(volCorte) * receitaM3) / (1+taxa)^idadeCorte

  custoPresente = 0
  custo5 = c(3172.83, 153.50, 146.75, 144.50, 144.5, 41.75)
  custo6 = c(3172.83, 153.50, 146.75, 144.50, 144.5, 144.5, 41.75)
  custo7 = c(3172.83, 153.50, 146.75, 144.50, 144.5, 144.5, 144.5, 41.75)
  custo8 = c(3172.83, 153.50, 146.75, 144.50, 144.5, 144.5, 144.5, 144.5, 41.75)
  custo = get(paste0('custo', idadeCorte))
  i = 1
  while(i <= length(custo)){
    custoPresente = custoPresente + (custo[i]/(1+taxa)^(i-1))
    i = i + 1
  }

  vpl = receitaPresente - custoPresente
  
  vet = (vpl*(1+taxa)^(length(custo)-1))/((1+taxa)^(length(custo)-1) - 1)
  
  return(vet)
}

vet('FZ1_1', 5, producao = producao, cadastro = cadastro, 
    receita = 70, taxa = 0.08)

fo = 'max:'

for(t in cadastro$Talhao){
  for (c in idadeCorte){
    
    fo = paste(fo, round(vet(t, c, producao = producao, cadastro = cadastro, 
                       receita = 70, taxa = 0.08), 2), paste(t,'_IC',c,sep=""), '+')
    
  }
}
