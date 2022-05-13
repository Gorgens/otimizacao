require(readxl)
require(tidyverse)
require(lubridate)

cadastro = read_xlsx('cadastro.xlsx')
cadastro$Plantio = as.Date(cadastro$Plantio, format="%Y-%m-%d")
cadastro$anoPlantio = year(cadastro$Plantio)
producao = read_xlsx('tabela_producao.xlsx')

idadeCorte = c(5, 6, 7, 8)
meta = 80000
horizonte = 10

vet = function(
  talhao, 
  idadeCorte, 
  producao, 
  cadastro, 
  receitaM3 = 70, 
  taxa = 0.1){

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


## Função objetivo -----------------------------------------
fo = 'max:'
for(t in cadastro$Talhao){
  for (c in idadeCorte){
    
    fo = paste(fo, round(vet(t, c, producao = producao, cadastro = cadastro, 
                       receita = 70, taxa = 0.10), 2), paste(t,'_IC',c,sep=""), '+')
    
  }
}

## Restrição de área ----------------------------------------
resarea = 'S.A.'
for(t in cadastro$Talhao){
  
  areaTemp = paste('AREA_', t, ":", sep="")
  
  for (c in idadeCorte){
    areaTemp = paste(areaTemp, paste(t,'_IC',c,sep=""), '+')
    
  }
  areaTalhao = cadastro %>%
    filter(Talhao == t) %>%
    select(Area)
  
  #resarea = paste(resarea, areaTemp, "=", as.numeric(areaTalhao), ";")
  resarea = paste(resarea, areaTemp, "=", 1, ";")
}
rm(t, c, areaTemp)

## Restrição produção mínima ---------------------------------
matrizProducao = function(
  idadeCorte, 
  producao, 
  cadastro,
  horizonte = 10){
  
  temp = data.frame(alternativa = c(), ano = c(), producao = c())
  for(h in seq(1:horizonte)-1){
    for(t in cadastro$Talhao){
    
    plantio = cadastro %>%
      filter(Talhao == t) %>%
      select(anoPlantio) %>% as.numeric()
    
      for(c in idadeCorte){
        
        if((year(today())-plantio+h)%%c == 0){
          a = paste(t,'_IC',c,sep="")
          b = year(today()) + h
          volTalhao = producao %>%
            filter(Sitio == as.numeric(cadastro[cadastro$Talhao == t,'Sitio'])) %>%
            filter(MatGen == as.character(cadastro[cadastro$Talhao == t,'MatGen'])) %>%
            filter(Idade == c) %>%
            select(m3ha) %>% as.numeric()
          areaTalhao = cadastro %>%
            filter(Talhao == t) %>%
            select(Area) %>% as.numeric()
          c =  volTalhao * areaTalhao
          temp = rbind(temp, data.frame(alternativa = a, ano = b, producao = c))
        }

      }
    }
  }
  return(temp)
}

matriz = matrizProducao(idadeCorte = idadeCorte, producao = producao, cadastro = cadastro, horizonte = 10)

resproducao = 'S.A.'
for(h in seq(1:horizonte)-1){
  temp = matriz %>%
    filter(ano == year(today()) + h)
  restemp = paste('Prod', year(today()) + h, ':', sep="")
  for(a in temp$alternativa){
    prodTemp = temp %>%
      filter(alternativa == a) %>%
      select(producao) %>% as.numeric()
    restemp = paste(restemp, prodTemp, a, "+") 
  }
  resproducao = paste(resproducao, restemp, ">=", meta, ";")
}

rm(a, h, prodTemp, restemp)

## Corte por talhão -------------------------------

resbin = "BIN"
for(t in cadastro$Talhao){
  for (c in idadeCorte){
    resbin = paste(resbin, paste(t,'_IC',c,sep=""))
  }
}
