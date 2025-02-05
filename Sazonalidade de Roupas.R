# Carregando pacotes necess�rios
library(sidrar)
library(tidyverse)
library(dplyr)
library(zoo)
library(fabletools)
library(feasts)
library(tsibble)
library(mFilter)
library(lubridate)
library(fastDummies)
library(lmtest)
library(reshape2)

# Criando Fun��es de Coleta
colet_ipca = function(){
  periodo = "all"
  var = 63
  IPCA = rbind(get_sidra(1419, 
                         variable = var,
                         period = periodo, 
                         geo = 'Brazil'),
               get_sidra(7060, 
                         variable = var,
                         period = periodo, 
                         geo = 'Brazil'),
               get_sidra(1419, 
                         variable = 66,
                         period = periodo, 
                         geo = 'Brazil'),
               get_sidra(7060, 
                         variable = 66,
                         period = periodo, 
                         geo = 'Brazil'))
  di = as.Date("01/01/2012",format = "%d/%m/%Y")
  datas = data.frame(unique(IPCA$M�s), seq(di,length = length(unique(IPCA$M�s)),by = "months"))
  colnames(datas) = c("M�s", "Data")
  IPCA = left_join(datas, IPCA, by = "M�s")
  IPCA$code = as.numeric(gsub("([0-9]+).*$", "\\1", IPCA$`Geral, grupo, subgrupo, item e subitem`))
  cate = data.frame(unique(nchar(IPCA$code)),c("Geral","Grupo","Subgrupo","Item","Subitem"))
  colnames(cate) = c('car','cate')
  IPCA$car = nchar(IPCA$code)
  IPCA = left_join(IPCA, cate, by = "car")
  IPCA$grupo = substr(IPCA$cod,1,1)
  IPCA$subgrupo = substr(IPCA$cod,1,2)
  IPCA$subgrupo[nchar(IPCA$cod)<2] = NA
  IPCA$item = substr(IPCA$cod,1,4)
  IPCA$item[nchar(IPCA$cod)<4] = NA
  IPCA = IPCA %>% select(Data, grupo, subgrupo, item, code, `Geral, grupo, subgrupo, item e subitem`, Valor, cate, `Vari�vel (C�digo)`)
  colnames(IPCA) = c('data','grupo','subgrupo','item','cod','desc','v_perc','categoria','id')
  IPCA$cod = as.character(IPCA$cod)
  dados = IPCA %>% filter(id == var)
  PESO = IPCA %>% filter(id == 66)
  dados$peso = PESO[,7]
  dados = dados[,-9]
  
  return(dados)
}
inf_month = function(dados, mes,dg, numi){
  IPCA = dados
  for (j in unique(IPCA$desc)) {
    data = IPCA %>% filter(desc == j) %>% mutate(v_perc = v_perc/100)
    data[1,7] = 1+data[1,7]
    for (i in 2:length(data[,7])) {
      data[i,7] = data[i-1,7]*(1+data[i,7])
    }
    if (j == unique(IPCA$desc)[1]) {
      NI = data
    } else {
      NI = rbind(NI,data)
    }
  }
  
  if (numi == TRUE) {
    return(NI)
  } else {
    
    for (n in unique(NI$desc)) {
      data = NI %>% filter(desc == n)
      data$v_perc = round((data$v_perc/lag(data$v_perc, n = mes)-1)*100,digits = dg)
      data = data[is.na(data$v_perc)==FALSE,]
      
      if (n == unique(NI$desc)[1]) {
        result = data
      } else {
        result = rbind(result,data)
      }
    }
    
    return(result)

  }
}

# Coletando dados
IPCA = colet_ipca()

# Calculando n�mero �ndice dos bens
INF = inf_month(IPCA, 12, 6, numi = TRUE)

# Organizando base de dados
IPCA$desc = gsub('[[:digit:]]+\\.', '', IPCA$desc)
INF$desc = gsub('[[:digit:]]+\\.', '', INF$desc)
INF$data = as.Date(INF$data)

# Identificando C�digos
Roupas = INF %>%
  filter(grupo == 4)
cod = unique(Roupas[,c(5,6)])

# Escolhendo c�digo para analisar
can = '41'

# Selecionando dados para an�lise
analise = Roupas %>% filter(cod == can) %>% select(data,v_perc)
analise$v_perc = analise$v_perc*100
comp = analise
comp$data = yearmonth(comp$data)
comp = comp %>% as_tsibble(index = data)
colnames(comp) = c('data','pre�o')

# Decomponto a s�rie temporal
comp = comp %>% model(STL(pre�o, robust = TRUE)) %>% components()
comp = comp[,-1]

# Base de dados para gr�ficos
graf = cbind(analise,comp[,3:6])
colnames(graf) = c('Data','Pre�o','Tend�ncia', 'Sazonalidade', 'Erro', 'Ajuste Sazonal')
graf = melt(graf, id.vars = 'Data')

# Construindo gr�fico do �ndice de pre�os de roupas
graf %>%
  filter(variable %in% c("Pre�o", "Ajuste Sazonal")) %>%
  ggplot() +
  aes(x = Data, y = value, colour = variable) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  labs(
    x = "Data",
    y = "�ndice",
    title = "Indice Geral de Pre�os ao Consumidor Amplo - Roupas",
    caption = "Fonte: IBGE. Elabora��o: Ev�nio Marques",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14L,
                              face = "bold")
  )

# Construindo gr�fico de sazonalidade
graf %>%
  filter(variable %in% "Sazonalidade") %>%
  ggplot() +
  aes(x = Data, y = value, colour = variable) +
  geom_line(size = 1.2) +
  scale_color_viridis_d(option = "viridis", direction = 1) +
  labs(
    x = "Data",
    y = "Sazonalidade",
    title = "Sazonalidade no pre�o de roupas",
    caption = "Fonte: IBGE. Elabora��o: Ev�nio Marques",
    color = "Legenda"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14L,
                              face = "bold")
  )

# Base de dados para analisar sazonalidade
saz = comp %>% select(data, season_year)
saz$mes = month(saz$data)
saz = saz %>% mutate(mes = case_when(mes == 1 ~ 'Janeiro',
                               mes == 2 ~ 'Fevereiro',
                               mes == 3 ~ 'Mar�o',
                               mes == 4 ~ 'Abril',
                               mes == 5 ~ 'Maio',
                               mes == 6 ~ 'Junho',
                               mes == 7 ~ 'Julho',
                               mes == 8 ~ 'Agosto',
                               mes == 9 ~ 'Setembro',
                               mes == 10 ~ 'Outubro',
                               mes == 11 ~ 'Novembro',
                               mes == 12 ~ 'Dezembro')) 
saz = dummy_cols(saz, select_columns = 'mes')

# Calculando M�dia
modelo = lm(season_year ~ -1 + mes_Janeiro + mes_Fevereiro + mes_Mar�o + mes_Abril + mes_Maio + mes_Junho + mes_Julho + mes_Agosto + mes_Setembro + mes_Outubro + mes_Novembro + mes_Dezembro, saz)
summary(modelo)
dados = data.frame(coefci(modelo, level = 0.99))  

# Organizando dados para gr�fico
colnames(dados) = c('inferior','superior')
dados$coef = modelo$coefficients
dados$mes = c(1:12)
dados$mes = factor(dados$mes, labels = c('Janeiro',
                             'Fevereiro',
                             'Mar�o',
                             'Abril',
                             'Maio',
                             'Junho',
                             'Julho',
                             'Agosto',
                             'Setembro',
                             'Outubro',
                             'Novembro',
                             'Dezembro'))
  
  


# Gr�fico da sazonalidade
ggplot(dados) +
  aes(x = mes, y = coef) +
  geom_point(shape = "circle", fill = "#112446", size = 2) +
  geom_errorbar( aes(x = mes, ymin = inferior, ymax = superior), colour = "#112446", width = 0.4, alpha=0.9, size=1) +
  geom_hline(yintercept = 0, colour = 'red') + 
  labs(
    x = "Data",
    y = "Sazonalidade",
    title = "M�dia da Sazonalidade por M�s",
    caption = "Fonte: IBGE. Elabora��o: Ev�nio Marques"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(size = 14L, face = "bold"))
                