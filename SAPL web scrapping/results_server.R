#################################################
#            RESULTADOS SERVER 
#################################################

library(dplyr)
library(lubridate)

#----------- leitura dos dados -------------------
sapl_data <- read.table('https://raw.githubusercontent.com/caio-alero/analise_sapl/main/dados_sapl', header = TRUE, sep = '\t', encoding = 'UTF-8')

sapl_data <- sapl_data %>% 
  mutate(data_apresentacao = format(lubridate::dmy(data_apresentacao), '%d/%m/%Y'),
         data_ultima_tram = format(lubridate::dmy(data_ultima_tram), '%d/%m/%Y'),
         mes_apresentacao = lubridate::month(data_apresentacao, label = TRUE, abbr = TRUE),
         ano_apresentacao = substring(data_apresentacao, 7, 11))


#----------------------------------------
#     números gerais
#----------------------------------------

var_tab <- sapl_data %>% 
  count(mes_apresentacao, ano_apresentacao) %>% 
  mutate(ano_apresentacao = as.factor(ano_apresentacao))

# total de matérias no ano
var_tab_anual <- var_tab %>%
  group_by(ano_apresentacao) %>% 
  summarise(n = sum(n)) %>% 
  mutate(variacao = (n/lag(n) - 1)*100)

# total de matérias no mês
var_tab_mensal <- var_tab %>%
  filter(ano_apresentacao == year(Sys.Date()),
         mes_apresentacao == month(Sys.Date(), label = TRUE))


# total de matérias trans. em lei
total_ap_ano <- sapl_data %>% 
  filter(ano_apresentacao == year(Sys.Date()),
         status == c('Proposição Transformada Em Lei')) %>% 
  count(status) %>% 
  pull(n)


# total de matérias arquivadas
sapl_data %>% 
  filter(ano_apresentacao == year(Sys.Date()),
         status == c('Proposição Arquivada')) %>% 
  count(status) %>% 
  pull(n)

#----------------------------------------------
#       gráficos
#----------------------------------------------


# qtd de materias por mes/ano
sapl_data %>%
  count(mes_apresentacao, ano_apresentacao) %>% 
  mutate(mes_apresentacao = as.character(mes_apresentacao)) %>%
  group_by(ano_apresentacao) %>% 
  e_charts(x = mes_apresentacao) %>% 
  e_line(n, emphasis = list(itemStyle = list(shadowBlur = 10))) %>% 
  e_mark_point(data = list(type = 'max')) %>% 
  e_tooltip(trigger = 'axis') %>% 
  e_theme("walden") #%>% 
#e_toolbox_feature("saveAsImage", title = 'save as image')


