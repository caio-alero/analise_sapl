#################################################
#            SERVER 
#################################################

library(shiny)

server <- function(input, output, session) {
  
  #options(OutDec = ',')
  
  
  #----------- leitura dos dados -------------------
  sapl_data <- read.table('https://raw.githubusercontent.com/caio-alero/analise_sapl/main/dados_sapl', header = TRUE, sep = '\t')
  
  sapl_data <- sapl_data %>% 
    mutate(data_apresentacao = format(lubridate::dmy(data_apresentacao), '%d/%m/%Y'),
           data_ultima_tram = format(lubridate::dmy(data_ultima_tram), '%d/%m/%Y'),
           mes_apresentacao = lubridate::month(data_apresentacao, label = TRUE, abbr = TRUE),
           ano_apresentacao = substring(data_apresentacao, 7, 11))
  
  
  #----------------------------------------
  #     total de materias valuebox
  #----------------------------------------
  
  variacao1 <- sapl_data %>% 
    count(ano_apresentacao) %>% 
    mutate(variacao = (n/lag(n) - 1)*100)
  
  output$ml_total <- renderInfoBox({
    
    ind1 <- sapl_data %>% 
      filter(ano_apresentacao == lubridate::year(Sys.Date())) %>% 
      count(ano_apresentacao) %>% 
      pull()
    
    variacao_atual <- variacao1[nrow(variacao1), 'variacao']
    
    valueBox2(title = toupper('Matérias apresentadas'), 
              subtitle =  tagList(HTML(ifelse(variacao_atual > 0, '&uarr;', '&darr;')), 
                                  paste0(round(variacao_atual, 1), '%'),
                                  'em relação ao ano anterior'),
              value = ind1,
              icon = icon("bar-chart"))
  })
  
  #----------------------------------------------
  #       serie temporal de materias por mes
  #----------------------------------------------
  
  output$chart1 <- renderEcharts4r({
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
  })
  
  output$chart2 <- renderEcharts4r({
    variacao1 %>% 
      mutate(ano_apresentacao = as.factor(ano_apresentacao)) %>%
      e_charts(x = ano_apresentacao) %>% 
      e_bar(serie = n) %>% 
      e_tooltip(trigger = 'axis') %>% 
      e_legend(show = FALSE) %>% 
      #e_labels(show = TRUE, fontSize = 12) %>% 
      e_y_axis(show = FALSE) %>% 
      e_theme('walden') 
  })
  


}

