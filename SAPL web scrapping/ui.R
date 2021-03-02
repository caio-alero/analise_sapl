
#################################################
#            UI 
#################################################

library(shiny)


ui <- fluidPage(
  
  includeCSS(path = "www/AdminLTE.css"),
  includeCSS(path = "www/shinydashboard.css"),
  
  tags$div(
    class = 'title',
    tags$br(),
    tags$h1('Painel SAPL'),
    tags$h2('Resumo das matérias legislativas'),
    tags$br()
  ),
  
  fluidRow(
    valueBoxOutput(outputId = 'ml_total'),
    valueBoxOutput('ml_aprovadas'),
    valueBoxOutput('ml_rejeitadas')
  ),
  
  
  verticalTabsetPanel(
    color = wes_palette("Darjeeling2")[2],
    
    verticalTabPanel(
      title = 'Matérias apresentadas por mês/ano',
      #box_height = '80px',
      icon = icon("home", "fa-2x"),
      fluidRow(column(echarts4rOutput(outputId = 'chart1'), width = 9),
               column(echarts4rOutput(outputId = 'chart2'), width = 3))
    ),
    
    
    verticalTabPanel(
      title = 'B',
      #box_height = '80px'
    ),
    
    
    verticalTabPanel(
      title = 'C',
      #box_height = '80px'
    )
  )
)

