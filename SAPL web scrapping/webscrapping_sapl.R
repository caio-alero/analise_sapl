library(rvest)      # web scrapping
library(dplyr)      # manipular dados
library(stringr)    # manipular os textos
library(xml2)       # ler html
library(jbkmisc)
library(tm)

# o código de scrape a seguir serve tanto para PLO como PLC
# tipos de materia: PLO, PLC, VP
# uma_pagina = TRUE ou FALSE

#sapl_scrap <- function(URL, tipo_materia = NULL, uma_pagina = FALSE) {
sapl_scrap <- function(URL, uma_pagina = FALSE) {

  # extraindo o numero total de paginas
  num_total_pages <- ifelse(uma_pagina,
                            1,
                            str_replace(URL, 'page=PAGE&', '') %>% 
                              read_html() %>% 
                              xml_find_all('//nav/ul') %>% 
                              xml_text() %>% 
                              str_clean() %>%
                              str_match_all('[0-9]+') %>% 
                              unlist() %>% 
                              as.numeric() %>% 
                              max())

  
  
  # scrapping das ementas
  num_projeto <- list()
  data_apresentacao <- list()
  localizacao_atual <- list()
  autor <- list()
  ementas <- list()
  status <- list()
  data_ultima_tram <- list()
  ultima_acao <- list()
  
  for(i in 1:num_total_pages) {
    
    print(i)
    
    url <- str_replace(URL, 'PAGE', as.character(i))
    pagina <- read_html(url) %>% xml_root()
    
    # numero de materia
    nodes_link <- xml_find_all(pagina, '//td//a')
    #num_projeto[[i]] <- xml_text(nodes_link)[str_detect(xml_text(nodes_link), tipo_materia)]
    num_projeto[[i]] <- xml_text(nodes_link)[!(xml_text(nodes_link) %in% c('Texto Original', '', 'Acompanhar Matéria'))]
    num_projeto[[i]] <- num_projeto[[i]][!str_detect(num_projeto[[i]], pattern = '\n')]
    
    
    # textos
    textos <- xml_find_all(pagina, '//td') %>% 
      xml_text() %>% 
      str_clean()
    
    # textos <- xml_find_all(pagina, '//td/div') %>% 
    #   xml_text() %>% 
    #   str_clean()
    
    textos <- textos[textos != ' ']
    textos <- textos[-1]
    
    # data de apresentacao
    data_apresentacao[[i]] <- xml_find_all(pagina, '//td/text()[6]') %>% 
      xml_text() %>% 
      str_clean()
    
    # autor[[i]] <- xml_find_all(pagina, '//td/text()[6]') %>% 
    #   xml_text() %>% 
    #   str_clean()
    autor[[i]] <- ifelse(str_detect(textos, 'Autor'),
                      ifelse(str_detect(textos, 'Localização Atual'), 
                             gsub('.*Autor: (.+) Localização Atual.*', '\\1', textos),
                             removeWords(gsub('.*Autor: (.+)', '\\1', textos), c('Texto Original', 'Acompanhar Matéria'))),
                      NA) 
    
    autor[[i]] <- gsub('Audiência.*', '', autor[[i]]) %>% 
      stringi::stri_trim_right()
    # localizacao_atual[[i]] <- xml_find_all(pagina, '//td/text()[7]') %>% 
    #   xml_text() %>% 
    #   str_clean()
    loc_atual <- ifelse(str_detect(textos, 'Localização Atual'),
                        gsub('.*Localização Atual: (.+) Status.*', '\\1', textos),
                        NA)
    localizacao_atual[[i]] <- ifelse(str_detect(textos, 'Localização Atual:'), loc_atual, NA)
    
    
    ementas[[i]] <- str_replace(URL, 'PAGE', as.character(i)) %>% 
      read_html() %>% 
      html_nodes('.dont-break-out') %>% 
      html_text()
    
    # status[[i]] <- xml_find_all(pagina, '//td/text()[8]') %>% 
    #   xml_text() %>% 
    #   str_clean()
    status2 <- ifelse(str_detect(textos, 'Status'), 
                      gsub('.*Status: (.+) Data Fim Prazo.*', '\\1', textos), 
                      NA)
    status[[i]] <- ifelse(str_detect(textos, 'Status:'), status2, NA)
    
    
    # data_ultima_tram[[i]] <- xml_find_all(pagina, '//td/text()[10]') %>% 
    #   xml_text() %>% 
    #   str_clean()
    dataUltimaTram <- ifelse(str_detect(textos, 'Data Da Última Tramitação'), 
                             gsub('.*Data Da Última Tramitação: (.+) Ultima Ação.*', '\\1', textos),
                             NA)
    data_ultima_tram[[i]] <- ifelse(str_detect(textos, 'Data Da Última Tramitação:'), dataUltimaTram, NA)
    
    
    # ultima_acao[[i]] <- xml_find_all(pagina, '//td/text()[11]') %>% 
    #   xml_text() %>% 
    #   str_clean()
    ultimaAcao <- ifelse(str_detect(textos, 'Ultima Ação'), 
                         ifelse(str_detect(textos, 'Texto Original'), 
                                gsub('.*Ultima Ação: (.+) Texto Original.*', '\\1', textos),
                                gsub('.*Ultima Ação: (.+)', '\\1', textos)
                         ),
                         NA)
    
    #ultimaAcao <- gsub('.*Ultima Ação: (.+) Texto Original.*', '\\1', textos) 
    ultima_acao[[i]] <- ifelse(str_detect(textos, 'Ultima Ação:'), ultimaAcao, NA)
    
    
  }
  
  
  sapl_data <- tibble(num_projeto = unlist(num_projeto),
                      data_apresentacao = unlist(data_apresentacao),
                      autor = unlist(autor),
                      localizacao_atual = unlist(localizacao_atual),
                      ementas = unlist(ementas),
                      status = unlist(status),
                      data_ultima_tram = unlist(data_ultima_tram),
                      ultima_acao = unlist(ultima_acao))
  
  return(sapl_data)
}

tempo_inicial <- Sys.time()
sapl_data <- sapl_scrap(URL = 'https://sapl.al.ro.leg.br/materia/pesquisar-materia?page=PAGE&tipo=&ementa=&numero=&numeracao__numero_materia=&numero_protocolo=&ano=&o=&tipo_listagem=1&tipo_origem_externa=&numero_origem_externa=&ano_origem_externa=&data_origem_externa_0=&data_origem_externa_1=&local_origem_externa=&data_apresentacao_0=01%2F01%2F2019&data_apresentacao_1=31%2F01%2F2021&data_publicacao_0=&data_publicacao_1=&autoria__autor=&autoria__primeiro_autor=unknown&autoria__autor__tipo=&autoria__autor__parlamentar_set__filiacao__partido=&relatoria__parlamentar_id=&em_tramitacao=&tramitacao__unidade_tramitacao_destino=&tramitacao__status=&materiaassunto__assunto=&indexacao=')
Sys.time() - tempo_inicial

for(i in 1: nrow(sapl_data)) {
  if(grepl('PLO', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'PLO' 
  if(grepl('PLC', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'PLC'
  if(grepl('PRE', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'PRE'
  if(grepl('PEC', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'PEC'
  if(grepl('VT', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'VT'
  if(grepl('VP', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'VP'
  if(grepl('IND', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'IND'
  if(grepl('REQ', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'REQ'
  if(grepl('ECM', as.character(sapl_data$num_projeto[i]), fixed = TRUE)) sapl_data$projeto[i] <- 'ECM'
}


sapl_data$data <- gsub(' De ', '', sapl_data$data_apresentacao) %>% 
  strptime(format = '%d %B %Y')

saveRDS(sapl_data, file = 'sapl_data.rds')


write.csv2(sapl_data[,-5], 'sapl_data.csv', row.names = FALSE)

