# ____________________SOCIODEMOGRÁFICOS PARA CIDS______________________________

#genero
#raca
#Idade
#faixa etaria
#menores 

## 4 panoramas - es - espsic - municipio - municipio psic

### LEGENDA
# dados es total: mortes total no es
# dados ci total: mortes totais no município 
# dados es psic: mortes por psicoativos no es
# dados ci psic: mortes por psicoativos no municipio 
# data_pop : base de dados das populações por ano nos municipios 
## _______________ RAÇA_______________________________________

#DADOS TOTAL

raca_CI_T <- data_pop %>%
  dplyr::filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor)
#dados_indic4$ano <- as.numeric(dados_indic4$ano)
raca_CI_T <- raca_CI_T %>%  inner_join(
  data.frame(
    dados_ci_total %>% group_by(ANOOBITO,RACACOR) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
raca_CI_T <- raca_CI_T |> mutate(Indic = (N.obitos/valor)*100000)

raca_es_T <- data_pop %>%
  dplyr::filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor)
#dados_indic5$ano <- as.numeric(dados_indic5$ano)
raca_es_T <- raca_es_T %>%  inner_join(
  data.frame(
    dados_es_total %>% group_by(ANOOBITO,RACACOR) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
raca_es_T <- raca_es_T |> mutate(Indic = (N.obitos/valor)*100000)



#DADOS PSIC 

raca_CI_P <- data_pop %>%
  dplyr::filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor)
#dados_indic4$ano <- as.numeric(dados_indic4$ano)
raca_CI_P <- raca_CI_P %>%  inner_join(
  data.frame(
    dados_ci_psic %>% group_by(ANOOBITO,RACACOR) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
raca_CI_P <- raca_CI_P |> mutate(Indic = (N.obitos/valor)*100000)

#ES 
raca_es_P <- data_pop %>%
  dplyr::filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor)
#dados_indic5$ano <- as.numeric(dados_indic5$ano)
raca_es_P <- raca_es_P %>%  inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO,RACACOR) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
raca_es_P <- raca_es_P |> mutate(Indic = (N.obitos/valor)*100000)

#_____________________________________________________________________________
# Gráfico da taxa de mortes por raça/cor
#es total 
graf_raca_es_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(raca_es_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 800) %>%
  hc_add_series(
    data = raca_es_T %>% filter(RACACOR == "Amarela") %>% pull(Indic),
    name = "Amarela",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_T %>% filter(RACACOR == "Branca") %>% pull(Indic),
    name = "Branca",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_T %>% filter(RACACOR == "Indígena") %>% pull(Indic),
    name = "Indígena",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_T %>% filter(RACACOR == "Negra") %>% pull(Indic),
    name = "Negra",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_T %>% filter(is.na(RACACOR)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1)) %>%
  hc_caption(
    text = "Gráfico 6 - Taxa de Mortes por Raça/Cor no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )
#____________________________________________________________________________

# ES PSIC
graf_raca_es_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(raca_es_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 14, tickInterval = 2) %>%
  
  hc_add_series(
    data = raca_es_P %>% filter(RACACOR == "Amarela") %>% pull(Indic),
    name = "Amarela",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_P %>% filter(RACACOR == "Branca") %>% pull(Indic),
    name = "Branca",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_P %>% filter(RACACOR == "Indígena") %>% pull(Indic),
    name = "Indígena",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_P %>% filter(RACACOR == "Negra") %>% pull(Indic),
    name = "Negra",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_es_P %>% filter(is.na(RACACOR)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1)) %>%
  hc_caption(
    text = "Gráfico 8 - Taxa de Mortes Causadas por Psicoativos por Raça/Cor no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )



# Cachoeiro total 
graf_raca_ci_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(raca_CI_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 800) %>%
  
  hc_add_series(
    data = raca_CI_T %>% filter(RACACOR == "Amarela") %>% pull(Indic),
    name = "Amarela",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_T %>% filter(RACACOR == "Branca") %>% pull(Indic),
    name = "Branca",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_T %>% filter(RACACOR == "Indígena") %>% pull(Indic),
    name = "Indígena",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_T %>% filter(RACACOR == "Negra") %>% pull(Indic),
    name = "Negra",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_T %>% filter(is.na(RACACOR)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 7 - Taxa de Mortes por Raça/Cor em Cachoeiro do Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

## CACHOEIRO PSIC 
graf_raca_ci_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(raca_CI_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 14, tickInterval = 2) %>%
  
  hc_add_series(
    data = raca_CI_P %>% filter(RACACOR == "Amarela") %>% pull(Indic),
    name = "Amarela",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_P %>% filter(RACACOR == "Branca") %>% pull(Indic),
    name = "Branca",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_P %>% filter(RACACOR == "Indígena") %>% pull(Indic),
    name = "Indígena",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_P %>% filter(RACACOR == "Negra") %>% pull(Indic),
    name = "Negra",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = raca_CI_P %>% filter(is.na(RACACOR)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 9 - Taxa de Mortes Causadas por Psicoativos por Raça/cor em Cachoeiro do Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )


## JUNTANDO OS GRAFICOS 

# TOTAIS

graf_r_1 <- browsable(
  htmltools::tagList(
    tags$h2("Graficos de Taxa de Mortes por Raça/Cor"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_raca_es_t, graf_raca_ci_t
    )))


#PSICOATIVOS
graf_r_2 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes Causadas por Psicoativos por Raça/Cor"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_raca_es_p, graf_raca_ci_p
    )))

