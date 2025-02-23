## 4 panoramas - es - espsic - municipio - municipio psic

#___________________ Gênero _________________________________

#MONTAGEM DOS DADOS

#CACHOEIRO TOTAL
genero_CI_T <- data_pop %>%
  dplyr::filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor)
genero_CI_T <- genero_CI_T %>%  inner_join(
  data.frame(
    dados_ci_total %>% group_by(ANOOBITO,SEXO) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
genero_CI_T <- genero_CI_T |> mutate(Indic = (N.obitos/valor)*100000)

#ES TOTAL
genero_es_T <- data_pop %>%
  dplyr::filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor)
genero_es_T <- genero_es_T %>%  inner_join(
  data.frame(
    dados_es_total %>% group_by(ANOOBITO,SEXO) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
genero_es_T <- genero_es_T |> mutate(Indic = (N.obitos/valor)*100000)


#DADOS PSIC 
#CACHOEIRO PISIC
genero_CI_P <- data_pop %>%
  dplyr::filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor)
#dados_indic4$ano <- as.numeric(dados_indic4$ano)
genero_CI_P <- genero_CI_P %>%  inner_join(
  data.frame(
    dados_ci_psic %>% group_by(ANOOBITO,SEXO) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
genero_CI_P <- genero_CI_P |> mutate(Indic = (N.obitos/valor)*100000)

#ESPÍRITO SANTO PSIC
genero_es_P <- data_pop %>%
  dplyr::filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor)
#dados_indic5$ano <- as.numeric(dados_indic5$ano)
genero_es_P <- genero_es_P %>%  inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO,SEXO) %>%
      summarise(N.obitos = n())
  ),
  by = c("ano" = "ANOOBITO")
)
genero_es_P <- genero_es_P |> mutate(Indic = (N.obitos/valor)*100000)

#GRAFICOS

# ES TOTAL

graf_genero_es_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(3)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(genero_es_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 800, tickInterval = 100) %>%
  
  hc_add_series(
    data = genero_es_T %>% filter(SEXO == "Feminino") %>% pull(Indic),
    name = "Feminino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_es_T %>% filter(SEXO == "Masculino") %>% pull(Indic),
    name = "Masculino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_es_T %>% filter(is.na(SEXO)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 10 - Taxa de Mortes por Gênero no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

#ES PSIC

graf_genero_es_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(3)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(genero_es_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 30, tickInterval = 5.0) %>%
  
  hc_add_series(
    data = genero_es_P %>% filter(SEXO == "Feminino") %>% pull(Indic),
    name = "Feminino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_es_P %>% filter(SEXO == "Masculino") %>% pull(Indic),
    name = "Masculino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_es_P %>% filter(is.na(SEXO)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 12 - Taxa de Mortes Causadas por Psicoativos por Gênero no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )


#CACHOEIRO TOTAL 


graf_genero_ci_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(3)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(genero_CI_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 800, tickInterval = 100) %>%
  
  hc_add_series(
    data = genero_CI_T %>% filter(SEXO == "Feminino") %>% pull(Indic),
    name = "Feminino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_CI_T %>% filter(SEXO == "Masculino") %>% pull(Indic),
    name = "Masculino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_CI_T %>% filter(is.na(SEXO)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 11 - Taxa de Mortes por Gênero em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

## CACHOEIRO PSICOATIVO

graf_genero_ci_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(3)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(genero_CI_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 30, tickInterval = 5.0) %>%
  
  hc_add_series(
    data = genero_CI_P %>% filter(SEXO == "Feminino") %>% pull(Indic),
    name = "Feminino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_CI_P %>% filter(SEXO == "Masculino") %>% pull(Indic),
    name = "Masculino",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = genero_CI_P %>% filter(is.na(SEXO)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 13 - Taxa de Mortes Causadas por Psicoativos por Gênero em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )



## JUNTANDO OS GRAFICOS 

# TOTAIS
grafico_g_1 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes por Gênero"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_genero_es_t, graf_genero_ci_t
    )))

#PSICOATIVOS
grafico_g_2 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes Causadas por Psicoativos por Gênero"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_genero_es_p, graf_genero_ci_p
    )))

