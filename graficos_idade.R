#_____________________________ FAIXA ETÁRIA _______________________________________

#**Menor de idade** - entre 0 e 17 anos\
#**Jovem-Adulto** - entre 18 e 30 anos\
#**Adulto** - entre 31 e 55 anos\
#**Idoso** - acima de 55 anos\

intervalos <- c(0, 17, 30, 55, Inf)  # Limites dos intervalos: 0-12, 13-24, 25-55, 56+
categorias <- c("Menor de idade", "Jovem-Adulto", "Adulto", "Idoso")
#_________________________________________________________________

# Processamento dos dados de Cachoeiro
faixa_CI_T <- data_pop %>%
  filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_ci_total %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos, labels = categorias, right = FALSE)) %>%
      group_by(ANOOBITO, Faixa_Etaria) %>%  # Agrupa por ano e faixa etária
      summarise(N.obitos = n(), .groups = "drop"),
    by = c("ano" = "ANOOBITO")
  ) %>%
  group_by(ano, Faixa_Etaria) %>%  # Garante agregação correta
  summarise(
    N.obitos = sum(N.obitos),
    valor = first(valor),  # Mantém a população do ano correspondente
    Indic = (N.obitos / valor) * 100000,
    .groups = "drop"
  )


#ES TOTAL


# Processamento dos dados de Cachoeiro
faixa_es_T <- data_pop %>%
  filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_es_total %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos, labels = categorias, right = FALSE)) %>%
      group_by(ANOOBITO, Faixa_Etaria) %>%  # Agrupa por ano e faixa etária
      summarise(N.obitos = n(), .groups = "drop"),
    by = c("ano" = "ANOOBITO")
  ) %>%
  group_by(ano, Faixa_Etaria) %>%  # Garante agregação correta
  summarise(
    N.obitos = sum(N.obitos),
    valor = first(valor),  # Mantém a população do ano correspondente
    Indic = (N.obitos / valor) * 100000,
    .groups = "drop"
  )


#DADOS PSIC 
#CACHOEIRO PISIC
faixa_CI_P <- data_pop %>%
  filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_ci_psic %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos, labels = categorias, right = FALSE)) %>%
      group_by(ANOOBITO, Faixa_Etaria) %>%  # Agrupa por ano e faixa etária
      summarise(N.obitos = n(), .groups = "drop"),
    by = c("ano" = "ANOOBITO")
  ) %>%
  group_by(ano, Faixa_Etaria) %>%  # Garante agregação correta
  summarise(
    N.obitos = sum(N.obitos),
    valor = first(valor),  # Mantém a população do ano correspondente
    Indic = (N.obitos / valor) * 100000,
    .groups = "drop"
  )

#ESPÍRITO SANTO PSIC
faixa_es_P <- data_pop %>%
  filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_es_psic %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos, labels = categorias, right = FALSE)) %>%
      group_by(ANOOBITO, Faixa_Etaria) %>%  # Agrupa por ano e faixa etária
      summarise(N.obitos = n(), .groups = "drop"),
    by = c("ano" = "ANOOBITO")
  ) %>%
  group_by(ano, Faixa_Etaria) %>%  # Garante agregação correta
  summarise(
    N.obitos = sum(N.obitos),
    valor = first(valor),  # Mantém a população do ano correspondente
    Indic = (N.obitos / valor) * 100000,
    .groups = "drop"
  )


## GRAFICOS

#CACHOEIRO TOTAL
graf_idade_ci_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(faixa_CI_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 1100, tickInterval = 200) %>%
  hc_add_series(
    data = faixa_CI_T %>% filter(Faixa_Etaria == "Menor de idade") %>% pull(Indic),
    name = "Menor de Idade",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_T %>% filter(Faixa_Etaria == "Jovem-Adulto") %>% pull(Indic),
    name = "Jovem-Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_T %>% filter(Faixa_Etaria == "Adulto") %>% pull(Indic),
    name = "Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_T %>% filter(Faixa_Etaria == "Idoso") %>% pull(Indic),
    name = "Idoso",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_T %>% filter(is.na(Faixa_Etaria)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 15 - Taxa de Mortes por Faixa-Etária em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

#ES TOTAL

graf_idade_es_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(faixa_es_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 1100, tickInterval = 200) %>%
  
  hc_add_series(
    data = faixa_es_T %>% filter(Faixa_Etaria == "Menor de idade") %>% pull(Indic),
    name = "Menor de Idade",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_T %>% filter(Faixa_Etaria == "Jovem-Adulto") %>% pull(Indic),
    name = "Jovem-Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_T %>% filter(Faixa_Etaria == "Adulto") %>% pull(Indic),
    name = "Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_T %>% filter(Faixa_Etaria == "Idoso") %>% pull(Indic),
    name = "Idoso",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_T %>% filter(is.na(Faixa_Etaria)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 14 - Taxa de Mortes por Faixa-Etária no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )


## PSICOATIVOS

## CACHOEIRO

graf_idade_ci_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(faixa_CI_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 14, tickInterval = 2) %>%
  
  hc_add_series(
    data = faixa_CI_P %>% filter(Faixa_Etaria == "Menor de idade") %>% pull(Indic),
    name = "Menor de Idade",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_P %>% filter(Faixa_Etaria == "Jovem-Adulto") %>% pull(Indic),
    name = "Jovem-Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_P %>% filter(Faixa_Etaria == "Adulto") %>% pull(Indic),
    name = "Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_P %>% filter(Faixa_Etaria == "Idoso") %>% pull(Indic),
    name = "Idoso",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_CI_P %>% filter(is.na(Faixa_Etaria)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 17 - Taxa de Mortes Causadas por Psicoativos por Faixa-Etária em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )



## es

graf_idade_es_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(faixa_es_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 14, tickInterval = 2) %>%
  
  hc_add_series(
    data = faixa_es_P %>% filter(Faixa_Etaria == "Menor de idade") %>% pull(Indic),
    name = "Menor de Idade",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_P %>% filter(Faixa_Etaria == "Jovem-Adulto") %>% pull(Indic),
    name = "Jovem-Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_P %>% filter(Faixa_Etaria == "Adulto") %>% pull(Indic),
    name = "Adulto",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_P %>% filter(Faixa_Etaria == "Idoso") %>% pull(Indic),
    name = "Idoso",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = faixa_es_P %>% filter(is.na(Faixa_Etaria)) %>% pull(Indic),
    name = "Não informado",
    marker = list(symbol = "circle")
  ) %>%
  
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 16 - Taxa de Mortes Causadas por Psicoativos por Faixa-Etária no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

## JUNTANDO OS GRAFICOS 

# TOTAIS
graf_i_1 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes por Faixa-Etária"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_idade_es_t, graf_idade_ci_t
    )))

#PSICOATIVOS
graf_i_2 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes Causadas por Psicoativos por Faixa-Etária"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_idade_es_p, graf_idade_ci_p
    )))




