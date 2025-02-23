#_____________________________ MENOR DE IDADE _______________________________________

#***Bebê** - entre 0 e 2 anos\
#**Criança** - entre 3 e 8 anos\
#**Pré-Adolescente** - entre 9 e 12 anos\
#**Adolescente** - entre 13 e 17 anos\

#Análise filtrada para menores de idade foi feita apenas com os
#valores numéricos presentes na base de dados, sem considerar os valores faltantes da variável IDADE2

intervalos_m <- c(-Inf, 2, 8, 12, 17)  # Limites dos intervalos_m: 0-12, 13-24, 25-55, 56+
categorias_m <- c("Bebê", "criança", "Pré-Adolescente", "Adolescente")  # Rótulos
#_______________________________________________________________
# Processamento dos dados de Cachoeiro
menores_CI_T <- data_pop %>%
  filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_ci_total %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos_m, labels = categorias_m, right = FALSE)) %>%
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
menores_es_T <- data_pop %>%
  filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_es_total %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos_m, labels = categorias_m, right = FALSE)) %>%
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
menores_CI_P <- data_pop %>%
  filter(codigoMunicipio == 3201209) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_ci_psic %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos_m, labels = categorias_m, right = FALSE)) %>%
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
menores_es_P <- data_pop %>%
  filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor) %>%
  inner_join(
    dados_es_psic %>%
      mutate(Faixa_Etaria = cut(IDADE2, breaks = intervalos_m, labels = categorias_m, right = FALSE)) %>%
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

graf_menores_ci_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(menores_CI_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 55, tickInterval = 5) %>%
  
  hc_add_series(
    data = menores_CI_T %>% filter(Faixa_Etaria == "Bebê") %>% pull(Indic),
    name = "Bebê",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_CI_T %>% filter(Faixa_Etaria == "criança") %>% pull(Indic),
    name = "Criança",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_CI_T %>% filter(Faixa_Etaria == "Pré-Adolescente") %>% pull(Indic),
    name = "Pré-Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_CI_T %>% filter(Faixa_Etaria == "Adolescente") %>% pull(Indic),
    name = "Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 19 - Taxa de Mortes de Menores de Idade em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

# CACHOEIRO PSIC

graf_menores_ci_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Cachoeiro de Itapemirim") %>%
  hc_xAxis(categories = unique(menores_CI_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 1, tickInterval = 0.2) %>%
  
  hc_add_series(
    data = menores_CI_P %>% filter(Faixa_Etaria == "Bebê") %>% pull(Indic),
    name = "Bebê",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_CI_P %>% filter(Faixa_Etaria == "criança") %>% pull(Indic),
    name = "Criança",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_CI_P %>% filter(Faixa_Etaria == "Pré-Adolescente") %>% pull(Indic),
    name = "Pré-Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_CI_P %>% filter(Faixa_Etaria == "Adolescente") %>% pull(Indic),
    name = "Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 21 - Taxa de Mortes de Menores de Idade Causadas por Psicoativos em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )


## ES TOTAL

graf_menores_es_t <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(menores_es_T$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 55, tickInterval = 5) %>%
  
  hc_add_series(
    data = menores_es_T %>% filter(Faixa_Etaria == "Bebê") %>% pull(Indic),
    name = "Bebê",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_es_T %>% filter(Faixa_Etaria == "criança") %>% pull(Indic),
    name = "Criança",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_es_T %>% filter(Faixa_Etaria == "Pré-Adolescente") %>% pull(Indic),
    name = "Pré-Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_es_T %>% filter(Faixa_Etaria == "Adolescente") %>% pull(Indic),
    name = "Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 18 - Taxa de Mortes de Menores de Idade no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )


#ES PSIC
graf_menores_es_p <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(5)) %>%
  hc_title(text = "Espírito Santo") %>%
  hc_xAxis(categories = unique(menores_es_P$ano), 
           title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 1, tickInterval = 0.2) %>%
  
  hc_add_series(
    data = menores_es_P %>% filter(Faixa_Etaria == "Bebê") %>% pull(Indic),
    name = "Bebê",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_es_P %>% filter(Faixa_Etaria == "criança") %>% pull(Indic),
    name = "Criança",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_es_P %>% filter(Faixa_Etaria == "Pré-Adolescente") %>% pull(Indic),
    name = "Pré-Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_add_series(
    data = menores_es_P %>% filter(Faixa_Etaria == "Adolescente") %>% pull(Indic),
    name = "Adolescente",
    marker = list(symbol = "circle")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 1))%>%
  hc_caption(
    text = "Gráfico 20 - Taxa de Mortes de Menores de Idade Causadas por Psicoativos no Espírito Santo",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

#%>%
 # hc_add_series(
  #  data = menores_es_P %>% filter(is.na(Faixa_Etaria)) %>% pull(Indic),
   # name = "Não informado",
    #marker = list(symbol = "circle")

## JUNTANDO OS GRAFICOS 

# TOTAIS
graf_mi_1 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes de Menores de Idade"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_menores_es_t, graf_menores_ci_t
    )))

#PSICOATIVOS
graf_mi_2 <- browsable(
  htmltools::tagList(
    tags$h2("Gráficos de Taxa de Mortes de Menores de Idade Causadas por Psicoativos"),
    tags$div(
      style = "display: flex; justify-content: space-around;",
      graf_menores_es_p, graf_menores_ci_p
    )))

#_______________________ CIDS MAIS COMUNS __________________________________

dados_menores <- dados_ci_psic %>%
  filter(IDADE2 < 18)
# Contar a frequência de cada causa básica
dados_causas <- dados_menores %>%
  count(CAUSABAS, name = "quantidade") %>%
  arrange(desc(quantidade))  # Ordenar do maior para o menor

# Criar o gráfico de barras
graf_mi_cid <- highchart() %>%
  hc_chart(type = "column") %>%  # Define o tipo como coluna (barras verticais)
  hc_xAxis(categories = dados_causas$CAUSABAS, title = list(text = "CID")) %>%
  hc_yAxis(title = list(text = "Quantidade"), min = 0) %>%
  hc_add_series(
    name = "Ocorrências",
    data = dados_causas$quantidade
  ) %>%
  hc_title(text = "Distribuição das Causas Básicas de Óbito") %>%
  hc_tooltip(pointFormat = "Quantidade: {point.y}") %>%
  hc_plotOptions(column = list(dataLabels = list(enabled = TRUE)))  # Exibir valores sobre as barras
