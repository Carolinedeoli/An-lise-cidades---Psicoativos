#baixar base de apreensoes



library(dplyr)
library(highcharter)

# INDICADOR 1

# Preparando os dados
dados_indic1 <- inner_join(
  dados_vix_psic %>%
    group_by(ANOOBITO) %>%
    summarise(N_obitos_vix_psic = n()),
  dados_vix_total %>%
    filter(IDADE2 > 17) %>%
    group_by(ANOOBITO) %>%
    summarise(N_obitos_vix_total = n()),
  by = "ANOOBITO"
)

dados_indic1 <- dados_indic1 %>%
  mutate(indic1 = (N_obitos_vix_psic / N_obitos_vix_total) * 1000)

# Criando o gráfico com highcharter
highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(1)) %>%
  hc_title(text = "Série do Indicador 1 de 2013 a 2022") %>%
  hc_xAxis(categories = dados_indic1$ANOOBITO, title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/1000"),
           min = 0, max = 30, tickInterval =2) %>%
  hc_add_series(data = dados_indic1$indic1, 
                name = "Indicador 1", 
                marker = list(symbol = "triangle-down")) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 0.5))


library(dplyr)
library(highcharter)

# INDICADORES 2 E 3

# Preparando os dados do Indicador 2
dados_indic2 <- inner_join(
  dados_es_psic %>%
    group_by(ANOOBITO) %>%
    summarise(N_obitos_vix_psic = n()),
  dados_br_psic %>%
    group_by(ANOOBITO) %>%
    summarise(N_obitos_es_psic = n()),
  by = "ANOOBITO"
)

dados_indic2 <- dados_indic2 %>%
  mutate(indic2 = (N_obitos_vix_psic / N_obitos_es_psic) * 100)

# Preparando os dados do Indicador 3
dados_indic3 <- inner_join(
  dados_es_total %>%
    group_by(ANOOBITO) %>%
    summarise(N_obitos_vix_total = n()),
  dados_br_total %>%
    group_by(ANOOBITO) %>%
    summarise(N_obitos_es_total = n()),
  by = "ANOOBITO"
)

dados_indic3 <- dados_indic3 %>%
  mutate(indic3 = (N_obitos_vix_total / N_obitos_es_total) * 100)

# Combinando os dados dos Indicadores 2 e 3
dados_indic2_3 <- data.frame(
  Ano = rep(dados_indic2$ANOOBITO, 2),
  valor = c(dados_indic2$indic2, dados_indic3$indic3),
  Indicador = c(rep("Indicador 2", nrow(dados_indic2)), rep("Indicador 3", nrow(dados_indic3)))
)

# Criando o gráfico com highcharter
highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(2)) %>%
  hc_title(text = "Indicador 2 e Indicador 3 de 2013 a 2022") %>%
  hc_xAxis(categories = unique(dados_indic2_3$Ano), title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100"), min = 0, max = 4, tickInterval = 0.5) %>%
  hc_add_series(
    data = dados_indic2_3 %>% filter(Indicador == "Indicador 2") %>% pull(valor),
    name = "Indicador 2",
    marker = list(symbol = "triangle-down")
  ) %>%
  hc_add_series(
    data = dados_indic2_3 %>% filter(Indicador == "Indicador 3") %>% pull(valor),
    name = "Indicador 3",
    marker = list(symbol = "triangle-down")
  ) %>%
  hc_tooltip(pointFormat = "Percentual: {point.y:.2f}%<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 0.5))


# INDICADORES 4 E 5

dados_indic4 <- data_pop %>%
  dplyr::filter(codigoMunicipio == 3205309) %>%
  select(codigoMunicipio, ano, valor)

#dados_indic4$ano <- as.numeric(dados_indic4$ano)

dados_indic4 <- dados_indic4 %>%  inner_join(
  data.frame(
    dados_vix_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),
  
  by = c("ano" = "ANOOBITO")
)


dados_indic5 <- data_pop %>%
  dplyr::filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor)

#dados_indic5$ano <- as.numeric(dados_indic5$ano)

dados_indic5 <- dados_indic5 %>%  inner_join(
  data.frame(
    dados_es_psic %>% group_by(ANOOBITO) %>%
      summarise(N.obitos = n())
  ),
  
  by = c("ano" = "ANOOBITO")
)


dados_indic4_5 <- data.frame(
  Ano = rep(dados_indic4$ano, 2),
  valor = c(dados_indic4$valor,
            dados_indic5$valor),
  N.obitos = c(dados_indic4$N.obitos,
               dados_indic5$N.obitos),
  Indicador = c(rep("Indicador 4", 10),
                rep("Indicador 5", 10)))

dados_indic4_5 <- dados_indic4_5 |> mutate(Indic = (N.obitos/valor)*100000)


highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(2)) %>%
  hc_title(text = "Indicadores 4 e 5 de 2013 a 2022") %>%
  hc_xAxis(categories = unique(dados_indic4_5$Ano), title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Óbitos/100000"), min = 0, max = 30, tickInterval = 0.5) %>%
  hc_add_series(
    data = dados_indic4_5 %>% filter(Indicador == "Indicador 4") %>% pull(Indic),
    name = "Indicador 4",
    marker = list(symbol = "triangle-down")
  ) %>%
  hc_add_series(
    data = dados_indic4_5 %>% filter(Indicador == "Indicador 5") %>% pull(Indic),
    name = "Indicador 5",
    marker = list(symbol = "triangle-down")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 0.5))
