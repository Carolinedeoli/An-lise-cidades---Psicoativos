dados_indic6 <- inner_join(
  dados_vix_ap %>%
    group_by(ANO) %>%
    summarise(N.apreensoes_vix = n()),
  data_ap %>%
    group_by(ANO) %>%
    summarise(N.apreensoes_es = n()),
  by = "ANO"
)

dados_indic6 <- dados_indic6 %>%
  mutate(indic6 = (N.apreensoes_vix / N.apreensoes_es) * 100)


highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(1)) %>%
  hc_title(text = "Série do Indicador 6 de 2018 a 2022") %>%
  hc_xAxis(categories = dados_indic6$ANO, title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Apreensões/100"),
           min = 0, max = 20, tickInterval =2) %>%
  hc_add_series(data = dados_indic6$indic6, 
                name = "Indicador 6", 
                marker = list(symbol = "triangle-down")) %>%
  hc_tooltip(pointFormat = "Percentual: {point.y:.2f}%<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 0.5))

# INDICADORES 7 E 8

dados_indic7 <- data_pop %>%
  dplyr::filter(codigoMunicipio == 3205309) %>%
  select(codigoMunicipio, ano, valor)

#dados_indic7$ano <- as.numeric(dados_indic7$ano)

dados_indic7 <- dados_indic7 %>%  inner_join(
  data.frame(
    dados_vix_ap %>% group_by(ANO) %>%
      summarise(N.apreensoes = n())
  ),
  
  by = c("ano" = "ANO")
)


dados_indic8 <- data_pop %>%
  dplyr::filter(codigoMunicipio == 32) %>%
  select(codigoMunicipio, ano, valor)

#dados_indic8$ano <- as.numeric(dados_indic8$ano)

dados_indic8 <- dados_indic8 %>%  inner_join(
  data.frame(
    data_ap %>% group_by(ANO) %>%
      summarise(N.apreensoes = n())
  ),
  
  by = c("ano" = "ANO")
)


dados_indic7_8 <- data.frame(
  Ano = rep(dados_indic7$ano, 2),
  valor = c(dados_indic7$valor,
            dados_indic8$valor),
  N.apreensoes = c(dados_indic7$N.apreensoes,
               dados_indic8$N.apreensoes),
  Indicador = c(rep("Indicador 7", 5),
                rep("Indicador 8", 5)))

dados_indic7_8 <- dados_indic7_8 |> mutate(Indic = (N.apreensoes/valor)*100000)


highchart() %>%
  hc_chart(type = "line") %>%
  hc_colors(viridis(2)) %>%
  hc_title(text = "Indicadores 7 e 8 de 2018 a 2022") %>%
  hc_xAxis(categories = unique(dados_indic7_8$Ano), title = list(text = "Anos")) %>%
  hc_yAxis(title = list(text = "Apreensões/100000"), min = 0, max = 1000, tickInterval = 100) %>%
  hc_add_series(
    data = dados_indic7_8 %>% filter(Indicador == "Indicador 7") %>% pull(Indic),
    name = "Indicador 7",
    marker = list(symbol = "triangle-down")
  ) %>%
  hc_add_series(
    data = dados_indic7_8 %>% filter(Indicador == "Indicador 8") %>% pull(Indic),
    name = "Indicador 8",
    marker = list(symbol = "triangle-down")
  ) %>%
  hc_tooltip(pointFormat = "Valor: {point.y:.2f}<br>{series.name}") %>%
  hc_plotOptions(line = list(lineWidth = 0.5))
