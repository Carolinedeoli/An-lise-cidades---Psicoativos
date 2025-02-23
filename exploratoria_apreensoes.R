#considerando que rodamos o leitura_filtragem_CIDADES.R" e já temos os dados de apreensão em um rds
#a sigla "ser" adicionada no nome de todos ´s gráficos é referrente a"Serra", nome do municipio usado como base
# é importante trocar o nome do gráfico para a sigla da cidade escolhida para poder gerar o Rmd depois.


#bibliotecas

Sys.setlocale("LC_ALL", "pt_BR.UTF-8")

# Gerar as cores da paleta Inferno
cores_inferno <- viridisLite::viridis(12)


# ---------- numero de apreensões por tipo de droga ------

# Dados agregados de apreensões totais
quant_incidente_total <- dados_ap_ci %>%
  group_by(ANO) %>%
  summarise(QUANTIDADE_TOTAL = n())

# Calcular apreensões por tipo de droga (Maconha, Cocaína e Crack)
quant_incidente_drogas <- dados_ap_ci %>%
  filter(TIPO %in% c("ILICITA: MACONHA", "ILICITA: COCAINA", "ILICITA: CRACK")) %>%
  group_by(ANO, TIPO) %>%
  summarise(QUANTIDADE = n()) %>%
  ungroup()

# Criar o gráfico com bolinhas e a paleta Inferno
serie_drogas_ci <- highchart() %>%
  # Gráfico total de apreensões
  hc_add_series(
    data = quant_incidente_total, 
    type = "line", 
    name = "Total de Apreensões", 
    hcaes(x = ANO, y = QUANTIDADE_TOTAL), 
    color = "black", 
    lineWidth = 1.5, 
    marker = list(symbol = "circle", radius = 4) 
  ) %>%
  # Série para Maconha
  hc_add_series(
    data = quant_incidente_drogas %>% filter(TIPO == "ILICITA: MACONHA"), 
    type = "line", 
    name = "Maconha", 
    hcaes(x = ANO, y = QUANTIDADE), 
    lineWidth = 0.8, 
    marker = list(symbol = "circle", radius = 4),
    color = cores_inferno[3] 
  ) %>%
  # Série para Cocaína
  hc_add_series(
    data = quant_incidente_drogas %>% filter(TIPO == "ILICITA: COCAINA"), 
    type = "line", 
    name = "Cocaína", 
    hcaes(x = ANO, y = QUANTIDADE), 
    lineWidth = 0.8, 
    marker = list(symbol = "circle", radius = 4),
    color = cores_inferno[5] 
  ) %>%
  # Série para Crack
  hc_add_series(
    data = quant_incidente_drogas %>% filter(TIPO == "ILICITA: CRACK"), 
    type = "line", 
    name = "Crack", 
    hcaes(x = ANO, y = QUANTIDADE), 
    lineWidth = 0.8, 
    marker = list(symbol = "circle", radius = 4),
    color = cores_inferno[7] 
  ) %>%
  
  # Títulos e rótulos
  hc_title(text = "Número de Apreensões de Drogas") %>%
  hc_subtitle(text = "Comparação das apreensões por tipo de droga") %>%
  hc_xAxis(
    title = list(text = "Ano"), 
    categories = unique(quant_incidente_total$ANO)
  ) %>%
  hc_yAxis(
    title = list(text = "Quantidade de Apreensões")
  ) %>%
  hc_tooltip(
    shared = TRUE, 
    valueSuffix = " ocorrências"
  ) %>%
  
  # Legenda para as cores
  hc_legend(
    enabled = TRUE
  )%>%
  hc_caption(
    text = "Gráfico 22 - Número de Apreensões de Drogas de 2018 a 2022 em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

serie_drogas_ci

rm(quant_incidente_total, quant_incidente_drogas)

#------- qual o incidente predominante?------

# Preparando os dados
incidente_predominante <- dados_ap_ci %>%
  group_by(`TIPO DE INCIDENTE`) %>%
  summarise(QUANTIDADE = n()) %>%
  arrange(desc(QUANTIDADE)) %>%
  slice_head(n = 10)  # Selecionar os 10 tipos mais frequentes

# Criar o gráfico de barras 
bar_incidentes_ci <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "10 Tipos de Incidentes Mais Frequentes") %>%
  hc_xAxis(
    title = list(text = "Tipo de Incidente"),
    categories = incidente_predominante %>% 
      arrange(desc(QUANTIDADE)) %>%
      pull(`TIPO DE INCIDENTE`)
  ) %>%
  hc_yAxis(title = list(text = "Quantidade")) %>%
  hc_plotOptions(
    series = list(
      borderWidth = 0,
      dataLabels = list(enabled = TRUE)
    )
  ) %>%
  hc_add_series(
    name = "Quantidade",
    data = incidente_predominante %>%
      arrange(desc(QUANTIDADE)) %>%
      pull(QUANTIDADE),
    colorByPoint = TRUE  
  ) %>%
  hc_colors(cores_inferno)%>%
  hc_caption(
    text = "Gráfico 23 - 10 Tipos de Incidentes Mais Frequentes Envolvendo Drogas de 2018 a 2022 em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  )

bar_incidentes_ci

rm(incidente_predominante)

#--------- qual tipo de droga predominante? --------

# Preparar os dados
droga_predominante <- dados_ap_ci %>%
  group_by(`TIPO`) %>%
  summarise(QUANTIDADE = n()) %>%
  arrange(desc(QUANTIDADE))

# Criar o gráfico de barras 
bar_droga_pred_ci <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Quantidade de Apreensões por Droga") %>%
  hc_xAxis(categories = droga_predominante$TIPO, title = list(text = "Tipo de Droga")) %>%
  hc_yAxis(title = list(text = "Quantidade")) %>%
  hc_series(list(
    name = "Quantidade",
    data = lapply(1:nrow(droga_predominante), function(i) {
      list(
        name = droga_predominante$TIPO[i],
        y = droga_predominante$QUANTIDADE[i],
        color = cores_inferno[i]
      )
    })
  )) %>%
  hc_tooltip(pointFormat = "Quantidade: {point.y}") %>%
  hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE)))%>%
  hc_caption(
    text = "Gráfico 24 - Quantidade de Apreensões por Droga de 2018 a 2022 em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 


bar_droga_pred_ci

rm(droga_predominante)

# --------- qual tipo de local predominante? -------

#grafico de BARRAS

# Criando a base de dados
local_predominante <- dados_ap_ci %>%
  group_by(`TIPO DE LOCAL`) %>%
  summarise(QUANTIDADE = n()) %>%
  arrange(desc(QUANTIDADE)) %>%
  slice_head(n = 10)

# Criando o gráfico 
bar_local_prod_ci <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Locais onde mais Acontecem Apreensão de Drogas") %>%
  hc_xAxis(categories = local_predominante$`TIPO DE LOCAL`, title = list(text = "Tipo de Local")) %>%
  hc_yAxis(title = list(text = "Quantidade")) %>%
  hc_series(list(
    name = "Quantidade",
    data = lapply(1:nrow(local_predominante), function(i) {
      list(
        name = local_predominante$`TIPO DE LOCAL`[i],
        y = local_predominante$QUANTIDADE[i],
        color = cores_inferno[i]
      )
    })
  )) %>%
  hc_tooltip(pointFormat = "Quantidade: {point.y}") %>%
  hc_plotOptions(bar = list(dataLabels = list(enabled = TRUE)))%>%
  hc_caption(
    text = "Gráfico 25 - Locais onde Mais Aconteceram Apreensões de Drogas de 2018 a 2022 em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 


bar_local_prod_ci

rm(local_predominante)


#grafico de SÉRIES

# Selecionar apenas os 8 locais mais frequentes
locais_selecionados <- c("VIA PÚBLICA", "RESIDÊNCIA", "OUTRO LOCAL", "COMERCIO",
                         "UNIDADE PRISIONAL", "RODOVIA ESTADUAL", "RODOVIA FEDERAL", "ESCOLA")

# Filtrar e agrupar os dados
local_predominante <- dados_ap_ci %>%
  filter(`TIPO DE LOCAL` %in% locais_selecionados) %>%  
  group_by(`TIPO DE LOCAL`, ANO) %>%  
  summarise(QUANTIDADE = n(), .groups = "drop") %>%
  arrange(ANO)

# Gerar uma paleta de cores com base no número de locais selecionados
num_locais <- length(locais_selecionados)
paleta_cores <- colorRampPalette(cores_inferno)(num_locais) 

# Criar o gráfico de séries temporais no highcharter
series_local_pred_ci <- highchart() %>%
  hc_chart(type = "line") %>%
  hc_title(text = "Locais com Maior Número de Apreensões de Drogas de 2018 a 2022") %>%
  hc_xAxis(title = list(text = "Ano"), categories = unique(local_predominante$ANO)) %>%
  hc_yAxis(title = list(text = "Ocorrências")) %>%
  hc_tooltip(shared = TRUE, valueSuffix = " ocorrências") %>%
  hc_plotOptions(
    line = list(
      marker = list(enabled = TRUE, symbol = "circle")  
    )
  ) %>%
  hc_add_series_list(
    lapply(seq_along(locais_selecionados), function(i) {
      dados_filtrados <- local_predominante %>%
        filter(`TIPO DE LOCAL` == locais_selecionados[i])
      list(
        name = locais_selecionados[i],
        data = dados_filtrados$QUANTIDADE,
        color = paleta_cores[i]  
      )
    })
  )%>%
  hc_caption(
    text = "Gráfico 26 - Locais com Maior Número de Apreensões de Drogas de 2018 a 2022 em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

series_local_pred_ci

rm(locais_selecionados, local_predominante)

#-------- apreensões por bairro -----

# Contar as apreensões por bairro
dados_bairro <- dados_ap_ci %>%
  filter(MUNICÍPIO == "CACHOEIRO DE ITAPEMIRIM") %>%
  count(BAIRRO, name = "num_apreensoes") %>%
  arrange(desc(num_apreensoes)) %>%
  slice_head(n = 15) #seleciona os 15 bairros com maior numero de ocorrencias

# Criar o gráfico 
bairro_pred_ci <- dados_bairro %>%
  hchart(type = "column", hcaes(x = BAIRRO, y = num_apreensoes, color = num_apreensoes)) %>%
  hc_colors(cores_inferno) %>%
  hc_title(text = "15 Bairros da Cidade com Maior Número de Apreensões") %>%
  hc_xAxis(title = list(text = "Bairro"), labels = list(rotation = -45)) %>%
  hc_yAxis(title = list(text = "Número de Apreensões")) %>%
  hc_tooltip(headerFormat = "<b>{point.key}</b><br>", pointFormat = "Apreensões: {point.y}")%>%
  hc_caption(
    text = "Gráfico 27 - 15 Bairros de Cachoeiro de Itapemirim com Maior Número de Apreensões",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

bairro_pred_ci




#------ proporção de drogas por bairro ------

# Agrupar por BAIRRO e TIPO de droga e contar as ocorrências de apreensão
ocorrencias_por_bairro_droga <- dados_ap_ci %>%
  group_by(BAIRRO, TIPO) %>%
  summarise(OCORRENCIAS = n(), .groups = "drop") 

# Calcular a proporção de cada droga dentro de cada bairro
proporcao_ocorrencias_bairro <- ocorrencias_por_bairro_droga %>%
  group_by(BAIRRO) %>%
  mutate(PORCENTAGEM = OCORRENCIAS / sum(OCORRENCIAS)) %>%
  ungroup()

# Selecionar os 5 bairros com maior número total de ocorrências
top5_bairros <- proporcao_ocorrencias_bairro %>%
  group_by(BAIRRO) %>%
  summarise(TOTAL = sum(OCORRENCIAS)) %>%
  slice_max(TOTAL, n = 5) %>%
  arrange(desc(TOTAL))

# Filtrar os dados para manter apenas os 5 bairros selecionados
proporcao_ocorrencias_bairro <- proporcao_ocorrencias_bairro %>%
  filter(BAIRRO %in% top5_bairros$BAIRRO) %>%
  mutate(BAIRRO = factor(BAIRRO, levels = top5_bairros$BAIRRO))

# Criar o gráfico de barras empilhadas 
prop_droga_bairro_ci <- highchart() %>%
  hc_chart(type = "bar") %>%
  hc_title(text = "Porcentagem (%) de Ocorrências de Apreensão por Bairro (Top 5)") %>%
  hc_xAxis(categories = unique(proporcao_ocorrencias_bairro$BAIRRO), title = list(text = "Bairro")) %>%
  hc_yAxis(title = list(text = "Proporção de Ocorrências (%)"), labels = list(format = "{value}%")) %>%
  hc_plotOptions(bar = list(stacking = "percent")) %>%
  hc_tooltip(pointFormat = "<b>{series.name}:</b> {point.y:.1f}%<br/>") %>%
  hc_legend(enabled = TRUE) %>%
  hc_caption(
    text = "Gráfico 28 - Porcentagem (%) de Ocorrências de Apreensão por Bairro de Cachoeiro de Itapemirim (Top 5)",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

# Adicionar as séries (tipos de droga) ao gráfico
tipos_droga <- unique(proporcao_ocorrencias_bairro$TIPO)
for (i in seq_along(tipos_droga)) {
  tipo <- tipos_droga[i]
  dados_tipo <- proporcao_ocorrencias_bairro %>%
    filter(TIPO == tipo) %>%
    arrange(BAIRRO) %>%
    select(BAIRRO, PORCENTAGEM)
  
  prop_droga_bairro_ci <- prop_droga_bairro_ci %>%
    hc_add_series(
      name = tipo,
      data = dados_tipo$PORCENTAGEM * 100,  
      color = cores_inferno[i]  
    )
}

prop_droga_bairro_ci

rm(dados_bairro, ocorrencias_por_bairro_droga, proporcao_ocorrencias_bairro, total_ocorrencias_bairro)

# -------------------------- ANALISES TEMPORAIS ------------------------

#--------- qual hora do dia teve mais ocorrencias? ---------

# Criar a coluna HORA_NUMERICA
dados_ap_ci <- dados_ap_ci %>%
  mutate(HORA_NUMERICA = hour(hms(`HORA DO FATO`)))

# Contar ocorrências por hora
dados_agrupados <- dados_ap_ci %>%
  group_by(HORA_NUMERICA) %>%
  summarise(Quantidade = n()) %>%
  arrange(HORA_NUMERICA)

# Garantir que todas as horas de 0 a 23 estejam presentes
dados_agrupados <- dados_agrupados %>%
  complete(HORA_NUMERICA = 0:23, fill = list(Quantidade = 0))


# Criar o gráfico 
hora_apre_ci <- highchart() %>%
  hc_chart(type = "column") %>%
  hc_title(text = "Número de Ocorrências por Hora do Dia") %>%
  hc_xAxis(categories = as.character(dados_agrupados$HORA_NUMERICA), title = list(text = "Hora do Dia")) %>%
  hc_yAxis(title = list(text = "Número de Ocorrências")) %>%
  hc_add_series(name = "Ocorrências", data = dados_agrupados$Quantidade, colorByPoint = TRUE) %>%
  hc_colors(cores_inferno) %>%
  hc_plotOptions(column = list(dataLabels = list(enabled = TRUE))) %>%
  hc_tooltip(pointFormat = "<b>{point.y} ocorrências</b>")%>%
  hc_caption(
    text = "Gráfico 29 - Número de Ocorrências por Hora do Dia em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

hora_apre_ci



#-----número de apreensões por por mes e por dia da semana ------

# Convertendo a coluna 'DATA DO FATO' para Date
if(!inherits(dados_ap_ci$`DATA DO FATO`, "Date")) {
  dados_ap_ci$`DATA DO FATO` <- as.Date(dados_ap_ci$`DATA DO FATO`, format = "%Y-%m-%d")
}

# Extrair mês e ano
dados_ap_ci <- dados_ap_ci %>%
  mutate(
    Mes = month(`DATA DO FATO`, label = TRUE, abbr = FALSE), 
    Ano = as.factor(year(`DATA DO FATO`)) 
  )

# Contar o número de ocorrências por mês e por ano
ocorrencias_por_mes_ano <- dados_ap_ci %>%
  group_by(Ano, Mes) %>%
  summarise(Quantidade = n(), .groups = 'drop')

# Garantir que todos os meses estejam presentes para cada ano
ocorrencias_por_mes_ano <- ocorrencias_por_mes_ano %>%
  complete(Ano, Mes, fill = list(Quantidade = 0))

# Criar o gráfico de séries temporais 
series_apre_mes_ci <- highchart() %>%
  hc_add_series(ocorrencias_por_mes_ano, type = "line", hcaes(x = Mes, y = Quantidade, group = Ano), marker = list(symbol = "circle")) %>%
  hc_colors(cores_inferno) %>%
  hc_title(text = "Número de Ocorrências por Mês em cada Ano") %>%
  hc_xAxis(title = list(text = "Mês"), categories = unique(ocorrencias_por_mes_ano$Mes)) %>%
  hc_yAxis(title = list(text = "Número de Ocorrências")) %>%
  hc_legend(enabled = TRUE) %>%
  hc_tooltip(shared = TRUE)%>%
  hc_caption(
    text = "Gráfico 30 - Número de Ocorrências por Mês em cada Ano em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

series_apre_mes_ci



# Agregar os dados por Mes, somando as Quantidade de todos os anos
ocorrencias_por_mes <- ocorrencias_por_mes_ano %>%
  group_by(Mes) %>%
  summarise(Total_Ocorrencias = sum(Quantidade, na.rm = TRUE)) %>%
  ungroup()

# Garantir que a coluna 'Mes' está como fator ordenado corretamente
ocorrencias_por_mes$Mes <- factor(ocorrencias_por_mes$Mes, 
                                  levels = c("janeiro", "fevereiro", "março", "abril", 
                                             "maio", "junho", "julho", "agosto", 
                                             "setembro", "outubro", "novembro", "dezembro"))

# Criar o gráfico de barras 
bar_apre_mes_ci <- highchart() %>%
  hc_add_series(ocorrencias_por_mes, type = "column", hcaes(x = Mes, y = Total_Ocorrencias, color = cores_inferno)) %>%
  hc_title(text = "Número Total de Ocorrências por Mês") %>%
  hc_xAxis(title = list(text = "Mês"), categories = unique(ocorrencias_por_mes$Mes)) %>%
  hc_yAxis(title = list(text = "Número de Ocorrências")) %>%
  hc_legend(enabled = FALSE)%>%
  hc_caption(
    text = "Gráfico 31 - Número Total de Ocorrências por Mês em cada Ano em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

bar_apre_mes_ci


# Extrair o dia da semana
dados_ap_ci <- dados_ap_ci %>%
  mutate(
    Dia_Semana = wday(`DATA DO FATO`, label = TRUE, abbr = FALSE, week_start = 7, locale = "pt_BR.UTF-8") # Domingo = 1
  )

# Definir os níveis do fator 'Dia_Semana' em ordem cronológica
dados_ap_ci$Dia_Semana <- factor(dados_ap_ci$Dia_Semana, 
                                  levels = c("domingo", "segunda-feira", "terça-feira", "quarta-feira", 
                                             "quinta-feira", "sexta-feira", "sábado"))

# Agregar os dados por Dia_Semana
ocorrencias_por_dia <- dados_ap_ci %>%
  group_by(Dia_Semana) %>%
  summarise(Quantidade = n(), .groups = 'drop')

# Criar o gráfico de barras 
bar_apre_dia_ci <- highchart() %>%
  hc_add_series(ocorrencias_por_dia, type = "column", hcaes(x = Dia_Semana, y = Quantidade), colorByPoint = TRUE) %>%
  hc_colors(cores_inferno) %>%  
  hc_title(text = "Número Total de Ocorrências por Dia da Semana") %>%
  hc_xAxis(title = list(text = "Dia da Semana"), categories = unique(ocorrencias_por_dia$Dia_Semana)) %>%
  hc_yAxis(title = list(text = "Número de Ocorrências")) %>%
  hc_legend(enabled = FALSE)%>%
  hc_caption(
    text = "Gráfico 32 - Número Total de Ocorrências por Dia da Semana em Cachoeiro de Itapemirim",
    align = "center",
    style = list(fontSize = "10px", fontWeight = "bold")
  ) 

bar_apre_dia_ci

rm(dados_agrupados)

