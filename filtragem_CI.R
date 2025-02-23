#Bibliotecas
library(gridExtra)
library(plotly)
library(scales)
library(readxl)
library(ggplot2)
library(readr)
library(plotly)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(RColorBrewer)
library(reactR)
library(htmltools)
library(reactable)
library(stringr)
library(sidrar)
library(janitor)
library(readxl)
library(knitr)
library(kableExtra)
library(tidyverse)
library(latex2exp)
library(geobr)
library(scales)
library(sf)
library(lwgeom)
library(grid)
library(patchwork)
library(ggpubr)
library(highcharter)
library(viridis)
library(purrr)

#LER E SALVAR DADOS TOTAIS BR


dados_br_total <- c()

for (ano in 2013:2022) {
  
  df <- readRDS(paste0("Bases/bases_de_dados/dados_", ano,".rds"))
  dados_br_total <- bind_rows(df,dados_br_total)
  
}

#ADICIONANDO COLUNAS EXTRAS

#criar coluna do ano em que o obito ocorreu
dados_br_total$DTOBITO <- ymd(dados_br_total$DTOBITO)
dados_br_total$ANOOBITO <- year(dados_br_total$DTOBITO)

#criar coluna da idade em anos completos
dados_br_total$DTNASC  <- ymd(dados_br_total$DTNASC)  
dados_br_total$DTOBITO  <- ymd(dados_br_total$DTOBITO)
dados_br_total$IDADE2  <- floor(interval(start  =  dados_br_total$DTNASC , 
                                         end = dados_br_total$DTOBITO) / years(1)) # arrendondado p baixo - anos completos


#codigos UF

codigosUF <- read_table("Bases/var_extras_UF.txt")

codigosUF$codigoUF <- as.character(codigosUF$codigoUF)


# criando a var codigosUF

dados_br_total <- dados_br_total %>% 
  mutate(
    codigoUF = substr(as.character(CODMUNOCOR), 1, 2)
  )

dados_br_total <- inner_join(dados_br_total, codigosUF, by = "codigoUF")

#ler codigos das cids que tem a ver com uso de psicoativos
codigos_psic <- readLines("Bases/codigos_psic.txt")

#Criando variável idade_unificada
# 1. arrumando variável idade
# Função para converter idade codificada em anos
convert_idade_para_anos <- function(IDADE) {
  # Verificar se o valor de IDADE é NA
  if (is.na(IDADE)) {
    return(NA)
  }
  
  # Extrair o primeiro dígito e os dois dígitos seguintes
  unidade <- as.numeric(substr(IDADE, 1, 1))  
  quantidade <- as.numeric(substr(IDADE, 2, 3))
  
  # Se a unidade ou quantidade for NA, retornar NA
  if (is.na(unidade) | is.na(quantidade)) {
    return(NA)
  }
  
  # Conversão para anos com base na unidade
  if (unidade == 1) {  # Minutos
    idade_anos <- 0  # Definir como 0 anos
  } else if (unidade == 2) {  # Horas
    idade_anos <- 0  # Definir como 0 anos
  } else if (unidade == 3) {  # Meses
    idade_anos <- 0  # Converter meses para anos em formato decimal
  } else if (unidade == 4) {  # Anos (de 00 a 99)
    idade_anos <- quantidade
  } else if (unidade == 5) {  # Anos (maior que 100 anos)
    idade_anos <- 100 + quantidade  # Adiciona 100 para representar idades acima de 100 anos
  } else if (unidade == 9) {  # Ignorado
    idade_anos <- NA  # Valor ignorado
  } else {
    idade_anos <- NA  # Tratamento para valores fora do esperado
  }
  
  
  return(idade_anos)
}
# Aplicando a função ao dataframe
dados_br_total$IDADE3 <- sapply(dados_br_total$IDADE, convert_idade_para_anos)

dados_br_total$IDADE3 <- as.integer(dados_br_total$IDADE3)

#2. Unificando 
#UNINDO COLUNAS A PARTIR DA CULUNA IDADE2

dados_br_total <- dados_br_total %>%
  mutate(
    idade_unificada = case_when(
      # Se um valor for negativo, ficar com o outro
      IDADE2 < 0 & IDADE3 >= 0 ~ IDADE3,
      IDADE3 < 0 & IDADE2 >= 0 ~ IDADE2,
      
      # Se um for NA e o outro não, ficar com o valor numérico
      is.na(IDADE2) & !is.na(IDADE3) ~ IDADE3,
      !is.na(IDADE2) & is.na(IDADE3) ~ IDADE2,
      
      # Se forem diferentes e nenhum for negativo ou NA, escolher IDADE2
      IDADE2 != IDADE3 ~ IDADE2,
      
      # Caso contrário, manter IDADE2 (ou poderia usar IDADE3, pois são iguais)
      TRUE ~ IDADE2
    )
  )


#Excluindo colunas auxiliares:
excluir <- c("IDADE", "IDADE2","IDADE3")
dados_br_total <- dados_br_total[,!(names(dados_br_total)%in% excluir)]

## Renomear nome da coluna
dados_br_total <- dados_br_total %>% 
  rename(IDADE2 = idade_unificada,
  )

#alterando raças 
# Transformar "preto" e "parda" em "negro"
dados_br_total <- dados_br_total %>%
  mutate(RACACOR = ifelse(RACACOR %in% c("Preta", "Parda"), "Negra", RACACOR))

dados_es_total <- dados_br_total %>%
  filter(Sigla == "ES")

dados_ci_total <- dados_es_total %>%
  filter(CODMUNOCOR == "320120")

#---------- Filtrar cids na variavel CAUSABAS

### CI PSIC
dados_ci_psic <- dados_es_ci %>%
  filter(str_detect(toupper(CAUSABAS), paste(codigos_psic, collapse = "|")))

### ES PSIC
dados_es_psic <- dados_es_total %>%
  filter(str_detect(toupper(CAUSABAS), paste(codigos_psic, collapse = "|")))


#população estimada dos municípios
data_pop <- read_csv("Carol/Relatório cidades/populacao_municipios_ES.csv")



