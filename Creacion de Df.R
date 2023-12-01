install.packages("dplyr")
library(dplyr)
library(lubridate)
library(readxl)


getwd()
# Para encontrar donde esta guardado este archivo
script_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
script_directory
# Se toma esta dirección como el nuevo working directory
setwd(script_directory)

#Se exportan los datos
Precios_Futuros <- read_excel("Precios Futuros.xlsx")
Gold_Spot <- read_excel("commodities-workbook.xlsx", sheet="Gold", 
                           range = "A11:B678")
Silver_Spot <- read_excel("commodities-workbook.xlsx", sheet="Silver", 
                        range = "A11:B681")
Copper_Spot <- read_excel("commodities-workbook.xlsx", sheet="Copper", 
                        range = "A11:B537")
Platinum_Spot <- read_excel("precios spot platino.xlsx", range = "A1:H181")



#Fecha inicial para los datos
specific_date <- as.Date("2020-12-01")
specific_date_end <- as.Date("2023-10-31")
#Se filtra la tabla de precios spot para platino
Platinum_Spot <- Platinum_Spot[, !(colSums(is.na(Platinum_Spot)) > 0)]
Platinum_Spot <- Platinum_Spot[c('Date', 'Close (troy oz)')] %>% rename('Spot' = 'Close (troy oz)') 
Platinum_Spot$Date <- as.Date(Platinum_Spot$Date)
#Se filtran los datos de los futuros
new_data <- Precios_Futuros[c('commodity', 'date', 'close')]
new_data$date <- as.Date(new_data$date)
new_data <- new_data[new_data$date >= specific_date & new_data$date <= specific_date_end ,]
new_data <- na.omit(new_data %>% rename('Date' = 'date','Future' = 'close'))
#Se pasa el tipo de fecha de POSIXct a Date
Gold_Spot$Date <- as.Date(Gold_Spot$Date)
Silver_Spot$Date <- as.Date(Silver_Spot$Date)
Copper_Spot$Date <- as.Date(Copper_Spot$Date)
#Se filtra por fecha los precios spot a partir del "2022-12-31"
Gold_Spot <- Gold_Spot[Gold_Spot$Date >= specific_date & Gold_Spot$Date <= specific_date_end,] %>% rename('Spot' = 'USD')
Silver_Spot <- Silver_Spot[Silver_Spot$Date >= specific_date & Silver_Spot$Date <= specific_date_end,] %>% rename('Spot' = 'Price')
Copper_Spot <- na.omit(Copper_Spot[Copper_Spot$Date >= specific_date & Copper_Spot$Date <= specific_date_end,]) %>% rename('Spot' = 'Price')
#Se separan los precios futuros por activo
gold_Future <- new_data[new_data$commodity == "Gold", ]
silver_Future <- new_data[new_data$commodity == "Silver", ]
copper_Future <- new_data[new_data$commodity == "Copper", ]
platinum_Future <- new_data[new_data$commodity == "Platinum", ]
# Extraer el año y mes de la fecha
gold_Future_my <- gold_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
silver_Future_my <- silver_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
copper_Future_my <- copper_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
platinum_Future_my <- platinum_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
Gold_Spot_my <- Gold_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Silver_Spot_my <- Silver_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Copper_Spot_my <- Copper_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Platinum_Spot_my <- Platinum_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
# Filtrar la primera aparición de cada mes
gold_Future_per_month <- gold_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
silver_Future_per_month <- silver_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
copper_Future_per_month <- copper_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
platinum_Future_per_month <- platinum_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Gold_Spot_per_month <- Gold_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Silver_Spot_per_month <- Silver_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Copper_Spot_per_month <- Copper_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
#Se unen precios spot y futuros
gold <- merge(gold_Future_per_month, Gold_Spot_per_month, by = "Date", all = TRUE)
silver <- merge(silver_Future_per_month, Silver_Spot_per_month, by = "Date", all = TRUE)
copper <- merge(copper_Future_per_month, Copper_Spot_per_month, by = "Date", all = TRUE)
platinum <- merge(platinum_Future_per_month, Copper_Spot_per_month, by = "Date", all = TRUE)
