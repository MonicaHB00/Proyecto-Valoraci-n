install.packages("dplyr")
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)


getwd()
# Para encontrar donde esta guardado este archivo
script_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
script_directory
# Se toma esta direcci?n como el nuevo working directory
setwd(script_directory)

#Se exportan los datos
Precios_Futuros <- read_excel("Precios_Futuros.xlsx")
Gold_Spot <- read_excel("commodities-workbook.xlsx", sheet="Gold", 
                           range = "A11:B678")
Silver_Spot <- read_excel("commodities-workbook.xlsx", sheet="Silver", 
                        range = "A11:B681")
Copper_Spot <- read_excel("commodities-workbook.xlsx", sheet="Copper", 
                        range = "A11:B537")
Platinum_Spot <- read_excel("precios spot platino.xlsx", range = "A1:H181")
Palladium_Spot <- read_excel("precios spot paladio.xlsx", range = "A1:B47")
palladium_Future <- read_excel("precios futuros paladio.xlsx", range = "A1:B49")
risk_free <- read_excel("tasa libre de riesgo.xlsx")


#Fecha inicial para los datos
specific_date <- as.Date("2010-12-01")
specific_date_end <- as.Date("2023-10-31")
#Se filtra la tabla de precios spot para platino y paladio
Platinum_Spot <- Platinum_Spot[, !(colSums(is.na(Platinum_Spot)) > 0)]
Platinum_Spot <- Platinum_Spot[c('Date', 'Close (troy oz)')] %>% rename('Spot' = 'Close (troy oz)') 
Platinum_Spot$Date <- as.Date(Platinum_Spot$Date)
Palladium_Spot$Date <- as.Date(Palladium_Spot$Date)
Palladium_Spot <- na.omit(Palladium_Spot[Palladium_Spot$Date >= specific_date & Palladium_Spot$Date <= specific_date_end,] %>% rename('Spot' = 'Close (troy oz)'))
#Se filtran los datos de los futuros
new_data <- Precios_Futuros[c('commodity', 'date', 'close')]
new_data$date <- as.Date(new_data$date)
new_data <- new_data[new_data$date >= specific_date & new_data$date <= specific_date_end ,]
new_data <- na.omit(new_data %>% rename('Date' = 'date','Future' = 'close'))
palladium_Future$Fecha <- as.Date(palladium_Future$Fecha, format = "%d.%m.%Y")
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
palladium_Future <- palladium_Future %>% rename('Date' = 'Fecha','Future' = 'Ãšltimo')
# Extraer el a?o y mes de la fecha
gold_Future_my <- gold_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
silver_Future_my <- silver_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
copper_Future_my <- copper_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
platinum_Future_my <- platinum_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))%>% select(-commodity)
palladium_Future_my <- palladium_Future %>%
  mutate(year_month = format(Date, "%Y-%m"))
Gold_Spot_my <- Gold_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Silver_Spot_my <- Silver_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Copper_Spot_my <- Copper_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Platinum_Spot_my <- Platinum_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
Palladium_Spot_my <- Palladium_Spot %>%
  mutate(year_month = format(Date, "%Y-%m"))
# Filtrar la primera aparici?n de cada mes
gold_Future_per_month <- gold_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
silver_Future_per_month <- silver_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
copper_Future_per_month <- copper_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
platinum_Future_per_month <- platinum_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
palladium_Future_per_month <- palladium_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Gold_Spot_per_month <- Gold_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Silver_Spot_per_month <- Silver_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Copper_Spot_per_month <- Copper_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Platinum_Spot_per_month <- Platinum_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Palladium_Spot_per_month <- Palladium_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
#Se unen precios spot y futuros
gold <- merge(gold_Future_per_month, Gold_Spot_per_month, by = "Date", all = TRUE)
silver <- merge(silver_Future_per_month, Silver_Spot_per_month, by = "Date", all = TRUE)
copper <- merge(copper_Future_per_month, Copper_Spot_per_month, by = "Date", all = TRUE)
platinum <- na.omit(merge(platinum_Future_per_month, Platinum_Spot_per_month, by = "Date", all = TRUE))
palladium <- na.omit(merge(palladium_Future_per_month, Palladium_Spot_per_month, by = "Date", all = TRUE))
#Se ponen las cosas como numeritos
gold$Future<-as.numeric(gold$Future)
silver$Future<-as.numeric(silver$Future)
copper$Future<-as.numeric(copper$Future)
platinum$Future<-as.numeric(platinum$Future)
palladium$Future<-as.numeric(palladium$Future)
platinum$Spot<-as.numeric(platinum$Spot)
palladium$Spot<-as.numeric(palladium$Spot)
#Todas las fechas se modifican al primero de cada mes
gold$Date <- as.Date(paste0(gold$Date, "-01"))
silver$Date <- as.Date(paste0(silver$Date, "-01"))
copper$Date <- as.Date(paste0(copper$Date, "-01"))
platinum$Date <- as.Date(paste0(platinum$Date, "-01"))
palladium$Date <- as.Date(paste0(palladium$Date, "-01"))
# Crear el gr?fico de l?neas con ggplot

#Gr?fico Oro
ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(#title = "Precio de Futuros y Spot del oro",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "cadetblue2", "Precio Futuro" = "goldenrod3")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black", size = 14),  # Modifica el color del texto de la leyenda a blanco
        legend.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"), # Ajustes del t?tulo
        axis.title = element_text(size = 16))+  
  guides(color = guide_legend(title = NULL))

#Gr?fico Plata
ggplot(silver, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(#title = "Precio de Futuros y Spot de la plata",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "cadetblue2", "Precio Futuro" = "#8B8989")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(silver$Future), by = 5)) +
   theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black", size = 14),  # Modifica el color del texto de la leyenda a blanco
        legend.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title = element_text(size = 16))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))


#Gr?fico Cobre
ggplot(copper, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(title = "Precio de Futuros y Spot del cobre",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "cadetblue2", "Precio Futuro" = "#D2691E")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(copper$Future), by = 10)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "black"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))


#Gr?fico Platino
ggplot(platinum, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(#title = "Precio de Futuros y Spot del platino",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "cadetblue2", "Precio Futuro" = "lightgrey")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(platinum$Future), by = 20)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black", size = 14),  # Modifica el color del texto de la leyenda a blanco
        legend.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title = element_text(size = 16))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))

#Gr?fico Paladio
ggplot(palladium, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(#title = "Precio de Futuros y Spot del paladio",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "cadetblue2", "Precio Futuro" = "azure3")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(palladium$Future), by = 200)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black", size = 14),  # Modifica el color del texto de la leyenda a blanco
        legend.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title = element_text(size = 16))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))

######
library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)

meses_proyectar<-24
fechas <- seq(from = ym("2023-11"), by = "months", length.out = meses_proyectar)
fechas_formato <- as.Date(format(fechas, "%Y-%m-%d"))




gold_23<-head(gold,155-2)
#Series de Tiempo futuro
ST_gold23_futuro <- ts(gold_23$Future, frequency = 12)
ST_gold23_futuro <-as.numeric(ST_gold23_futuro)

#series de tiempo spot
ST_gold23_spot<-as.numeric(gold_23$Spot,frecuency=12)

#Arimas
ms_gold23_futuro <- Arima(ST_gold23_futuro, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 1, 1), period = 12))
ms_gold23_spot <- Arima(ST_gold23_spot, order = c(1, 1, 1),
                      seasonal = list(order = c(1, 1, 1), period = 12))

#Proyyeciones
proyecciones_futuro_gold23<- forecast(ms_gold23_futuro, h = meses_proyectar+2)
proyecciones_spot_gold23<- forecast(ms_gold23_spot, h = meses_proyectar+2)

###PAra el 2022-

gold_22<-head(gold,155-12-2)
#Sereies de Tiempo futuro
ST_gold22_futuro <- ts(gold_22$Future, frequency = 12)
ST_gold22_futuro <-as.numeric(ST_gold22_futuro)

#series de tiempo spot
ST_gold22_spot<-as.numeric(gold_22$Spot,frecuency=12)

#Arimas
ms_gold22_futuro <- Arima(ST_gold22_futuro, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 1, 1), period = 12))
ms_gold22_spot <- Arima(ST_gold22_spot, order = c(1, 1, 1),
                      seasonal = list(order = c(1, 1, 1), period = 12))

#Proyyeciones
proyecciones_futuro_gold22<- forecast(ms_gold22_futuro, h = 12+1)
proyecciones_spot_gold22<- forecast(ms_gold22_spot, h = 12+1)

fechas_g22<-seq(from = ym("2022-08"), by = "months", length.out = 12+1)
fechas_formatog22 <- as.Date(format(fechas_g22, "%Y-%m-%d"))     
gold_proyecciones22<-data_frame(Date=fechas_formatog22,
                              Future=proyecciones_futuro_gold22$mean,
                              Spot=proyecciones_spot_gold22$mean )
gold_completo22<-bind_rows(gold_22,gold_proyecciones22)
gold_completo22$Date <- as.Date(paste0(gold_completo22$Date, "-01"), format = "%Y-%m-%d")
gold_completo22$Periodo <- ifelse(gold_completo22$Date > as.Date("2022-08-01"), "Proyeccion", "Datos")


#creacion del nuevo df

              
fechas_g<-seq(from = ym("2023-08"), by = "months", length.out = meses_proyectar+2)
fechas_formatog <- as.Date(format(fechas_g, "%Y-%m-%d"))     
gold_proyecciones23<-data_frame(Date=fechas_formatog,
                              Future=proyecciones_futuro_gold23$mean,
                              Spot=proyecciones_spot_gold23$mean )
gold_completo23<-bind_rows(gold_23,gold_proyecciones23)
gold_completo23$Date <- as.Date(paste0(gold_completo23$Date, "-01"), format = "%Y-%m-%d")
gold_completo23$Periodo <- ifelse(gold_completo23$Date > as.Date("2023-08-01"), "Proyeccion", "Datos")

#Grafico de las proyecciones de Foward y spot 
#Gold



ggplot(data = gold_completo) + 
  geom_line(aes(x = Date, y = Spot, group = 1, color = Periodo)) +
  scale_color_manual(values = c("Datos" = "black", "Proyeccion" = "blue")) +
  labs(x = "Fecha", y = "Valor del Spot", title = "Proyeccion de los Precios Spot a dos a?os", 
       caption = "Fuente: Elaboracion propia con datos de ")


ggplot(data = gold_completo) + 
  geom_line(aes(x = Date, y = Future, group = 1, color = Periodo)) +
  #geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("Datos" = "black", "Proyeccion" = "blue")) +
  labs(x = "Fecha", y = "Valor del Foward", title = "Proyeccion de los Precios Foward a dos a?os", 
       caption = "Fuente: Elaboracion propia con datos de ")

#ggplot(data = gold_completo) + 
#  geom_line(aes(x = Date, y = Spot, group = 1, color = Periodo)) +
#  scale_color_manual(values = c("Datos" = "black", "Proyeccion" = "blue")) +
#  labs(x = "Fecha", y = "Valor del Spot", title = "Proyeccion de los Precios Spot a dos a?os", 
#       caption = "Fuente: Elaboracion propia con datos de ")


#ggplot(data = gold_completo) + 
#  geom_line(aes(x = Date, y = Future, group = 1, color = Periodo)) +
#  #geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
#  scale_color_manual(values = c("Datos" = "black", "Proyeccion" = "blue")) +
#  labs(x = "Fecha", y = "Valor del Foward", title = "Proyeccion de los Precios Foward a dos a?os", 
#       caption = "Fuente: Elaboracion propia con datos de ")



##########################Copper
ST_copper_futuro <- ts(copper$Future, frequency = 12)
ms_copper_futuro <- auto.arima(ST_copper_futuro)

ST_copper_spot<-ts(copper$Spot,frequency=12)

ms_copper_spot<-arima(ST_copper_spot,c(0,0,6))
ms_copper_spot <- arima(ST_copper_spot,c(1,0,10))



proyecciones_futuro_copper<- forecast(ms_copper_futuro, h = meses_proyectar)
proyecciones_spot_copper<- forecast(ms_copper_spot, h = meses_proyectar)

#plot(proyecciones_futuro_copper)
#plot(proyecciones_spot_copper)

copper_proyecciones<-data_frame(Date=as.Date(fechas_formato),
                                Future=proyecciones_futuro_copper$mean,
                                Spot=proyecciones_spot_copper$mean )
copper_completo<-bind_rows(copper,copper_proyecciones)

copper_completo$Date <- as.Date(paste0(copper_completo$Date, "-01"), format = "%Y-%m-%d")
copper_completo$periodoF <- ifelse(copper_completo$Date > as.Date("2023-10-01"), "Despu?sF", "AntesF")
copper_completo$periodoS <- ifelse(copper_completo$Date > as.Date("2023-10-01"), "Despu?sS", "AntesS")
#Grafico del futuro 
#ggplot(data = copper_completo) + 
#  geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  #geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  #scale_color_manual(values = c("AntesF" = "black", "Despu?sF" = "blue", "AntesS" = "red", "Despu?sS" = "purple"))


#ggplot(data = copper_completo) + 
  #geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
#  geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
#  scale_color_manual(values = c("AntesF" = "black", "Despu?sF" = "blue", "AntesS" = "red", "Despu?sS" = "purple"))



###########################SILVER
#Proyeccion del 2022
silver_23<-head(silver,155-2)
#Series de Tiempo futuro
ST_silver23_futuro <- ts(silver_23$Future, frequency = 12)
ST_silver23_futuro <-as.numeric(ST_silver23_futuro)

#series de tiempo spot
ST_silver23_spot<-as.numeric(silver_23$Spot,frecuency=12)

#Arimas
ms_silver23_futuro <-arima(ST_silver23_futuro,order=c(10,0,9))
ms_silver23_spot <- arima(ST_silver23_spot, order = c(1, 1, 1))

#Proyyeciones
proyecciones_futuro_silver23<- forecast(ms_silver23_futuro, h = meses_proyectar+2)
proyecciones_spot_silver23<- forecast(ms_silver23_spot, h = meses_proyectar+2)

silver_proyecciones23<-data_frame(Date=fechas_formatog,
                                  Future=proyecciones_futuro_silver23$mean,
                                  Spot=proyecciones_spot_silver23$mean )
silver_completo23<-bind_rows(silver_23,silver_proyecciones23)
silver_completo23$Date <- as.Date(paste0(silver_completo23$Date, "-01"), format = "%Y-%m-%d")
silver_completo23$Periodo <- ifelse(silver_completo23$Date > as.Date("2023-08-01"), "Proyeccion", "Datos")

###PAra el 2022-

silver_22<-head(silver,155-12-2)
#Sereies de Tiempo futuro
ST_silver22_futuro <- ts(silver_22$Future, frequency = 12)
ST_silver22_futuro <-as.numeric(ST_silver22_futuro)

#series de tiempo spot
ST_silver22_spot<-as.numeric(silver_22$Spot,frecuency=12)

#Arimas
ms_silver22_futuro <-arima(ST_silver22_futuro,order=c(10,0,9))
ms_silver22_spot <- arima(ST_silver22_spot, order = c(1, 1, 1))

#Proyyeciones
proyecciones_futuro_silver22<- forecast(ms_silver22_futuro, h = 12+1)
proyecciones_spot_silver22<- forecast(ms_silver22_spot, h = 12+1)

fechas_g22<-seq(from = ym("2022-08"), by = "months", length.out = 12+1)
fechas_formatog22 <- as.Date(format(fechas_g22, "%Y-%m-%d"))     
silver_proyecciones22<-data_frame(Date=fechas_formatog22,
                                  Future=proyecciones_futuro_silver22$mean,
                                  Spot=proyecciones_spot_silver22$mean )
silver_completo22<-bind_rows(silver_22,silver_proyecciones22)
silver_completo22$Date <- as.Date(paste0(gold_completo22$Date, "-01"), format = "%Y-%m-%d")
silver_completo22$Periodo <- ifelse(gold_completo22$Date > as.Date("2022-08-01"), "Proyeccion", "Datos")


#creacion del nuevo df


###GRafico de Silver

#silver_completo$Date <- as.Date(paste0(silver_completo$Date, "-01"), format = "%Y-%m-%d")
#silver_completo$periodoF <- ifelse(silver_completo$Date > as.Date("2023-10-01"), "Despu?sF", "AntesF")
#silver_completo$periodoS <- ifelse(silver_completo$Date > as.Date("2023-10-01"), "Despu?sF", "AntesS")

#grafico de futuro
#ggplot(data = silver_completo) + 
#  geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  #geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
#  scale_color_manual(values = c("AntesF" = "black", "Despu?sF" = "blue", "AntesS" = "red", "Despu?sS" = "purple"))

#Grafico del Spot
#ggplot(data = silver_completo) + 
  #geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
##  geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
#  scale_color_manual(values = c("AntesF" = "black", "Despu?sF" = "blue", "AntesS" = "red", "Despu?sS" = "purple"))

##---------------------------------------##
#C?lculo con fórmula
risk_free$date <- as.Date(risk_free$date)
fecha_deseada <- as.Date("2022-08-01")
r <- as.numeric(risk_free%>% filter(Date == fecha_deseada) %>% select(`risk free`))
t <- (1:13)/12


# Filtra el dataframe para obtener el Spot para la fecha deseada
Gold_S_0 <- as.numeric(gold %>% filter(Date == fecha_deseada) %>% select(Spot))
Silver_S_0 <- as.numeric(silver%>% filter(Date == fecha_deseada) %>% select(Spot))
Platinum_S_0 <- as.numeric(platinum%>% filter(Date == as.Date("2022-12-01")) %>% select(Spot))

FuturosGold <- Gold_S_0*exp(r*t)
FuturosSilver <- Silver_S_0*exp(r*t)
FuturosPlatinum <- Platinum_S_0*exp(r*t)



fechas_futuras <- seq.Date(from = as.Date(fecha_deseada), by = "1 month", length.out = 13)
gold_form <- data.frame(Date = fechas_futuras, Estimated_Future = FuturosGold)

datos_gold <- gold %>% filter(Date >= fecha_deseada & Date <= as.Date("2023-08-01")) %>% select(Date,Future)


gold_proy <- gold_completo22[,1:2] %>% filter(Date >= as.Date("2017-01-01"))
gold_proy2 <-gold_completo22%>% filter(Date >= fecha_deseada)#[gold_completo22$Periodo == "Proyeccion", ]
proy <- gold_proy2[1:2]
colnames(proy)[colnames(proy) == "Future"] <- "Proyectado"
gold_proy_form <- merge(gold_proy, gold_form, by = "Date", all = TRUE)
gold_proy_form <- merge(gold_proy_form, proy, by = "Date", all = TRUE)
gold_proy_form <- merge(gold_proy_form,datos_gold , by = "Date", all = TRUE)


#Gr?fico oro proyectado y 
ggplot(gold_proy_form, aes(x = Date)) +
  geom_line(aes(y = Estimated_Future, color = "Precio Estimado"), linewidth = 1) +
  geom_line(aes(y = Future.x, color = "Datos históricos"), linewidth = 1) +
  geom_line(aes(y = Proyectado, color = "Precio Proyectado"), linewidth = 1) +
  geom_line(aes(y = Future.y, color = "Datos históricos 2"), linewidth = 1) +
  labs(#title = "Precio de futuros del Oro estimado con fórmula y proyectado mediante Arima",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Estimado" = "green", "Precio Proyectado" = "red","Datos históricos 2" = "blue","Datos históricos" = "goldenrod")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "black"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))



######Plata

silver_form <- data.frame(Date = fechas_futuras, Estimated_Future = FuturosSilver)

datos_silver <- silver %>% filter(Date >= fecha_deseada & Date <= as.Date("2023-08-01")) %>% select(Date,Future)


silver_proy <- silver_completo22[,1:2] %>% filter(Date >= as.Date("2017-01-01"))
silver_proy2 <-silver_completo22%>% filter(Date >= fecha_deseada)#[gold_completo22$Periodo == "Proyeccion", ]
proy_s <- silver_proy2[1:2]
colnames(proy_s)[colnames(proy_s) == "Future"] <- "Proyectado"
silver_proy_form <- merge(silver_proy, silver_form, by = "Date", all = TRUE)
silver_proy_form <- merge(silver_proy_form, proy_s, by = "Date", all = TRUE)
silver_proy_form <- merge(silver_proy_form,datos_silver , by = "Date", all = TRUE)


#Gr?fico oro proyectado y 
ggplot(silver_proy_form, aes(x = Date)) +
  geom_line(aes(y = Estimated_Future, color = "Precio Estimado"), linewidth = 1) +
  geom_line(aes(y = Future.x, color = "Datos históricos"), linewidth = 1) +
  geom_line(aes(y = Proyectado, color = "Precio Proyectado"), linewidth = 1) +
  geom_line(aes(y = Future.y, color = "Datos históricos 2"), linewidth = 1) +
  labs(#title = "Precio de futuros del Oro estimado con fórmula y proyectado mediante Arima",
    x = "Fecha",
    y = "Precio") +
  scale_color_manual(values = c("Precio Estimado" = "green", "Precio Proyectado" = "red","Datos históricos 2" = "blue","Datos históricos" = "#8B8989")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(silver$Future), by = 5)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black", size = 14),  # Modifica el color del texto de la leyenda a blanco
        legend.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title = element_text(size = 16))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))

####Platino
platinum_form <- data.frame(Date = fechas_futuras, Estimated_Future = FuturosPlatinum)

datos_platinum <- platinum %>% filter(Date >= fecha_deseada & Date <= as.Date("2023-08-01")) %>% select(Date,Future)


#platinum_proy <- platinum_completo22[,1:2] %>% filter(Date >= as.Date("2017-01-01"))
#platinum_proy2 <-platinum_completo22%>% filter(Date >= fecha_deseada)#[gold_completo22$Periodo == "Proyeccion", ]
#proy_pl <- platinum_proy2[1:2]
#colnames(proy)[colnames(proy_pl) == "Future"] <- "Proyectado"
#platinum_proy_form <- merge(platinum_proy, platinum_form, by = "Date", all = TRUE)
#platinum_proy_form <- merge(platinum_proy_form, proy_pl, by = "Date", all = TRUE)
platinum_proy_form <- merge(platinum_proy_form,datos_platinum , by = "Date", all = TRUE)


ggplot(platinum_proy_form, aes(x = Date)) +
  geom_line(aes(y = Estimated_Future, color = "Precio Estimado"), linewidth = 1) +
  geom_line(aes(y = Future.x, color = "Datos históricos"), linewidth = 1) +
  geom_line(aes(y = Proyectado, color = "Precio Proyectado"), linewidth = 1) +
  geom_line(aes(y = Future.y, color = "Datos históricos 2"), linewidth = 1) +
  labs(#title = "Precio de futuros del Oro estimado con fórmula y proyectado mediante Arima",
    x = "Fecha",
    y = "Precio") +
  scale_color_manual(values = c("Precio Estimado" = "green", "Precio Proyectado" = "red","Datos históricos 2" = "blue","Datos históricos" = "#8B8989")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(platinum$Future), by = 20)) +
  theme(panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black", size = 14),  # Modifica el color del texto de la leyenda a blanco
        legend.title = element_text(color = "black", size = 14),
        axis.text = element_text(color = "black", size = 12),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las l?neas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"),
        axis.title = element_text(size = 16))+  # Ajustes del t?tulo
  guides(color = guide_legend(title = NULL))


####Paladio


###############################Desde agosto

goldP<-head(gold,155-12)
ST_goldP_futuro <- ts(goldP$Future, frequency = 12)
ST_goldP_spot<-ts(goldP$Spot,frequency=12)

adf_test <- adf.test(ST_goldP_futuro)
print(adf_test)
# Define the range for p, d, q
p <- 0:10
d <- 0
q <- 0:10

# Create a data frame with all combinations of p, d, q
pdq <- expand.grid(p = p, d = d, q = q)

# Apply the ARIMA model to each combination
results1 <- apply(pdq, 1, function(x) {
  tryCatch({
    model <- arima(ST_goldP_spot, order = c(x['p'], x['d'], x['q']))
    return(c(x, AIC = AIC(model)))
  }, error = function(e) {
    return(NULL)
  })
})
#  
results <- apply(pdq, 1, function(x) {
  tryCatch({
    model <- arima(ST_goldP_futuro, order = c(x['p'], x['d'], x['q']))
    return(c(x, AIC = AIC(model)))
  }, error = function(e) {
    return(NULL)
  })
})#5,1,2

ms_goldP_futuro<-Arima(ST_goldP_futuro, order = c(1, 1, 1),
                       seasonal = list(order = c(1, 1, 1), period = 12))
ms_goldP_spot <- Arima(ST_goldP_futuro, order = c(1, 1, 1),
                       seasonal = list(order = c(1, 1, 1), period = 12))


proyecciones_futuro_goldP<- forecast(ms_goldP_futuro, h = 12)
proyecciones_spot_goldP<- forecast(ms_goldP_spot, h = 12)


plot(proyecciones_futuro_goldP)
plot(proyecciones_spot_goldP)


fechas <- seq(from = ym("2022-11"), by = "months", length.out = 12)
fechas_formatoP <- as.Date(format(fechas, "%Y-%m-%d"))

goldP_proyecciones<-data_frame(Date=as.Date(fechas_formatoP),
                               Future=proyecciones_futuro_goldP$mean,
                               Spot=proyecciones_spot_goldP$mean )
goldP_completoP<-bind_rows(goldP,goldP_proyecciones)
