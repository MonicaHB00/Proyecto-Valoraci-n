install.packages("dplyr")
library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)


getwd()
# Para encontrar donde esta guardado este archivo
script_directory <- dirname(rstudioapi::getSourceEditorContext()$path)
script_directory
# Se toma esta direcciÃ³n como el nuevo working directory
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
risk_free <- read_excel("tasa libre de riesgo.xlsx")


#Fecha inicial para los datos
specific_date <- as.Date("2010-12-01")
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
# Extraer el aÃ±o y mes de la fecha
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
# Filtrar la primera apariciÃ³n de cada mes
gold_Future_per_month <- gold_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
silver_Future_per_month <- silver_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
copper_Future_per_month <- copper_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
platinum_Future_per_month <- platinum_Future_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Gold_Spot_per_month <- Gold_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Silver_Spot_per_month <- Silver_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Copper_Spot_per_month <- Copper_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
Platinum_Spot_per_month <- Platinum_Spot_my %>% group_by(year_month) %>% slice_min(Date) %>% select(-Date) %>% rename('Date' = 'year_month')
#Se unen precios spot y futuros
gold <- merge(gold_Future_per_month, Gold_Spot_per_month, by = "Date", all = TRUE)
silver <- merge(silver_Future_per_month, Silver_Spot_per_month, by = "Date", all = TRUE)
copper <- merge(copper_Future_per_month, Copper_Spot_per_month, by = "Date", all = TRUE)
platinum <- na.omit(merge(platinum_Future_per_month, Platinum_Spot_per_month, by = "Date", all = TRUE))
#Se ponen las cosas como numeritos
gold$Future<-as.numeric(gold$Future)
silver$Future<-as.numeric(silver$Future)
copper$Future<-as.numeric(copper$Future)
platinum$Future<-as.numeric(platinum$Future)
#Todas las fechas se modifican al primero de cada mes
gold$Date <- as.Date(paste0(gold$Date, "-01"))
silver$Date <- as.Date(paste0(silver$Date, "-01"))
copper$Date <- as.Date(paste0(copper$Date, "-01"))
platinum$Date <- as.Date(paste0(platinum$Date, "-01"))

# Crear el gráfico de líneas con ggplot

#Gráfico 1 del oro
ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(title = "Precio de Futuros y Spot del Oro",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "#7FFFD4", "Precio Futuro" = "goldenrod")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(panel.background = element_rect(fill = "black"),  # Modifica el fondo del panel  #8B8B83
        plot.background = element_rect(fill = "black"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "black", color = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "white"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "white"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "white"),  # Modifica el color de las líneas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "white", face = "bold"))+  # Ajustes del título
  guides(color = guide_legend(title = NULL))
  

#Gráfico 2 del oro
ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(title = "Precio de Futuros y Spot del Oro",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "#7FFFD4", "Precio Futuro" = "goldenrod")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(panel.background = element_rect(fill = "black"),  # Modifica el fondo del panel  #8B8B83
        plot.background = element_rect(fill = "black"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "black", color = "goldenrod"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "goldenrod"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "goldenrod"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "goldenrod"),  # Modifica el color de las líneas de los ejes a blanco
        panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "goldenrod", face = "bold"))+  # Ajustes del título
  guides(color = guide_legend(title = NULL))

#Gráfico 3 del oro
ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(title = "Precio de Futuros y Spot del Oro",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "#7FFFD4", "Precio Futuro" = "goldenrod")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(panel.background = element_rect(fill = "black"),  # Modifica el fondo del panel  #8B8B83
        panel.grid.major.x = element_blank(),  # Elimina las líneas verticales principales
        panel.grid.minor.x = element_blank(),  # Elimina las líneas verticales secundarias (si las hubiera)
        plot.background = element_rect(fill = "black"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "black", color = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "white"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "white"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "white"),  # Modifica el color de las líneas de los ejes a blanco
        plot.title = element_text(hjust = 0.5, color = "white", face = "bold"))+  # Ajustes del título
  guides(color = guide_legend(title = NULL))

#Gráfico 4 del oro
ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(title = "Precio de Futuros y Spot del Oro",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "#7FFFD4", "Precio Futuro" = "goldenrod")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(panel.background = element_rect(fill = "black"),  # Modifica el fondo del panel  #8B8B83
        plot.background = element_rect(fill = "black"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "black", color = "goldenrod"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "goldenrod"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "goldenrod"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "goldenrod"),  # Modifica el color de las líneas de los ejes a blanco
        plot.title = element_text(hjust = 0.5, color = "goldenrod", face = "bold"))+  # Ajustes del título
  guides(color = guide_legend(title = NULL))

#Gráfico 5 del oro
ggplot(gold, aes(x = Date)) +
  geom_line(aes(y = Spot, color = "Precio Spot"), linewidth = 1) +
  geom_line(aes(y = Future, color = "Precio Futuro"), linewidth = 1) +
  labs(title = "Precio de Futuros y Spot del Oro",
       x = "Fecha",
       y = "Precio") +
  scale_color_manual(values = c("Precio Spot" = "#7FFFD4", "Precio Futuro" = "goldenrod")) +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year")+  
  scale_y_continuous(breaks = seq(0, max(gold$Future), by = 100)) +
  theme(plot.background = element_rect(fill = "white"),   # Modifica el fondo del plot
        legend.background = element_rect(fill = "white"),  # Modifica el fondo de la leyenda
        legend.text = element_text(color = "black"),  # Modifica el color del texto de la leyenda a blanco
        axis.text = element_text(color = "black"),  # Modifica el color de los valores de los ejes a blanco
        axis.line = element_line(color = "black"),  # Modifica el color de las líneas de los ejes a blanco
        plot.title = element_text(hjust = 0.5, color = "black", face = "bold"))+  # Ajustes del título
  guides(color = guide_legend(title = NULL))


######
library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)

meses_proyectar<-24

#Sereies de Tiempo futuro
ST_gold_futuro <- ts(gold$Future, frequency = 12)
ST_gold_futuro <-as.numeric(ST_gold_futuro)

#series de tiempo spot
ST_gold_spot<-as.numeric(gold$Spot,frecuency=12)

#Arimas
ms_gold_futuro <- Arima(ST_gold_futuro, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 1, 1), period = 12))
ms_gold_spot <- Arima(ST_gold_spot, order = c(1, 1, 1),
                      seasonal = list(order = c(1, 1, 1), period = 12))

#Proyyeciones
proyecciones_futuro_gold<- forecast(ms_gold_futuro, h = meses_proyectar)
proyecciones_spot_gold<- forecast(ms_gold_spot, h = meses_proyectar)


#print(proyecciones_futuro_gold)
#print(proyecciones_spot_gold)

#creacion del nuevo df
fechas <- seq(from = ym("2023-11"), by = "months", length.out = meses_proyectar)
fechas_formato <- format(fechas, "%Y-%m")

gold_proyecciones<-data_frame(Date=fechas_formato,
                              Future=proyecciones_futuro_gold$mean,
                              Spot=proyecciones_spot_gold$mean )
gold_completo<-bind_rows(gold,gold_proyecciones)

#Grafico de las proyecciones de Foward y spot 
#Gold
gold_completo$Date <- as.Date(paste0(gold_completo$Date, "-01"), format = "%Y-%m-%d")
gold_completo$Periodo <- ifelse(gold_completo$Date > as.Date("2023-10-01"), "Proyeccion", "Datos")


ggplot(data = gold_completo) + 
  geom_line(aes(x = Date, y = Spot, group = 1, color = Periodo)) +
  scale_color_manual(values = c("Datos" = "black", "Proyeccion" = "blue")) +
  labs(x = "Fecha", y = "Valor del Spot", title = "Proyeccion de los Precios Spot a dos años", 
       caption = "Fuente: Elaboracion propia con datos de ")


ggplot(data = gold_completo) + 
  geom_line(aes(x = Date, y = Future, group = 1, color = Periodo)) +
  #geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("Datos" = "black", "Proyeccion" = "blue")) +
  labs(x = "Fecha", y = "Valor del Foward", title = "Proyeccion de los Precios Foward a dos años", 
       caption = "Fuente: Elaboracion propia con datos de ")


