
library(tidyverse)
#Gold
gold_completo$Date <- as.Date(paste0(gold_completo$Date, "-01"), format = "%Y-%m-%d")
gold_completo$Periodo <- ifelse(gold_completo$Date > as.Date("2023-10-01"), "Proyeccion", "Datos")

library(ggplot2)

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

#Silver 
silver_completo$Date <- as.Date(paste0(silver_completo$Date, "-01"), format = "%Y-%m-%d")
silver_completo$periodoF <- ifelse(silver_completo$Date > as.Date("2023-10-01"), "DespuésF", "AntesF")
silver_completo$periodoS <- ifelse(silver_completo$Date > as.Date("2023-10-01"), "DespuésS", "AntesS")

ggplot(data = silver_completo) + 
  #geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("AntesF" = "black", "DespuésF" = "blue", "AntesS" = "red", "DespuésS" = "purple"))

#copper
copper_completo$Date <- as.Date(paste0(copper_completo$Date, "-01"), format = "%Y-%m-%d")
copper_completo$periodoF <- ifelse(copper_completo$Date > as.Date("2023-10-01"), "DespuésF", "AntesF")
copper_completo$periodoS <- ifelse(copper_completo$Date > as.Date("2023-10-01"), "DespuésS", "AntesS")

ggplot(data = copper_completo) + 
  #geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("AntesF" = "black", "DespuésF" = "blue", "AntesS" = "red", "DespuésS" = "purple"))

