###Proyecciones a Futuro 
install.packages("forecast")
install.packages("ggplot2")
install.packages("tseries")
library(forecast)
library(ggplot2)
library(tseries)
library(lubridate)

meses_proyectar<-24

ST_gold_futuro <- ts(gold$Future, frequency = 155)
ST_gold_futuro <-as.numeric(ST_gold_futuro)

ST_gold_spot<-as.numeric(gold$Spot,frecuency=155)

ms_gold_futuro <- Arima(ST_gold_futuro, order = c(1, 1, 1),
                       seasonal = list(order = c(1, 1, 1), period = 24))
ms_gold_spot <- Arima(ST_gold_spot, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 1, 1), period = 24))


proyecciones_futuro_gold<- forecast(ms_gold_futuro, h = meses_proyectar)
proyecciones_spot_gold<- forecast(ms_gold_spot, h = meses_proyectar)


print(proyecciones_futuro_gold)
print(proyecciones_spot_gold)

#par(mar = c(5, 4, 4, 2) + 0.1)
#plot(proyecciones_futuro_gold, main = "Proyección de Precios Forward con SARIMA", xlab = "Fecha", ylab = "Precio Forward")
#lines(ST_gold_futuro, col = "black")
#lines(ST_gold_spot, col = "red")

fechas <- seq(from = ym("2023-11"), by = "months", length.out = meses_proyectar)
fechas_formato <- format(fechas, "%Y-%m")

gold_proyecciones<-data_frame(Date=fechas_formato,
                              Future=proyecciones_futuro_gold$mean,
                              Spot=proyecciones_spot_gold$mean )
gold_completo<-bind_rows(gold,gold_proyecciones)

##

ST_silver_futuro <- ts(silver$Future, frequency = 155)
ST_silver_futuro <-as.numeric(ST_silver_futuro)

ST_silver_spot<-as.numeric(silver$Spot,frecuency=155)

ms_silver_futuro <- Arima(ST_silver_futuro, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 1, 1), period = 24))
ms_silver_spot <- Arima(ST_silver_spot, order = c(1, 1, 1),
                      seasonal = list(order = c(1, 1, 1), period = 24))


proyecciones_futuro_silver<- forecast(ms_silver_futuro, h = meses_proyectar)
proyecciones_spot_silver<- forecast(ms_silver_spot, h = meses_proyectar)


print(proyecciones_futuro_silver)
print(proyecciones_spot_silver)

#par(mar = c(5, 4, 4, 2) + 0.1)
#plot(proyecciones_futuro_gold, main = "Proyección de Precios Forward con SARIMA", xlab = "Fecha", ylab = "Precio Forward")
#lines(ST_gold_futuro, col = "black")
#lines(ST_gold_spot, col = "red")

silver_proyecciones<-data_frame(Date=fechas_formato,
                              Future=proyecciones_futuro_silver$mean,
                              Spot=proyecciones_spot_silver$mean )
silver_completo<-bind_rows(gold,silver_proyecciones)

##########
ST_cooper_futuro <- ts(cooper$Future, frequency = 155)
ST_cooper_futuro <-as.numeric(ST_cooper_futuro)

ST_cooper_spot<-as.numeric(cooper$Spot,frecuency=155)

ms_cooper_futuro <- Arima(ST_cooper_futuro, order = c(1, 1, 1),
                          seasonal = list(order = c(1, 1, 1), period = 24))
ms_cooper_spot <- Arima(ST_cooper_spot, order = c(1, 1, 1),
                        seasonal = list(order = c(1, 1, 1), period = 24))


proyecciones_futuro_cooper<- forecast(ms_cooper_futuro, h = meses_proyectar)
proyecciones_spot_cooper<- forecast(ms_cooper_spot, h = meses_proyectar)


print(proyecciones_futuro_cooper)
print(proyecciones_spot_cooper)

#par(mar = c(5, 4, 4, 2) + 0.1)
#plot(proyecciones_futuro_gold, main = "Proyección de Precios Forward con SARIMA", xlab = "Fecha", ylab = "Precio Forward")
#lines(ST_gold_futuro, col = "black")
#lines(ST_gold_spot, col = "red")

cooper_proyecciones<-data_frame(Date=fechas_formato,
                                Future=proyecciones_futuro_cooper$mean,
                                Spot=proyecciones_spot_cooper$mean )
cooper_completo<-bind_rows(gold,cooper_proyecciones)