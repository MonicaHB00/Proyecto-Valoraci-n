###Proyecciones a Futuro 
install.packages("forecast")
install.packages("ggplot2")
install.packages("tseries")
library(forecast)
library(ggplot2)
library(tseries)

###Funcion temporal para eliminar la primera y ultima linea 
gold <-gold[-c(1, nrow(gold)),] 
###

serie_temporal <- ts(gold$Future, frequency = 35)
serie_temporal <-as.numeric(serie_temporal)

#se revisa estacionaridad
#adf_test <- adf.test(serie_temporal)
#print(adf_test)


modelo_arima <- auto.arima(serie_temporal)

proyecciones <- forecast(modelo_arima, h = 24)

print(proyecciones)

plot(proyecciones, main = "Proyección de Precios Forward", xlab = "Fecha", ylab = "Precio Forward")
lines(serie_temporal, col = "blue")




modelo_sarima <- Arima(serie_temporal, order = c(1, 1, 1),
                       seasonal = list(order = c(1, 1, 1), period = 12))

print(modelo_sarima)

proyecciones_sarima <- forecast(modelo_sarima, h = 24)

print(proyecciones_sarima)

par(mar = c(5, 4, 4, 2) + 0.1)
plot(proyecciones_sarima, main = "Proyección de Precios Forward con SARIMA", xlab = "Fecha", ylab = "Precio Forward")
lines(serie_temporal, col = "blue")
