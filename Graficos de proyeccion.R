
library(tidyverse)
#Gold
gold_completo$Date <- as.Date(paste0(gold_completo$Date, "-01"), format = "%Y-%m-%d")
gold_completo$periodoF <- ifelse(gold_completo$Date > as.Date("2023-10-01"), "DespuésF", "AntesF")
gold_completo$periodoS <- ifelse(gold_completo$Date > as.Date("2023-10-01"), "DespuésS", "AntesS")

ggplot(data = gold_completo) + 
  geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  #geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("AntesF" = "black", "DespuésF" = "blue", "AntesS" = "red", "DespuésS" = "purple"))

#Silver 
silver_completo$Date <- as.Date(paste0(silver_completo$Date, "-01"), format = "%Y-%m-%d")
silver_completo$periodoF <- ifelse(silver_completo$Date > as.Date("2023-10-01"), "DespuésF", "AntesF")
silver_completo$periodoS <- ifelse(silver_completo$Date > as.Date("2023-10-01"), "DespuésS", "AntesS")

ggplot(data = silver_completo) + 
  geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("AntesF" = "black", "DespuésF" = "blue", "AntesS" = "red", "DespuésS" = "purple"))

#copper
copper_completo$Date <- as.Date(paste0(copper_completo$Date, "-01"), format = "%Y-%m-%d")
copper_completo$periodoF <- ifelse(copper_completo$Date > as.Date("2023-10-01"), "DespuésF", "AntesF")
copper_completo$periodoS <- ifelse(copper_completo$Date > as.Date("2023-10-01"), "DespuésS", "AntesS")

ggplot(data = copper_completo) + 
  geom_line(aes(x = Date, y = Future, group = 1, color = periodoF)) +
  geom_line(aes(x = Date, y = Spot, group = 1, color = periodoS)) +
  scale_color_manual(values = c("AntesF" = "black", "DespuésF" = "blue", "AntesS" = "red", "DespuésS" = "purple"))

