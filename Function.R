#Install packages####
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggplotAssist") #Sugerido
#Library
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggplotAssist)
#Open file####
#68 lineas inservibles 
datos <- read.table(file = "AirSea_ST_0001.asc", skip = 68,header = T)
#Fix the base####
datos <- datos[datos$SSH != -99,c("YYYYMM","SSH")]
datos$YYYYMM <- ymd(datos$YYYYMM,truncated = 2)
datos$Month <- month(datos$YYYYMM)
datos$Year <- year(datos$YYYYMM)
datos <- datos[,c("Year","Month","SSH")]
datos$Month <- factor(datos$Month,
                      labels = c("Ene","Feb","Mar","Abr","May", "Jun", 
                                 "Jul", "Aug", "Sep", "Oct","Nov", "Dic"))
#Means Year and Month####
MY <- datos %>% group_by(Year) %>% summarise(medias = mean(SSH))
MM <- datos %>% group_by(Month) %>% summarise(medias = mean(SSH))

ggplot(data = MY, aes(x=Year, y=medias)) + 
  geom_boxplot(data = datos,aes(x=Year,y=SSH,group=Year), 
               fill = "#aee7e8", outlier.color = "#24009c") +
  scale_y_continuous(name = "Height") + xlab("Year") + 
  labs(tag = "Sea surface height") + 
  theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 20),
        plot.tag.position = "top")  +
  geom_point(color = "#c72c41") + geom_line(color = "#c72c41")


ggplot(data = MM, aes(x=Month, y=medias)) + 
  geom_boxplot(data = datos,aes(x=Month,y=SSH,group=Month), 
               fill = "#aee7e8", outlier.color = "#24009c") + 
  scale_y_continuous(name = "Height") + 
  scale_x_continuous(name = "Month", breaks = 1:12 , labels = 
                       c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep",
                         "Oct","Nov","Dic")) +
  labs(tag = "Sea surface height") + 
  theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 20),
        plot.tag.position = "top")  +
  geom_point(color = "#c72c41")  + geom_line(color = "#c72c41")


