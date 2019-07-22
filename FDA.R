#Install packages####
install.packages("gamlss")
install.packages("lubridate")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggplotAssist") #Sugerido
#Library####
library(gamlss)
library(lubridate)
library(dplyr)
library(ggplot2)
library(ggplotAssist)
#Open file####
datos <- read.table(file = "AirSea_ST_0001.asc", skip = 68,header = T)
datos <- datos[datos$SSH != -99,c("SSH")]
#Data analysis####
Quant <- data.frame(Probs=seq(0,1,0.0001),
                    Cuantil_P=quantile(datos,probs = seq(0,1,0.0001)))

Ajuste <- fitDist(y = datos, type = "realplus")

histDist(datos, family=as.name(Ajuste$family[1]), col.hist = "white", 
         col.main = "Black", line.col = "blue",border.hist = "black",
         main= paste("Family:",Ajuste$family[2]), xlab="Datos",ylab="Densidad",
         col.axis = "black", col.lab = "black")
#eval = Evalua expresiones
#parse = convierte textos en expresiones sin evaluar
#paste0 = concatena textos sin espacios
Quant$Cuantil_P <- eval(parse(text = paste0("q",Ajuste$family[1],
                                            "(p=seq(0,1,0.0001),mu=",
                                            Ajuste$mu,
                                            ",sigma=",Ajuste$sigma,")")))
#Graphic####
d_0.01 <- round(Quant$Cuantil_P[101],2)
d_0.05 <- round(Quant$Cuantil_P[501],2)
d_0.95 <- round(Quant$Cuantil_P[9501],2)
d_0.99 <- round(Quant$Cuantil_P[9901],2)
ggplot(data = Quant) + 
  geom_line(aes(x=Cuantil_P,y=Probs), color = "#FF9770", size=1) +
  geom_line(aes(x=Cuantil_P,y=Probs), color = "#70D6FF", size=1) + 
  scale_x_continuous(name = "X",breaks = seq(0,ceiling(max(datos)),5)) +
  scale_y_continuous(name = "Probability") +
  labs(tag = paste("Data VS",Ajuste$family[2])) +
  theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 15),
        plot.tag.position = "top") +
  scale_fill_manual(values = c("#FF9770","#70D6FF")) +
  geom_text(aes(x=d_0.01,y=0.01),label=d_0.01)+
  geom_text(aes(x=d_0.05,y=0.05),label=d_0.05)+
  geom_text(aes(x=d_0.95,y=0.95),label=d_0.95)+
  geom_text(aes(x=d_0.99,y=0.99),label=d_0.99)
