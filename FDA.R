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
datos <- as.data.frame(datos)
#Data analysis####
Ajuste <- fitDist(y = datos$datos, type = "realplus")

datos$densidad <- eval(parse(text = paste0("d",Ajuste$family[1],"(x = datos$datos, mu = ",
                                           Ajuste$mu,", sigma = ",
                                           Ajuste$sigma,")")))

#Graphic hist and fit of model####
ggplot(data = datos,aes(x = datos)) + 
  geom_histogram(aes(y = ..density..),binwidth=density(datos$datos)$bw, fill = "#70D6FF",
                 color = "#000000") + 
  geom_line(aes(y = densidad), colour = "#FF9770", size = 1.3) + 
  scale_x_continuous(name = "Data") + scale_y_continuous(name = "Density") +
  labs(tag = paste("Density with distribution",Ajuste$family[2])) +
  theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 15),
        plot.tag.position = "top")


#Compartion beetwen data vs distribution####
#eval = Evalua expresiones
#parse = convierte textos en expresiones sin evaluar
#paste0 = concatena textos sin espacios
Quant <- data.frame(Probs=seq(0,1,0.0001),
                    Cuantil_P=quantile(datos$datos,probs = seq(0,1,0.0001)))
Quant$Cuantil_T <- eval(parse(text = paste0("q",Ajuste$family[1],
                                            "(p=seq(0,1,0.0001),mu=",
                                            Ajuste$mu,
                                            ",sigma=",Ajuste$sigma,")")))
#Graphic####
cuantiles <- round(quantile(Quant$Cuantil_P,c(0.01,0.05,0.95,0.99)),2)

ggplot(data = Quant) + 
  geom_line(aes(x = Cuantil_P, y = Probs), color = "#FF9770", size = 1) +
  geom_line(aes(x = Cuantil_T, y = Probs), color = "#70D6FF", size = 1) + 
  scale_x_continuous(name = "X",breaks = seq(0,ceiling(max(datos)),5)) +
  scale_y_continuous(name = "Probability") +
  labs(tag = paste("Data VS",Ajuste$family[2])) +
  theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 15),
        plot.tag.position = "top") +
  scale_fill_manual(values = c("#FF9770","#70D6FF")) +
  geom_line(aes(x = Cuantil_T, y = 0.01), color = "#A7C9A3", size = 1) +
  geom_line(aes(x = Cuantil_T, y = 0.05), color = "#A7C9A3", size = 1) +
  geom_line(aes(x = Cuantil_T, y = 0.95), color = "#A7C9A3", size = 1) +
  geom_line(aes(x = Cuantil_T, y = 0.99), color = "#A7C9A3", size = 1) +
  geom_text(aes(x = cuantiles[1], y = 0.01), label = cuantiles[1]) +
  geom_text(aes(x = cuantiles[2], y = 0.05), label = cuantiles[2]) +
  geom_text(aes(x = cuantiles[3], y = 0.95), label = cuantiles[3]) +
  geom_text(aes(x = cuantiles[4], y = 0.99), label = cuantiles[4]) +
  geom_text(aes(x = 0, y = 0.01), label = "1%") +
  geom_text(aes(x = 0, y = 0.05), label = "5%") +
  geom_text(aes(x = 0, y = 0.95), label = "95%") +
  geom_text(aes(x = 0, y = 0.99), label = "99%")

