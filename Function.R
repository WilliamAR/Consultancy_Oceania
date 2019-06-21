#Abrir archivos####
#68 lineas inservibles 
install.packages("lubridate")
library(lubridate)
datos <- read.table(file = "AirSea_ST_0001.asc", skip = 68,header = T)
#Arreglando la base####
datos <- datos[datos$SSH != -99,c("YYYYMM","SSH")]
datos$YYYYMM <- ymd(datos$YYYYMM,truncated = 2)
datos$Month <- month(datos$YYYYMM)
datos$Year <- year(datos$YYYYMM)
datos <- datos[,c("Year","Month","SSH")]
boxplot(datos$SSH ~ datos$Year, las = 1, main = "Sea surface height", xlab = "Year",
        ylab = "Height", ylim = c(0,max(datos$SSH)))
Mean_year <- function(Year,SSH){
  Y <- c()
  MA <- c()
  suma <- SSH[1]
  meses <- 1
  k <- 1
  for(i in 2:length(Year)){
    
    if(Year[i-1]==Year[i]){
      suma <- suma + SSH[i]
      meses <- meses + 1
      if(i == length(Year)){
        Y[k] <- Year[i]
        MA[k] <- suma/meses
      }
    }else{
      Y[k] <- Year[i-1]
      MA[k] <- suma/meses
      suma <- SSH[i]
      meses <- 1
      k <- k + 1
    }
  }
  return(data.frame(SSH= MA,Year=Y))
}
MY <- Mean_year(datos$Year,datos$SSH)
lines(MY,col = "red", type = 'b', pch=19)

boxplot(datos$SSH ~ datos$Month, las = 1, main = "Sea surface height", xlab = "Month",
        ylab = "Height", ylim = c(0,max(datos$SSH)))
Mean_month <- function(Month,SSH){
  MM <- rep(NA,times = 12)
  for(i in 1:12){
    MM[i] <- mean(datos[datos$Month==i,"SSH"])
  }
  return(as.data.frame(MM))
}
MM <- Mean_month(datos$Month,datos$SSH)
lines(x,col = "red", type = 'b', pch=19)

install.packages("ggplot2")
install.packages("ggplotAssist")
library(ggplot2)
library(ggplotAssist)
datos <- datos
datos$Month <- factor(datos$Month,
                           labels = c("Ene","Feb","Mar","Abr","May", "Jun", 
                                      "Jul", "Aug", "Sep", "Oct","Nov", "Dic"))

BPY <- ggplot(datos, aes(x=factor(Year),y=SSH)) +
  geom_boxplot(fill = "#aee7e8", outlier.color = "#24009c") +
  scale_y_continuous(name = "Height") + scale_x_discrete(name = "Year") +
  theme_light() + labs(tag = "Sea surface height") +
  theme(plot.tag = element_text(lineheight = 2,face = "bold",size = 20),
        plot.tag.position = "top")  + geom_point(data = MY, color = "#c72c41")+
  geom_line(data = MY,x=as.numeric(Year), color = "#c72c41")
BPY

