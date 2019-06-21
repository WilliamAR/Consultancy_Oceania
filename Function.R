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
  MA <- c()
  suma <- SSH[1]
  meses <- 1
  k <- 1
  for(i in 2:length(Year)){
    
    if(Year[i-1]==Year[i]){
      suma <- suma + SSH[i]
      meses <- meses + 1
      if(i == length(Year)){
        MA[k] <- suma/meses
      }
    }else{
      MA[k] <- suma/meses
      suma <- SSH[i]
      meses <- 1
      k <- k + 1
    }
  }
  return(MA)
}
x <- Mean_year(datos$Year,datos$SSH)
lines(x,col = "red", type = 'b', pch=19)

boxplot(datos$SSH ~ datos$Month, las = 1, main = "Sea surface height", xlab = "Month",
        ylab = "Height", ylim = c(0,max(datos$SSH)))
Mean_month <- function(Month,SSH){
  MM <- rep(NA,times = 12)
  for(i in 1:12){
    MM[i] <- mean(datos[datos$Month==i,"SSH"])
  }
  return(MM)
}
x <- Mean_month(datos$Month,datos$SSH)
lines(x,col = "red", type = 'b', pch=19)
