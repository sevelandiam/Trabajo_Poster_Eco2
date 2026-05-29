#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#                      Codigo Trabajo #1 Econometria II
#Codigo Elaborado por: Jose David Mayorga Bonilla 
#                      Sebastian Velandia Muñoz 
#                      Juan Nicolás Rodríguez Forero 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Paquetes y librerias:
rm (list = ls())
install.packages("readxl")
install.packages("fs")
install.packages("here")
install.packages("dplyr")
install.packages("ggtime")
install.packages("tidyverse")
install.packages("tsibble")
install.packages("feasts")
install.packages("fable")
install.packages("tseries")
install.packages("FinTS")
install.packages("lmtest")
install.packages("urca")
install.packages("zoo")
install.packages("forecast")
library(fs)
library(here)
library(dplyr)
library(ggtime)
library(tidyverse)
library(tsibble)
library(feasts)
library(fable)
library(tseries)
library(FinTS)
library(lmtest)
library(zoo)
library(urca)
library(forecast)
library(readxl)
#PARA VER LA BASE DE DATOS CON RUTAS RELATIVAS :)
#rutas relativas para correr el codigo en cualquier computador. 
here::i_am("Codigo/Codigo_DeudaExterna.R")
Directorio<-fs::path(here::here("Datos"))
ruta_deuda<-fs::path(Directorio, "Serie de tiempo Deuda Externa.xlsx")
base_serie<-readxl::read_excel(ruta_deuda, sheet = 2)
view(base_serie) #para ver la base de datos xd 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Grafico de la serie.
base_serie$Periodo<-zoo::as.yearmon(as.character(base_serie$Periodo), format = "%Y-%m")
ggplot(base_serie, aes(x=Periodo,  y= Deuda_externa))+
  geom_line(color = "blue2", linewidth = 1)+
  scale_x_yearmon(format = "%Y-%m", n=8)+
  labs(title = "Serie de Tiempo Deuda Externa de Colombia",
       x="Periodo",
       y="Deuda externa")+
  theme_minimal()+
  theme(axis.title = element_text(color = "gray1"),
        plot.title = element_text(color = "gray1"))
Deuda_ts<-ts(base_serie$Deuda_externa,
             start = c(2010, 1),
             frequency = 12)
#Dickey Fuller para la serie.
adf.test(Deuda_ts)
#UR CLASE DE LUNA. 
?ur.df
adf_tendencia <- ur.df(Deuda_ts, type = c("trend"), selectlags = "AIC")
summary(adf_tendencia)#hallar el t value= rho/ sd(rho) (ver la significancia de rho y de la tendencia, para saber si esa prueba realizada fue correcta)
acf(Deuda_ts, main="FAC Deuda Externa")
pacf(Deuda_ts, main="FACP Deuda Externa")
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#PRUEBA DE REGRESION PARA VER SI LA TENDENCIA ES SIGNIFICATIVA EN EL MODELO. (VER R2 Y SIGNIFICANCIA)
# Crear tendencia temporal
t <- 1:length(Deuda_ts)
# Regresión con tendencia lineal
modelo_tend <- lm(as.numeric(Deuda_ts) ~ t)
summary(modelo_tend)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#DIFERENCIA DE LA DEUDA AL SER NO ESTACIONARIA 
diffdeuda<-diff(Deuda_ts)
diffdeuda=na.omit(diffdeuda)
view(diffdeuda)
autoplot(diffdeuda,
         title="Primera diferencia de la serie",
         x="Periodo",
         y="Diferencia de la deuda",
         color="darkblue")
adf.test(diffdeuda) #dickey fuller para la diferencia.(Estacionaria)
par(mfrow = c(1, 2))
acf(diffdeuda, main = "FAC - Deuda diferenciada")
pacf(diffdeuda, main = "FACP - Deuda diferenciada")
par(mfrow = c(1, 1))
kpss.test(diffdeuda) #Este test funciona al contrario de Dickey Fuller. (Estacionaria)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Posibles modelos. 
modelo1<-arima(diffdeuda, order = c(1,0,1),include.mean = TRUE)
modelo2 <- arima(diffdeuda, order= c(1,0,2), include.mean = TRUE)
modelo3 <- arima(diffdeuda, order = c(0,0,2), include.mean =  TRUE)
modelo4 <- arima(diffdeuda, order = c(1,0,0), include.mean = TRUE)
modelo5 <- arima(diffdeuda, order = c(0,0,0), include.mean = TRUE)
modelo1
modelo2
modelo3
modelo4
modelo5
#evaluar significancia de los rezagos de los modelos
#PONER DESVIACIONES ESTANDAR EN EL POSTER
tabla_modelos <- data.frame(
  Modelo = c("ARMA(1,1)", "ARMA(1,2)", "ARMA(0,2)", "ARMA(1,0)", "ARMA(0,0)"),
  AIC = c(
    AIC(modelo1),AIC(modelo2), AIC(modelo3), AIC(modelo4), AIC(modelo5)
  ),
  BIC = c( BIC(modelo1),BIC(modelo2),BIC(modelo3),BIC(modelo4),BIC(modelo5)
  )
)
#AIC y BIC para posibles modelos
tabla_modelos 
#Se escoge modelo 3 MA(2) POR AIC. 
#coeficientes del modelo 3 
summary(modelo3)
coeftest(modelo3)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Residuos del modelo para hacer pruebas. 
residuos3 <- residuals(modelo3)
par(mfrow=c(1,2))
acf(residuos3)
pacf(residuos3)
#No autocorrelacion. (El modelo no presenta correlacion en los errores)
Box.test(residuos3,
         lag=6,#Se usa ese 6 por la peridocidad (meses, entonces indica que son cada 6 meses)
         type = "Ljung-Box") #se rechaza la hipotesis nula, no hay autocorrelacion en los errores. 
#Heterocedasticidad. 
rediduos_cuadrado <- residuos3^2 #si hay correlacion en la varianza significa heterocedastico. 
par(mfrow=c(1,2))
acf(rediduos_cuadrado)
pacf(rediduos_cuadrado)
ArchTest(residuos3)#HETEROCEDASTICO (german dijo que dejaramos asi xd)


#QQplot para ver los periodos que afectan los residuos. 
qqnorm(residuos3,
       main = "QQ-plot de los residuos")
qqline(residuos3, col = "red", lwd = 2)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Pronostico para la diferencia.
pronostico_boot <- forecast(
  modelo3,
  h = 10,
  bootstrap = TRUE,
  npaths = 5000
)
summary(pronostico_boot)
autoplot(pronostico_boot)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Pronostico de la diferencia a niveles 
#pasar el pronostico de la diferencia a niveles 
ultimonivel <- as.numeric(tail(Deuda_ts,1))
#pronosticos para la diferencia 
pron_diff <- as.numeric(pronostico_boot$mean)
#pasar el pronostico a diferencia de niveles
pron_niveles <- ultimonivel+cumsum(pron_diff)
tabla_pronostico_nivel <- data.frame(
  Periodo = time(pronostico_boot$mean),
  Pronostico_diferencia = pron_diff,
  Pronostico_nivel = pron_niveles
)

tabla_pronostico_nivel
# Intervalos de la diferencia
lo80_diff <- as.numeric(pronostico_boot$lower[,1])
hi80_diff <- as.numeric(pronostico_boot$upper[,1])

lo95_diff <- as.numeric(pronostico_boot$lower[,2])
hi95_diff <- as.numeric(pronostico_boot$upper[,2])

# Pasar intervalos a niveles
lo95_nivel <- ultimonivel + cumsum(lo95_diff)
hi95_nivel <- ultimonivel + cumsum(hi95_diff)
# Tabla completa
tabla_pronostico_nivel <- data.frame(
  Periodo = time(pronostico_boot$mean),
  Pronostico_diferencia = pron_diff,
  Pronostico_nivel = pron_niveles,
  Lo_95 = lo95_nivel,
  Hi_95 = hi95_nivel
)
tabla_pronostico_nivel
#faltan los intervalos de confianza en la tabla.  
#falta graficar el pronostico de nivel. (MUY IMPORTANTE lol)

# Serie original
serie_original <- data.frame(
  Periodo = as.yearmon(time(Deuda_ts)),
   Deuda = as.numeric(Deuda_ts)
  )
# Pronóstico en niveles
pronostico_df <- data.frame(
  Periodo = as.yearmon(time(pronostico_boot$mean)),
  Pronostico = tabla_pronostico_nivel$Pronostico_nivel,
  Lo_95 = tabla_pronostico_nivel$Lo_95,
  Hi_95 = tabla_pronostico_nivel$Hi_95
)
ggplot() +
  geom_line(aes(as.yearmon(time(Deuda_ts)), as.numeric(Deuda_ts)), color = "gray1") +
  scale_y_continuous(labels = scales::comma) +
  geom_ribbon(
    data = tabla_pronostico_nivel,
    aes(as.yearmon(Periodo), ymin = Lo_95, ymax = Hi_95),
    fill="blue3",
    alpha = 0.25,
  ) +
  geom_line(
    data = tabla_pronostico_nivel,
    aes(as.yearmon(Periodo), Pronostico_nivel),
    color = "red3",
    linewidth = 1
  ) +
  labs(
    title = "Pronóstico de la deuda externa en niveles",
    x = "Periodo",
    y = "Deuda externa"
  ) +
  theme_minimal()

