# Instalar el paquete 'readxls' el paquete más práctico para leer ficheros Excel
install.packages("readr")
# Leer el paquete
library(readxl)
#Permite tranformar bases de datos 
install.packages("dplyr")
library(dplyr)
#Enrutar
setwd("C:/Users/USUARIO/Dropbox/UJAVERIANA CALI/Programacion")
datos=read_excel("Precios.xlsx")
precios = select(datos, -Fecha)
#Leer datos como serie de tiempo
precios.ts = data.frame(ts(precios, start = c(2014, 1, 2), frequency = 252))
(precios.ts)

##Gráfica de los precios
plot(precios.ts, main=" ", col="deepskyblue", xlab="Fecha")
title(main="Precios de los Activos")

##############BANCOLOMBIA PF#################
#Correlograma (Función de autocorrelación)
acf(precios.ts$BANCOLOMBIA.PF)
#Valores de las autocorrelaciones
ACFB=acf(precios.ts$BANCOLOMBIA.PF, plot=FALSE)$acf
(ACFB)

###############ECOPETROL#################
#Correlograma (Función de autocorrelación)
acf(precios.ts$ECOPETROL)
#Valores de las autocorrelaciones
ACFE=acf(precios.ts$ECOPETROL, plot=FALSE)$acf
(ACFE)

###############ISA#################
#Correlograma (Función de autocorrelación)
acf(precios.ts$ISA)
#Valores de las autocorrelaciones
ACFE=acf(precios.ts$ISA, plot=FALSE)$acf
(ACFE)

###############COLCAP#################
#Correlograma (Función de autocorrelación)
acf(precios.ts$COLCAP)
#Valores de las autocorrelaciones
ACFE=acf(precios.ts$COLCAP, plot=FALSE)$acf
(ACFE)

#Prueba Dickey Fuller Aumentado: elimina la autocorrelación e indica si una 
#serie es estacionaria o no
install.packages("tseries")
library(tseries)
adf.test(precios.ts$BANCOLOMBIA.PF)
adf.test(precios.ts$ECOPETROL)
adf.test(precios.ts$ISA)
adf.test(precios.ts$COLCAP)

##Rendimientos
precios.ts = ts(precios, start = c(2014, 1, 2), frequency = 252)
Rendimientos=diff(log(precios.ts))

##Gráfica de los Rendimientos
plot(Rendimientos, main=" ", col="deepskyblue", xlab="Fecha")
title(main="Rendimientos del Portafolio")

#Convertir en data.frame
Rendimientos=data.frame(diff(log(precios.ts)))

##Prueba Dickey- Fuller de los Rendimientos
adf.test(Rendimientos$BANCOLOMBIA.PF)
adf.test(Rendimientos$ECOPETROL)
adf.test(Rendimientos$ISA)
adf.test(Rendimientos$COLCAP)


##Rendimientos esperados y Volatilidades
Rendesperado=c(mean(Rendimientos$BANCOLOMBIA.PF), mean(Rendimientos$ECOPETROL),
               mean(Rendimientos$ISA), mean(Rendimientos$COLCAP))
                
Volatilidad=c(sd(Rendimientos$BANCOLOMBIA.PF), sd(Rendimientos$ECOPETROL),
              sd(Rendimientos$ISA), sd(Rendimientos$COLCAP))

Resumen = data.frame (rbind(Rendesperado,Volatilidad))
colnames(Resumen)<- c("BANCOLOMBIA.PF","ECOPETROL", "ISA", "COLCAP")
Resumen*100

CV=c(sd(Rendimientos$BANCOLOMBIA.PF)/mean(Rendimientos$BANCOLOMBIA.PF), 
     sd(Rendimientos$ECOPETROL)/mean(Rendimientos$ECOPETROL),
     sd(Rendimientos$ISA)/mean(Rendimientos$ISA),
     sd(Rendimientos$COLCAP)/mean(Rendimientos$COLCAP))

Resumen = data.frame (rbind(CV))
colnames(Resumen)<- c("BANCOLOMBIA.PF","ECOPETROL", "ISA", "COLCAP")
Resumen

#Matriz de Varianza-Covarianza
Cov=cov(Rendimientos)*100
Cov

#Matriz de Correlaciones
corr = cor(Rendimientos)
corr

