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

#Correlograma (Función de autocorrelación)
acf(Rendimientos$BANCOLOMBIA.PF)
#Valores de las autocorrelaciones
ACFE=acf(Rendimientos$BANCOLOMBIA.PF, plot=FALSE)$acf
(ACFE)

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

#Índices Financieros
install.packages(MASS)
library(MASS)


############################################################################
##########################ÍNDICES FINANCIEROS###############################
############################################################################

#Betas
BCOLCAP=sd(Rendimientos$COLCAP)/sd(Rendimientos$COLCAP)*cor(Rendimientos$COLCAP,Rendimientos$COLCAP)
BBANCOLOMBIA.PF=sd(Rendimientos$BANCOLOMBIA.PF)/sd(Rendimientos$COLCAP)*cor(Rendimientos$BANCOLOMBIA.PF,Rendimientos$COLCAP)
BECOPETROL=sd(Rendimientos$ECOPETROL)/sd(Rendimientos$COLCAP)*cor(Rendimientos$ECOPETROL,Rendimientos$COLCAP)
BISA=sd(Rendimientos$ISA)/sd(Rendimientos$COLCAP)*cor(Rendimientos$ISA,Rendimientos$COLCAP)

Betas=c(BCOLCAP, BBANCOLOMBIA.PF, BECOPETROL, BISA)

Betas = data.frame (rbind(Betas))
colnames(Betas)= c("COLCAP", "BANCOLOMBIA.PF","ECOPETROL", "ISA")
Betas

##Cálculo de los Betas con Regresión Lineal
Regresion1 = lm(BANCOLOMBIA.PF ~ COLCAP, data = Rendimientos)
summary(Regresion1)
Regresion2 = lm(ECOPETROL ~ COLCAP, data = Rendimientos)
summary(Regresion2)
Regresion3 = lm(ISA ~ COLCAP, data = Rendimientos)
summary(Regresion3)

#¿ISA ES UN TÍTULO RIESGOSO?
t=(1.0020828-1)/0.0295390
gl=1865-2
pt(t, gl, lower.tail = F) 

#Revisemos los otros títulos
tB=(1.141669 -1)/0.022905
pt(tB, gl, lower.tail = F) 

tE=(1.371 -1)/0.03376
pt(tE, gl, lower.tail = F) 


#tasa libre de riesgo: tomada de tradingeconomics.com
TLR=0.0695

#Índice de Sharpe
SharpeCOLCAP=(mean(Rendimientos$COLCAP)-TLR)/sd(Rendimientos$COLCAP)
SharpeBANCOLOMBIA.PF=(mean(Rendimientos$BANCOLOMBIA.PF)-TLR)/sd(Rendimientos$BANCOLOMBIA.PF)
SharpeECOPETROL=(mean(Rendimientos$ECOPETROL)-TLR)/sd(Rendimientos$ECOPETROL)
SharpeISA=(mean(Rendimientos$ISA)-TLR)/sd(Rendimientos$ISA)

Sharpe=c(SharpeCOLCAP, SharpeBANCOLOMBIA.PF, SharpeECOPETROL, SharpeISA)

Sharpe = data.frame (rbind(Sharpe))
colnames(Sharpe)= c("COLCAP", "BANCOLOMBIA.PF","ECOPETROL", "ISA")
Sharpe 

#Índice de Treynor
TreynorCOLCAP=(mean(Rendimientos$COLCAP)-TLR)/BCOLCAP
TreynorBANCOLOMBIA.PF=(mean(Rendimientos$BANCOLOMBIA.PF)-TLR)/BBANCOLOMBIA.PF
TreynorECOPETROL=(mean(Rendimientos$ECOPETROL)-TLR)/BECOPETROL
TreynorISA=(mean(Rendimientos$ISA)-TLR)/BISA

Treynor=c(TreynorCOLCAP, TreynorBANCOLOMBIA.PF, TreynorECOPETROL, TreynorISA)

Treynor = data.frame (rbind(Treynor))
colnames(Treynor)= c("COLCAP", "BANCOLOMBIA.PF","ECOPETROL", "ISA")
Treynor

##################################################################
#######################UTILIZANDO FPORTFOLIO######################
################RESTRICCIÓN SOBRE EL RENDIMIENTO##################
##################################################################

##RESTRICCIONES
#POSICIÓN EN LARGO
install.packages("timeDate")
library(timeDate)
install.packages("timeSeries")
library(timeSeries)
install.packages("fBasics")
library(fBasics)
install.packages("fAssets")
library(fAssets)
install.packages("fPortfolio")
library(fPortfolio)

#library(knitr)
#library(kableExtra)

##Eliminar COLCAP
precios1 = select(datos, -Fecha, -COLCAP)
#Leer datos como serie de tiempo
precios1.ts = ts(precios1, start = c(2014, 1, 2), frequency = 252)
(precios1.ts)



##Rendimientos
Rendimientos1=data.frame(diff(log(precios1.ts)))

##Restricciones

markov=portfolioSpec()

#Tasa libre de riesgo
setRiskFreeRate(markov)= 0.0695
#Cantidad de carteras en frontera
setNFrontierPoints(markov) = 20 
#Restricción posición larga
constraints="LongOnly"

Frontera = portfolioFrontier(as.timeSeries(Rendimientos1),spec=markov,constraints )
Frontera

##Frontera efieciente

frontierPlot(Frontera)
grid()
tangencyPoints(Frontera, pch = 19, col = "red", cex=2)
tangencyLines(Frontera, col="grey", pch=19, cex=2)
minvariancePoints(Frontera, col="blue", pch=19, cex=2)
monteCarloPoints(Frontera, mCsteps=2000, col="green", cex=0.001)

##Pesos de los portafolios
col <- qualiPalette(ncol(Rendimientos1), "Pastel1")
weightsPlot(Frontera, col=col)

## Portafolio Eficiente
efPortfolio = efficientPortfolio(as.timeSeries(Rendimientos1),markov,constraints)
efPortfolio

## Portafolio tangente
tgPortfolio = tangencyPortfolio(as.timeSeries(Rendimientos1),markov,constraints)
tgPortfolio

#Participación por portafolio

weightsPie(efPortfolio, col=col )
mtext(text = "Portafolio eficiente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

weightsPie(tgPortfolio, col=col)
mtext(text = "Portafolio tangente", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)


