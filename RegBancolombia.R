# Instalar el paquete 'readxls' el paquete más práctico para leer ficheros Excel
install.packages("readr")
# Leer el paquete
library(readxl)
#Permite tranformar bases de datos 
install.packages("dplyr")
library(dplyr)
#Enrutar
setwd("C:/Users/USUARIO/Dropbox/UJAVERIANA CALI/Programacion")

datos=read_excel("Bancolombia-Reg.xlsx")

datos1=select(datos, -Fecha)

#Tranformación de datos sin cambiar los datos existentes
datos2 = mutate(datos1, lnBANCOLOMBIAPF =log(BANCOLOMBIAPF), lnCOLCAP=log(COLCAP), lnPIB=log(PIB))

#Regresión lineal múltiple
Regresion = lm(lnBANCOLOMBIAPF ~ lnCOLCAP + lnPIB + Tasadesempleo + TRM + 
                 Tasadeinteres + Inflacion, data = datos2 )
summary(Regresion)

########################################################################
########################EVALUACIÓN DE SUPUESTOS#########################
########################################################################

###################
#MULTICOLINEALIDAD#
###################
#Gráfico de dispersión múltiple
pairs ( datos1 ) 
pairs(COLCAP ~ PIB + Tasadesempleo + TRM + Tasadeinteres + Inflacion, 
      data = datos2, main="Matriz de Dispersión")
#Matríz de correlación
## libreria para Matriz de correlación Y covarianza
install.packages("MASS")
library(MASS)
cor=cor(datos1)
cor
#Exportar las matrices 
capture.output(cor, file="cor.doc")
capture.output(cor, file="cor.xls")

############################
#NORAMALIDAD EN LOS ERRORES#
############################
library(normtest)
shapiro.test(Regresion$residuals)

#####################
#HETEROSCEDASTICIDAD#
#####################

#Diagrama de Residuales al cuadrado
plot(Regresion$residuals^2 ~ datos2$lnCOLCAP, xlab = "lnCOLCAP", ylab = "Residuales al cuadrado", xlim = c(5, 7.5), ylim = c(0, 0.007), main="Gráfica 01.1b.: Diagrama de Dispersión Residuales al Cuadrado")

install.packages("zoo")
library(zoo)
install.packages("lmtest")
library(lmtest)

#PRUEBA BREUSH-PAGAN-GOODFREY
bptest(Regresion)

# Corrección de hetroscedasticidad: Regresión robusta
install.packages("sandwich")
library(sandwich)
install.packages("stargaze")
library(stargazer)
Regresion_robusta=coeftest(Regresion, vcov = vcovHC(Regresion, type = "HC0"))
stargazer(Regresion_robusta, title = "Modelo Robusto Estimado", type = "text")




