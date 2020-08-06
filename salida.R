setwd("C:/Users/David Esteban/Google Drive/Materias cursadas/Ecología Forestal II/Salida de Campo")

library(MASS)
require(lattice)
library(RColorBrewer)
#display.brewer.all()

grad=read.csv2("gradiente.csv")
datos<-read.csv2("alturas.csv")
datos$DAP=datos$CAP/pi
datos$TRAN=as.factor(datos$TRAN)
range(datos$DAP)
which(datos$DAP==range(datos$DAP)[2])
datos[67,]
range(datos$H)
which(datos$H==20.8)
datos[27,]
myColours <- brewer.pal(6,"Blues")

my.settings <- list(
  superpose.polygon=list(col="red", border="black"),
  strip.background=list(col="white"),
  strip.border=list(col="black")
)
#### DISTRIBUCIÓN DAP & H

histogram(~DAP|TRAN,datos, xlab = "DAP (cm)",ylab="Número de individuos",relation="same"
          , col="white",type = "count", breaks = c(10,15,20,25,30,35,40))

datos2=subset(datos,TIPO=="Arb")
histogram(~H|TRAN,datos2, xlab = "Altura (m)",ylab="Número de individuos",relation="same", 
          col="white",type = "count",breaks = c(1,4,7,10,13,16,19,22))

##### GRADIENTE ####
require(MASS)
require(lattice)
color=c("chartreuse4","firebrick3","green","blue2","gold","gray35","darkorchid","cyan","black")
grad=read.csv2("gradiente.csv")
grad$TRAN=as.factor(grad$TRAN)

my.settings <- list(
  superpose.line=list(col=color, border="black"),
  strip.border=list(col="black")
)

par(mfrow=c(1,1))
xyplot(ABU~COBER|TRAN, groups=SP,grad,type="l",auto.key=list(space="bottom", columns=4, 
  points=FALSE, lines=TRUE,title="Especies", cex.title=1),col=color,
  par.settings = my.settings, 
  ylab="Abundancia relativa (%)", xlab = "Cobertura herbácea (%)")

xyplot(ABU~MO|TRAN, groups=SP,grad,type="l",auto.key=list(space="bottom", columns=4, 
     points=FALSE, lines=TRUE,title="Especies", cex.title=1),col=color,
       par.settings = list(superpose.line=list(col=color)), 
       ylab="Abundancia relativa (%)", xlab = "Espesor de la materia orgánica (cm)")

xyplot(ABU~PEN|TRAN, groups=SP,grad,type="l",auto.key=list(space="bottom", columns=4, 
   points=FALSE, lines=TRUE,title="Especies", cex.title=1),col=color,
       par.settings = list(superpose.line=list(col=color)), 
       ylab="Abundancia relativa (%)", xlab = "Pendiente (°)")

xyplot(ABU~LUZ|TRAN, groups=SP,grad,type="l",auto.key=list(space="bottom", columns=4, 
     points=FALSE, lines=TRUE,title="Especies", cex.title=1),col=color,
       par.settings = list(superpose.line=list(col=color)), 
       ylab="Abundancia relativa (%)", xlab = "Disponibilidad de luz(%)")


estructura=read.csv2("completo.csv")
estructura$TRAN=as.factor(estructura$TRAN)
estructura$sub=as.factor(estructura$sub)
e50=subset(estructura, sub==50)
e100=subset(estructura , sub==100)
histogram(~DAP|TRAN*sub,estructura, xlab = "DAP (cm)", ylab = "Número de individuos",
          relation="same",col="white",type = "count", breaks = c(1,10,15,20,25,30,35,40))

histogram(~H|TRAN*sub,estructura, xlab = "Altura (m)", ylab = "Número de individuos",
          relation="same",col="white",type = "count",breaks = c(1,4,7,10,13,16,19,22))

