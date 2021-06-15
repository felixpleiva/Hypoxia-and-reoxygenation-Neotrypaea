#Limpio espacio de trabajo
rm(list=ls()) 

#----------------------------------------------
#CREADO 20141006 FL+EN
today<-format(Sys.Date(),"%Y%m%d")
#----------------------------------------------
# Directorios y librerias
#----------------------------------------------
setwd ("~/Insync/Compartida FL+KP+PG/Leiva et al CBP/Datos")
getwd() #Verifica directorio
library(plotrix)
#----------------------------------------------

datos1 <-read.table("regulation.csv",header=TRUE, sep=",", dec=".", strip.white=TRUE)
summary(datos1)

#datos1$saturacion2=datos1$saturacion^2
#datos1$saturacion3=datos1$saturacion^3
#lm.2=lm(mo2~saturacion+saturacion2+saturacion3-1,data=datos1[7:36,])
#summary(lm.2)
x=data.frame(saturacion=seq(0,100,0.001))
mm2=nls(mo2~100*saturacion/(km+saturacion),
        data=datos1[7:36,],start=list(km=10))
summary(mm2)
y=c(predict(mm2,newdata=x))
#area bajo la curva
Rvalue<-sum(y)/(length(x$saturacion)*100)
#grafico
jpeg(paste("5.1. Figure 3 Oxygen regulation value (",today,").jpeg"),height = 12,width = 14,units="cm",quality=100,res=300)
par(mfrow=c(1,1),mar=c(4,4.1,1,1))
plot(datos1,pch=21,cex=1.2,lwd=0,bg=c(rep("gray90",6),rep("gray25",30)),
     ylab=expression(paste("Maximum"," ",~italic(M),O[2],~"(%)")),
     las=1,
     xlab="Oxygen saturation (%)",
     ylim=c(0,102), yaxs="i",#deja los ejes en 0,0
    xlim=c(-0.1,101.5), xaxs="i",
    polygon(c(x[,1],100),c(y,0),col="gray90"))
text(55,c(50),c("R=75.27%"),font=2,cex=1)
     dev.off()
