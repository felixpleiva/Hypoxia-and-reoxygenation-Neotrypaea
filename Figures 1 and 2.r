#----------------------------------------------
# GUION PARA EL ANALISIS DE DATOS DEL EFECTO DEL
# NIVEL DE OXIGENO EN LA FISIOLOGIA DEL CAMARON 
# FANTASMA NEOTRYPAEA UNCINATA
#----------------------------------------------
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
# Leo y exploro datos
#----------------------------------------------
datos1 <-read.table("finaldata2.csv",header=TRUE, sep=",", dec=".", strip.white=TRUE)
summary(datos1)
datos1$O2_LEVEL.1<-factor(datos1$O2_LEVEL.1, levels=c("R6","R12","Control","H12","H12+R6","H12+R12"))
#-----------------------------------------------
#Creo Matriz de diseño de gráficos (Figure 1)
#-----------------------------------------------
jpeg(paste("4.1. Figure 1 (",today,").jpeg"),height = 18,width = 40,units="cm",quality=100,res=300)
oma(2,2,2,2)
split.screen(c(2,2))
split.screen(rbind(c(0.1,0.78,0.1,0.98),c(0.82,0.95,0.1,0.98)),screen=1)
#1A METABOLIC RATE
screen(5)
par(mar=c(0.5,4.5,3,0))
sd1<-aggregate(MO2~O2_LEVEL,data=datos1,sd)
media1<-aggregate(MO2~O2_LEVEL,data=datos1,mean)
with(media1,plot(O2_LEVEL,MO2,xaxt="n",ylim=c(0,10),xlim=c(-0.1,22.1),
                 pch=21,cex=1.5,bg=c("white",rep("black",4)),
                 las=1,
                 xlab="",#bquote(paste(italic(p),O[2]~"(kPa)",sep="")),
                 ylab=bquote(paste(italic(M),O[2],~"(",mu,"mol ",.(O[2]~h^-1),g^-1,")",sep="")),
                 cex.lab=1.2,cex.axis=1))
with(sd1,plotCI(x=O2_LEVEL,media1$MO2,MO2,add=T,lwd=2,
                pch=21,pt.bg=c("white",rep("black",4)),cex=1.5))
axis(1,at=seq(0,22,0.5),lab=FALSE,tcl=-0.2)
axis(1,at=seq(0,22,1),lab=FALSE,tcl=-0.4)
axis(1,at=seq(0,22,2),lab=seq(0,22,2),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,10,0.5),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,10,1),lab=FALSE,tcl=-0.4)
with(media1,lines(O2_LEVEL,media1$MO2,lty=2,lwd=1))
#lines(c(7.5,21.5),c(6.5,6.5),lend=1)
#lines(c(1.5,4.5),c(4.5,4.5))
text(media1$O2_LEVEL,c(4.1,4.5,6.2,6.7,6.2),c("a","a","b","b","b"),cex=1)
abline(v=8.46,lty=2,)
text(0,c(9.5),c("A)"),font=2,cex=1.2)
screen(6)
par(mar=c(0.5,0,3,0))
sd2<-aggregate(MO2.1~O2_LEVEL.1,data=datos1[1:12,],sd)
media2<-aggregate(MO2.1~O2_LEVEL.1,data=datos1[1:12,],mean)
with(media2,plot(c(1,2),MO2.1,xlim=c(0.5,2.5), #define variables x e y
                 #text(media2$O2_LEVEL.1,c(6,12),c("c","a"))
                 yaxt="n",xaxt="n", #anula ejes x e y
                 ylim=c(0,10), #define limites en y
                 pch=21,cex=1.5,bg="white", # tipo y tamaño de simbolo
                 las=1,
                 xlab="",#xlab=bquote(paste(italic(p),O[2]~"(kPa)",sep="")),
                 ylab=""#bquote(paste(italic(M),O[2]," (",mu,"mol ",.(O[2]~h^-1),g^-1,")",sep=""))
                ))
with(sd2,plotCI(c(1,2),media2$MO2.1,MO2.1,add=T,lwd=2,
                pch=21,pt.bg="white",cex=1.5))

axis(1,at=c(1,2),lab=c(6,12),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,10,0.5),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,10,1),lab=FALSE,tcl=-0.4)
text(media2$O2_LEVEL.1,c(8.2,6.2),c("c","b"),cex=1)

----------------------------------
# Segundo panel (Oxyhemocyanin)
-----------------------------------
split.screen(rbind(c(0.1,0.78,0.1,0.98),c(0.82,0.95,0.1,0.98)),screen=2)
screen(7)
par(mar=c(0.5,4.5,3,0))
sd3<-aggregate(OXY~O2_LEVEL,data=datos1,sd)
media3<-aggregate(OXY~O2_LEVEL,data=datos1,mean)
with(media3,plot(O2_LEVEL,OXY,xaxt="n",ylim=c(0,2.5),xlim=c(-0.1,22.1),
                 pch=21,cex=1.5,bg=c("white",rep("black",4)),
                 las=1,
                 ylab=expression(paste(Oxy-Hc, " ",(mmol~L^-1))),
                 cex.lab=1.2,cex.axis=1))               
                          
with(sd3,plotCI(x=O2_LEVEL,media3$OXY,OXY,add=T,lwd=2,
               pch=21,pt.bg=c("white",rep("black",4)),cex=1.5))
axis(1,at=seq(0,22,0.5),lab=FALSE,tcl=-0.2)
axis(1,at=seq(0,22,1),lab=FALSE,tcl=-0.4)
axis(1,at=seq(0,22,2),lab=seq(0,22,2),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,2.5,0.125),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,2.5,0.25),lab=FALSE,tcl=-0.4)
with(media3,lines(O2_LEVEL,media3$OXY,lty=2,lwd=1))
#lines(c(7.5,21.5),c(6.5,6.5),lend=1)
#lines(c(1.5,4.5),c(4.5,4.5))
text(media3$O2_LEVEL,c(1.9,1.43,1.3,1.4,1.4),c("a","b","b","b","b"),cex=1)
#abline(v=8.1,lty=4)
text(0,c(2.375),c("B)"),font=2,cex=1.2)
screen(8)
par(mar=c(0.5,0,3,0))
sd4<-aggregate(OXY.1~O2_LEVEL.1,data=datos1[1:12,],sd)
media4<-aggregate(OXY.1~O2_LEVEL.1,data=datos1[1:12,],mean)
with(media4,plot(c(1,2),OXY.1,xlim=c(0.5,2.5), #define variables x e y
                 yaxt="n",xaxt="n", #anula ejes x e y
                 ylim=c(0,2.5), #define limites en y
                 pch=21,cex=1.5,bg="white", # tipo y tamaño de simbolo
                 las=1,
                 xlab="",cex.axis=1.2,#bquote(paste(italic(p),O[2]~"(kPa)",sep="")),
                 ylab=bquote(paste(italic(M),O[2],~"(",mu,"mol ",.(O[2]~h^-1),g^-1,")",sep="")),
                 cex.lab=1.2,cex.axis=1))
with(sd4,plotCI(c(1,2),media4$OXY.1,OXY.1,add=T,lwd=2,
                pch=21,pt.bg="white",cex=1.5))

axis(1,at=c(1,2),lab=c(6,12),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,2.5,0.125),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,2.5,0.25),lab=FALSE,tcl=-0.4)
text(media4$O2_LEVEL.1,c(1.34,1.1),c("b","b"),cex=1)


----------------------------------
# Tercer panel (Hemmolymph protein)
----------------------------------
split.screen(rbind(c(0.1,0.78,0.1,0.98),c(0.82,0.95,0.1,0.98)),screen=3)
screen(9)
par(mar=c(4,4.5,0,0))
sd5<-aggregate(PROT~O2_LEVEL,data=datos1,sd)
media5<-aggregate(PROT~O2_LEVEL,data=datos1,mean)
with(media5,plot(O2_LEVEL,PROT,xaxt="n",ylim=c(0,100),xlim=c(-0.1,22.1),
                 pch=21,cex=1.5,bg=c("white",rep("black",4)),
                 las=1,
                 xlab=bquote(paste(italic(p),O[2]~"(kPa)",sep="")),
                 ylab=expression(paste(Hem-Prot, " ",(mg~mL^-1))),
                 cex.lab=1.2,cex.axis=1))
with(sd5,plotCI(x=O2_LEVEL,media5$PROT,PROT,add=T,lwd=2,
                pch=21,pt.bg=c("white",rep("black",4)),cex=1.5))
axis(1,at=seq(0,22,0.5),lab=FALSE,tcl=-0.2,cex.axis=1)
axis(1,at=seq(0,22,1),lab=FALSE,tcl=-0.4)
axis(1,at=seq(0,22,2),lab=seq(0,22,2),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,100,10),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,100,20),lab=FALSE,tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,100,5),lab=FALSE,tcl=-0.2,cex.axis=1)
with(media5,lines(O2_LEVEL,media5$PROT,lty=2,lwd=1))
#lines(c(7.5,21.5),c(6.5,6.5),lend=1)
#lines(c(1.5,4.5),c(4.5,4.5))
text(media5$O2_LEVEL,c(98,82,75,75,75),c("a","ab","b","b","b"),cex=1)
#abline(v=8.1,lty=4)
text(0,c(95),c("C)"),font=2,cex=1.2)
screen(10)
par(mar=c(4,0.1,0,0))
sd6<-aggregate(PROT.1~O2_LEVEL.1,data=datos1[1:12,],sd)
media6<-aggregate(PROT.1~O2_LEVEL.1,data=datos1[1:12,],mean)
with(media6,plot(c(1,2),PROT.1,xlim=c(0.5,2.5), #define variables x e y
                 yaxt="n",xaxt="n", #anula ejes x e y
                 ylim=c(0,100), #define limites en y
                 pch=21,cex=1.5,bg="white", # tipo y tamaño de simbolo
                 las=1,
                 xlab=("Time (h)"),cex.lab=1.2),
                 ylab="")#bquote(paste(italic(M),O[2]," (",mu,"mol ",.(O[2]~h^-1),g^-1,")",sep=""))))
with(sd6,plotCI(c(1,2),media6$PROT.1,PROT.1,add=T,lwd=2,
                pch=21,pt.bg="white",cex=1.5))

axis(1,at=c(1,2),lab=c(6,12),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,100,10),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,100,20),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,100,5),lab=FALSE,tcl=-0.2)
text(media6$O2_LEVEL.1,c(65,65),c("b","b"),cex=1)


----------------------------------
# Cuarto panel (lactate)
----------------------------------
split.screen(rbind(c(0.1,0.78,0.1,0.98),c(0.82,0.95,0.1,0.98)),screen=4)
screen(11)
par(mar=c(4,4.5,0,0))
sd7<-aggregate(LACT~O2_LEVEL,data=datos1,sd)
media7<-aggregate(LACT~O2_LEVEL,data=datos1,mean)
with(media7,plot(O2_LEVEL,LACT,xaxt="n",ylim=c(0,25),xlim=c(-0.1,22.1),
                 pch=21,cex=1.5,bg=c("white",rep("black",4)),
                 las=1,
                 xlab=bquote(paste(italic(p),O[2]~"(kPa)",sep="")),
                 ylab=expression(paste(Hem-Lact, " ",(mmol~L^-1))),
                 cex.lab=1.2,cex.axis=1))
with(sd7,plotCI(x=O2_LEVEL,media7$LACT,LACT,add=T,lwd=2,
                pch=21,pt.bg=c("white",rep("black",4)),cex=1.5))
axis(1,at=seq(0,22,0.5),lab=FALSE,tcl=-0.2)
axis(1,at=seq(0,22,1),lab=FALSE,tcl=-0.4)
axis(1,at=seq(0,22,2),lab=seq(0,22,2),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,25,2.5),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,25,5),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,25,1.25),lab=FALSE,tcl=-0.2)
with(media7,lines(O2_LEVEL,media7$LACT,lty=2,lwd=1))
#lines(c(7.5,21.5),c(6.5,6.5),lend=1)
#lines(c(1.5,4.5),c(4.5,4.5))
text(media7$O2_LEVEL,c(20.5,18,10,8,8),c("a","b","c","c","c"),cex=1)
#abline(v=8.1,lty=4)
text(0,c(23.75),c("D)"),font=2,cex=1.2)
screen(12)
par(mar=c(4,0.1,0,0))
sd8<-aggregate(LACT.1~O2_LEVEL.1,data=datos1[1:12,],sd)
media8<-aggregate(LACT.1~O2_LEVEL.1,data=datos1[1:12,],mean)
with(media8,plot(c(1,2),LACT.1,xlim=c(0.5,2.5), #define variables x e y
                 yaxt="n",xaxt="n", #anula ejes x e y
                 ylim=c(0,25), #define limites en y
                 pch=21,cex=1.5,bg="white", # tipo y tamaño de simbolo
                 las=1,
                 xlab=("Time (h)"),cex.lab=1.2),
                 ylab="")#bquote(paste(italic(M),O[2]," (",mu,"mol ",.(O[2]~h^-1),g^-1,")",sep=""))))
with(sd8,plotCI(c(1,2),media8$LACT.1,LACT.1,add=T,lwd=2,
                pch=21,pt.bg="white",cex=1.5))

axis(1,at=c(1,2),lab=c(6,12),tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,25,2.5),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,25,5),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,25,1.25),lab=FALSE,tcl=-0.2)
text(media8$O2_LEVEL.1,c(10,8),c("c","c"),cex=1)

dev.off()
close.screen(all.screens=TRUE)

#--------------------------------
#Figure 2
#--------------------------------

jpeg(paste("Figure 2 (",today,").jpeg"),height = 18,width = 20,units="cm",quality=100,res=300)
par(mfrow=c(2,2),oma=c(0,0,0,0),mar=c(2,5,1,1))
data.plot<-subset(datos1,!(O2_LEVEL.1 %in% c("R6","R12")))
#METABOLIC RATE
sd9<-aggregate(MO2.1~O2_LEVEL.1,data=data.plot,sd)
media9<-aggregate(MO2.1~O2_LEVEL.1,data=data.plot,mean)
bp1=with(media9,barplot(MO2.1,cex.lab=1.2,ylim=c(0,11),las=1,names=O2_LEVEL.1,
        ylab=bquote(paste(italic(M),O[2],~"(",mu,"mol ",.(O[2]~h^-1),g^-1,")",sep="")),
        cex.lab=1.2,cex.axis=1,
        col=c("white","black","gray40","gray90")))
        plotCI(x=bp1,y=media9$MO2.1,uiw=sd9$MO2.1,
        liw=FALSE,add=TRUE,err="y",pch=NA)

axis(2,at=seq(0,10,0.5),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,10,1),lab=FALSE,tcl=-0.4)
text(bp1,media9$MO2.1+sd9$MO2.1+1,c("a","b","c","a"),cex=1)
text(0.28,c(10.5),c("A)"),font=2,cex=1.2)
box()

#OXYHEMOCYANIN
sd10<-aggregate(OXY.1~O2_LEVEL.1,data=data.plot,sd)
media10<-aggregate(OXY.1~O2_LEVEL.1,data=data.plot,mean)
bp2=with(media10,barplot(OXY.1,cex.lab=1.2,ylim=c(0,2.75),las=1,names=O2_LEVEL.1,
        ylab=expression(paste(Oxy-Hc, " ",(mmol~L^-1))),
        cex.lab=1.2,cex.axis=1,
        col=c("white","black","gray40","gray90")))
        plotCI(x=bp2,y=media10$OXY.1,uiw=sd10$OXY.1,
        liw=FALSE,add=TRUE,err="y",pch=NA)

axis(2,at=seq(0,2.5,0.125),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,2.5,0.25),lab=FALSE,tcl=-0.4)
text(bp2,media10$OXY.1+sd10$OXY.1+0.22,c("a","b","a","a"),cex=1)
text(0.28,c(2.625),c("B)"),font=2,cex=1.2)
box()

#HEMOLYMPH PROTEIN
sd11<-aggregate(PROT.1~O2_LEVEL.1,data=data.plot,sd)
media11<-aggregate(PROT.1~O2_LEVEL.1,data=data.plot,mean)
sd11<-aggregate(PROT.1~O2_LEVEL.1,data=data.plot,sd)
media11<-aggregate(PROT.1~O2_LEVEL.1,data=data.plot,mean)
bp3=with(media11,barplot(PROT.1,ylim=c(0,110),las=1,names=O2_LEVEL.1,cex.lab=1.2,
        ylab=expression(paste(Hem-Prot, " ",(mg~mL^-1))),
        cex.lab=1.2,cex.axis=1,
        col=c("white","black","gray40","gray90")))
        plotCI(x=bp3,y=media11$PROT.1,uiw=sd11$PROT.1,
        liw=FALSE,add=TRUE,err="y",pch=NA)

axis(2,at=seq(0,100,10),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,100,20),lab=FALSE,tcl=-0.4,cex.axis=1)
axis(2,at=seq(0,100,5),lab=FALSE,tcl=-0.2,cex.axis=1)
text(bp3,media11$PROT.1+sd11$PROT.1+10,c("a","b","a","a"),cex=1)
text(0.28,c(105),c("C)"),font=2,cex=1.2)
box()

#HEMOLYMPH LACTATE
sd12<-aggregate(LACT.1~O2_LEVEL.1,data=data.plot,sd)
media12<-aggregate(LACT.1~O2_LEVEL.1,data=data.plot,mean)
sd12<-aggregate(LACT.1~O2_LEVEL.1,data=data.plot,sd)
media12<-aggregate(LACT.1~O2_LEVEL.1,data=data.plot,mean)
bp4=with(media12,barplot(LACT.1,cex.lab=1.2,ylim=c(0,27.5),las=1,names=O2_LEVEL.1,
        ylab=expression(paste(Hem-Lact, " ",(mmol~L^-1))),
        cex.lab=1.2,cex.axis=1,
        col=c("white","black","gray40","gray90")))
        plotCI(x=bp4,y=media12$LACT.1,uiw=sd12$LACT.1,
        liw=FALSE,add=TRUE,err="y",pch=NA)

axis(2,at=seq(0,25,5),lab=FALSE,tcl=-0.4)
axis(2,at=seq(0,25,1.25),lab=FALSE,tcl=-0.2)
axis(2,at=seq(0,25,2.5),lab=FALSE,tcl=-0.4,cex.axis=1)
text(bp4,media12$LACT.1+sd12$LACT.1+2.8,c("a","b","a","a"),cex=1)
text(0.28,c(26.25),c("D)"),font=2,cex=1.2)
box()

dev.off()

