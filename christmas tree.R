plot(1:10,1:10,xlim=c(-5,5),ylim=c(0,10),type="n",xlab="",ylab="",xaxt="n",yaxt="n")

rect(-1,0,1,2,col="tan3",border="tan4",lwd=3)
polygon(c(-5,0,5),c(2,4,2),col="palegreen3",border="palegreen4",lwd=3)
polygon(c(-4,0,4),c(3.5,5.5,3.5),col="palegreen4",border="palegreen3",lwd=3)
polygon(c(-3,0,3),c(5,6.5,5),col="palegreen3",border="palegreen4",lwd=3)
polygon(c(-2,0,2),c(6.25,7.5,6.25),col="palegreen4",border="palegreen3",lwd=3)

points(x=runif(4,-5,5),y=rep(2,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(x=runif(4,-4,4),y=rep(3.5,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(x=runif(4,-3,3),y=rep(5,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(x=runif(4,-2,2),y=rep(6.25,4),col=sample(c("blue","red"),size=4,replace=T),cex=3,pch=19)
points(0,7.5,pch=8,cex=5,col="gold",lwd=3)

xPres = runif(10,-4.5,4.5)
xWidth = runif(10,0.1,0.5)
xHeight=runif(10,0,1)
for(i in 1:10){
  rect(xPres[i]-xWidth[i],0,xPres[i]+xWidth[i],xHeight[i],col=sample(c("blue","red"),size=1))
  rect(xPres[i]-0.2*xWidth[i],0,xPres[i]+0.2*xWidth[i],xHeight[i],col=sample(c("gold","grey87"),size=1))
}
