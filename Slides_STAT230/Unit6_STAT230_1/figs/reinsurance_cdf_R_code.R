Fx<-function(y){
  ff<-function(x){
  if(x<=0){val=0}
  if((x<10)*(x>0)){ val=1- (10/(10+x))^3}
  if(x>=10){val=1}
    return(val)
  }
  val1=apply(as.matrix(y), MARGIN = 1, FUN = ff)
  
  return(val1)
}

plot(Fx, -1, 9.9999, ylab="f(x)", xlab="Support of X", ylim=c(-.01, 1.01), xlim=c(-1, 13))
par(new=T)
plot(Fx, 10, 13, ylab="f(x)", xlab="Support of X", ylim=c(-.01, 1.01), xlim=c(-1, 13))
points(10,1,pch=19)
