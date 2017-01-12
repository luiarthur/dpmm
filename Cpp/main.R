library(rcommon)
library(Rcpp)
sourceCpp("dpmm.cpp")

#N <- 6000 # 70 seconds
set.seed(1)
N <- 100
p <- sort(sample(c(.1,.5,.9),N,replace=TRUE))

m <- rep(100,N)
y <- rbinom(N, m, p)

set.seed(1)
system.time(out <- fit(y,m,alpha=1,cs=1,B=2000,burn=10000,printEvery=1000))

plot(p,col='grey',pch=20,ylim=c(0,1),las=1,ylab='Probabilities',main="p")
points(apply(out,1,mean),col='blue',lwd=2,cex=.4)
add.errbar(t(apply(out,1,quantile,c(.025,.975))), col='blue', lwd=.5)
points(y/m, col='red',pch=20,cex=.5)


####################################
# Test parallel speed and behavior
library(doMC)
registerDoMC(8)
sim <- function(seed=1) {
  set.seed(seed)
  fit(y,m,alpha=1,cs=1,B=2000,burn=10000,printEvery=1000)
}
system.time(out_par <- foreach(i=1:8) %dopar% sim(i))
sapply(out_par, sum) # indeed faster and replicable

####################################
times <- matrix(c( 100,  2.67,  1.21, 
                   200,  6.37,  2.35,
                   400, 12.96,  3.61,
                   600, 20.37,  6.29,
                   800, 28.65,  9.27,
                  1000, 38.83, 11.21,
                  2000, 62.99, 23.05),ncol=3,byrow=TRUE)
colnames(times) <- c("N", "scala", "cpp")

par(mfrow=c(2,1))
plot(times[,1], times[,2], cex=2, type='o', pch=20, col='green', lwd=3, 
     ylim=range(times[,-1]), xlab="N", ylab="Time (seconds)", bty='n',fg='grey',
     main='Time vs N')
lines(times[,1], times[,3], cex=2, type='o', pch=20, col='red', lwd=3)
legend('topleft', legend=c('scala','cpp'), text.col=c('green','red'), text.font=2,
       bty='n')

plot(times[,1], times[,2] / times[,3], type='o', col='grey', pch=20, cex=2, lwd=3,
     ylab="Time (seconds)", xlab="N", main="Scala / Cpp Time", bty='n',fg='grey')
abline(h=1:10, col='grey')
par(mfrow=c(1,1))

times[,2] / times[,3]

