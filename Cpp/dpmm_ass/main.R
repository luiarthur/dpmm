library(rcommon)
library(Rcpp)
Sys.setenv("PKG_CXXFLAGS"="-std=c++11")
sourceCpp("dpmm.cpp")

#N <- 6000 # 70 seconds
set.seed(1)
N <- 100
p <- sort(sample(c(.1,.5,.9),N,replace=TRUE))

m <- 30
y <- rbinom(N, m, p)

system.time(out <- fit(y,m,alpha=1,cs=1,B=2000,burn=10000,printEvery=1000))

plot(p,col='grey',pch=20,ylim=c(0,1),las=1,ylab='Probabilities',main="p")
points(apply(out,1,mean),col='blue',lwd=2,cex=.4)
add.errbar(t(apply(out,1,quantile,c(.025,.975))), col='blue', lwd=.5)
points(y/m, col='red',pch=20,cex=.5)


