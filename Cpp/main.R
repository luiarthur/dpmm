library(Rcpp)
sourceCpp("dpmm.cpp")

N <- 30
p <- sort(sample(c(.1,.5,.9),N,replace=TRUE))

m <- rep(100,N)
y <- rbinom(N, m, p)

sourceCpp("dpmm.cpp")
out <- fit(y,m,alpha=1,cs=1,B=20,burn=0,printEvery=0)

plot(p,col='grey',pch=20,ylim=c(0,1))
points(y/m, col='red',pch=20)
points(apply(out,1,mean),col='blue')
