if (!is.loaded("dpmm.so"))
  dyn.load("dpmm.so")

rvonmises2 <- function(n,mu,nu,k1,k2,l){
  phi <- rep(0,n)
  psi <- rep(0,n)
  Cdata <- .C("dpmm",as.integer(n),as.double(mu),as.double(nu),
                          as.double(k1),as.double(k2),as.double(l),
                          phi=as.double(phi),psi=as.double(psi),
                          as.integer(PRINT))

  cbind(Cdata$phi, Cdata$psi)                        
}
