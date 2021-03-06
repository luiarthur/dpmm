include("DPMM.jl")

using Distributions, RCall
R"library(rcommon)"

N = 100
M = 100
v_truth = sort(sample([.1,.5,.9],N))
x = [ rand(Binomial(M,v_truth[i])) for i in 1:N ] .+ 0.0

function update(v::Vector{Float64})
  alpha = 1.0
  cs = 1.0
  lf(t::Float64,i::Int) = x[i]*log(t) + (M-x[i])*log(1-t)
  lg0(v::Float64) = 0.0
  rg0() = rand()
  DPMM.neal8(alpha,v,lf,lg0,rg0,DPMM.metLogit,cs)
end

@time out = DPMM.gibbs(fill(.5,N),update,2000,10000,printFreq=1000);

v = Matrix(hcat(out...)')

acc = vec(mapslices(vj -> length(unique(vj))/size(v,1), v, dims=1))
clusters = mapslices( v_row -> length(unique(v_row)), v, dims=2 )

R"plot($v_truth,ylim=c(0,1),fg='grey',ylab='probability',main='Clusters',pch=20)"
R"points(apply($v,2,mean),col='blue')"
R"add.errbar(t(apply($v,2,quantile,c(.025,.975))),col='blue')"
R"plotInPlot(function() plot($v[,1],type='l',bty='n',axes=FALSE,col='grey',main='Trace',cex.main=.7),'topleft')"
R"plotInPlot(function()plot($acc,main='Acceptance Rates',type='l',bty='n',col='grey',fg='grey',cex.main=.6),'bottomright')"



#=
include("test.jl")
=#
