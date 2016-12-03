include("DPMM.jl")

using Distributions

N = 30
M = 100
v_truth = sample([.1,.5,.9],N)
x = [ rand(Binomial(M,v[i])) for i in 1:N]

immutable State
  v::Vector{Float64}
end

function update(state::State)

end

gibbs()
