function neal8(a::Float64, θ::Vector{Float64},
               lf, lg0, rg0, mh, cs::Float64)

  f(x::Float64,i::Int) = exp(lf(x,i))
  const n = length(θ)
  const oneToN = collect(1:n)
  const newθ = copy(θ)

  # update each element
  for i in 1:n
    const θ₋ᵢ = newθ[1:end .!= i] # all but i-th element
    const mapUT = countmap(θ₋ᵢ)
    const aux = in(newθ[i], θ₋ᵢ) ? rg0() : newθ[i]
    const probExisting = [ut[2] * f(ut[1],i) for ut in mapUT] #10x slower than scala
    const probAux = a * f(aux, i)
    const ut = collect(keys(mapUT)) #10x slower than scala
    newθ[i] = wsample([ut;aux], [probExisting; probAux])
  end

  # update by cluster
  const θ_star = unique(newθ)
  for θⱼ in unique(θ_star)
    const idx = oneToN[newθ .== θⱼ]#find(ti -> ti == θⱼ, newθ)
    ll(t::Float64) = sum([lf(t,i) for i in idx])
    newθ[idx] = mh(θⱼ,ll,lg0,cs)
  end

  return newθ
end
