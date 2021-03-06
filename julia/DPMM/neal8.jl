function neal8(a::Float64, θ::Vector{Float64},
               lf::Function, lg0::Function, rg0::Function, mh::Function, cs::Float64)

  f(x::Float64,i::Int) = exp(lf(x,i))
  n::Int = length(θ)
  oneToN::Vector{Int} = collect(1:n)
  newθ = copy(θ)

  mapUT = countmap(θ)

  # update each element
  for i in 1:n
    mapUT[newθ[i]] -= 1
    aux = let
      if mapUT[newθ[i]] > 0 
        rg0()
      else
        delete!(mapUT,newθ[i])
        newθ[i]
      end
    end

    prob::Vector{Float64} = [ ut[2] * f(ut[1], i) for ut in mapUT ]
    probAux::Float64 = a * f(aux, i)
    append!(prob, probAux)

    ut::Vector{Float64} = collect(keys(mapUT))
    append!(ut, aux)

    newθ[i] = wsample(ut, prob)

    if haskey(mapUT,newθ[i])
      mapUT[newθ[i]] += 1
    else
      mapUT[newθ[i]] = 1
    end
  end

  # update by cluster
  θ_star = unique(newθ)
  for θⱼ in θ_star
    idx = oneToN[newθ .== θⱼ]#find(ti -> ti == θⱼ, newθ)
    ll(t::Float64) = sum([lf(t,i) for i in idx])
    newθ[idx] .= mh(θⱼ,ll,lg0,cs)
  end

  return newθ
end
