# compare with the other gibbs
function gibbs(init, update::Function, B::Int, burn::Int; printFreq::Int=0)
  out = Vector{Any}(undef, B)
  out[1] = init

  for i in 2:(B+burn)
    if i <= burn + 1
      out[1] = update(out[1])
    else
      out[i-burn] = update(out[i-burn-1])
    end

    if printFreq > 0 && i % printFreq == 0
      print("\rProgress: ",i,"/",B+burn)
    end
  end

  return out
end

"""
metropolis step with normal proposal
"""
function metropolis(curr::Float64, ll::Function, lp::Function, cs::Float64)

  cand = rand(Normal(curr,cs))

  if ll(cand) + lp(cand) - ll(curr) - lp(curr) > log(rand())
    new_state = cand
  else
    new_state = curr
  end

  return new_state
end

logit(p::Float64) = log(p / (1.0-p))
invlogit(x::Float64) = 1.0 / (1.0+exp(-x))

function metLogit(curr::Float64, ll::Function, lp::Function, cs::Float64)

  function lp_logit(logit_p::Float64)
    p = invlogit(logit_p)
    #logJ = -logit_p + 2.0*log(p)  #???
    logJ = -logit_p + 2.0*log(1-p) #???
    return lp(p) + logJ
  end
  
  ll_logit(logit_p::Float64) = ll(invlogit(logit_p))

  return invlogit(metropolis(logit(curr),ll_logit,lp_logit,cs))
end


