"""
metropolis step with normal proposal
"""
function metropolis(curr::Float64, ll, lp, cs::Float64)

  const cand = rand( Distributions.Normal(curr,cs) )

  if ll(cand) + lp(cand) - ll(curr) - lp(curr) > log(rand())
    new_state = cand
  else
    new_state = curr
  end

  return new_state
end

#function metLogit()
#end
