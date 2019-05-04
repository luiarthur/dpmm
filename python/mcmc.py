import numpy as np
import math
from collections import Counter

# SOMETHING ISN'T RIGHT HERE. FIXME!!!

def gibbs(init_state, update_fn, B, burn, printFreq):
    out = [init_state] * B
    #
    for i in range(B+burn):
        if i <= burn:
            out[0] = update_fn(out[0])
        else:
            out[i-burn-1] = update_fn(out[i-burn])
        #
        if printFreq > 0 and (i+1)%printFreq == 0:
            print("\rProgress: {}/{}".format(i+1, B+burn))
    #
    if printFreq > 0:
        pass # FIXME
    #
    return out


def metropolis(curr, ll, lp, prop_step_size):
    cand = np.random.normal(curr, prop_step_size)
    u = np.random.rand()
    #
    lg = lambda x: ll(x) + lp(x)
    out = cand if lg(cand) - lg(curr) > math.log(u) else curr
    #
    return out

def metLogit(curr, ll, lp, prop_step_size):
    logit = lambda p: math.log( p/(1-p) )
    inv_logit = lambda x: 1.0 / (1.0 + math.exp(-x))
    #
    def lp_logit(logit_p):
        p = inv_logit(logit_p)
        log_J = -logit_p + 2 * math.log(p)
        return lp(p) + log_J
    #
    def ll_logit(logit_p):
        return ll( inv_logit(logit_p) )
    #
    return inv_logit( metropolis(logit(curr),ll_logit,lp_logit,prop_step_size) )


def wsample_log(x, log_prob): # good
    mx = max(log_prob)
    prob_pre_normalized = map(lambda lp: math.exp(lp - mx), log_prob)
    sum_pp = sum(prob_pre_normalized)
    prob = map(lambda pp: pp / sum_pp,prob_pre_normalized)
    return np.random.choice(x,p=prob)


def algo8(alpha, t_old, cs, lf, lg0, rg0, mh):
    n = len(t_old)
    #
    t_new = t_old[:] # makes a copy of t_old. Yes, very weird syntax.
    map_ut = Counter(t_old)
    #
    # update each element
    for i in range(n):
        map_ut[t_new[i]] -= 1
        if map_ut[t_new[i]] > 0:
            aux = rg0()
        else:
            map_ut.pop(t_new[i])
            aux = t_new[i]
        #
        log_prob = [ math.log(ni) + lf(ui,i) for (ui,ni) in map_ut.iteritems() ]
        log_prob_aux = math.log(alpha) + lf(aux,i)
        log_prob.append(log_prob_aux)
        #
        ut = map_ut.keys()
        ut.append(aux)
        t_new[i] = wsample_log(ut, log_prob)
        #
        if map_ut.has_key(t_new[i]):
            map_ut[t_new[i]] += 1
        else:
            map_ut[t_new[i]] = 1
    #
    # Update by cluster
    ut = set(t_new)
    for tj in ut:
        idx = [i for i in range(n) if t_new[i] == tj]
        ll = lambda t: sum( lf(t,i) for i in idx )
        new_t = mh(tj, ll, lg0, cs)
        for i in idx:
            t_new[i] = new_t
    #
    return t_new






