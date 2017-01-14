import mcmc
import math
import numpy as np
import rpy2.robjects as robj

rplot = robj.r('plot')



N = 30
M = 100
v_truth = np.random.choice([.1,.5,.9], N)
v_truth.sort()
x = np.random.binomial(M,v_truth)

def update(v):
    alpha = 1.0
    cs = 1.0
    lf = lambda t,i: x[i]*math.log(t) + (M-x[i])*math.log(1.-t)
    lg0 = lambda v:  0.0
    rg0 = lambda: np.random.rand()
    return mcmc.algo8(alpha, v, cs, lf, lg0, rg0, mcmc.metLogit)

out = mcmc.gibbs([.5]*N, update, 2000, 10000, 1000)

rplot(robj.FloatVector(v_truth),ylab='',pch=20,col='grey')

