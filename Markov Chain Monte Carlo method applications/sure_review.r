
library(quantmod)
library(m537)
library(m537tools)
library(MCMCpack)
library(bayesm)

library(parallel);
library(future.apply);

prmdf = getfinwdat()   
ns = dim(prmdf)[1]
s = 10;
n = (ns - s);
prmdfn = prmdf[1:n,,drop = F]; # this is used for estimation
prmdff = prmdf[(n+1):ns,,drop = F]
dim(prmdff)

capmfrmls = list(prmibm ~ prmsp500-1,
                 prmford ~ prmsp500-1);
datls = suremat(modelfrmls = capmfrmls,datdf = prmdf);

## train prior

priorls = trainpriorsureg(modelfrmls = capmfrmls,
                          data = prmdfn,
                          trainpct = .15)
nt = priorls$nt

thetamcapmsure = MCMCsureg(modelfrmls = capmfrmls,
                           data  = prmdfn);
summarymcmc(thetamcapmsure,header = T);

thetamcapmsuret = MCMCsuret(modelfrmls = capmfrmls,
                            data  = prmdfn,
                            nu = 2.5);
summarymcmc(thetamcapmsuret,header = T)



# Does sure improve

independentls = mapply("MCMCregresst",
                       modelfrm = capmfrmls,
                       MoreArgs = list(data = prmdfn,
                                       nu = 2.5),
                       SIMPLIFY = FALSE);
sum(logmarglik(independentls));


# Prediction: Sampling the future

yfmls = predictsuret(thetam = thetamcapmsuret,
                     pdatdf = prmdff,
                     nu = 2.5);

apply(yfmls[[1]],1,"quantile",prob = c(.025,.5,.975));
apply(yfmls[[2]],1,"quantile",prob = c(.025,.5,.975));
apply(yfmls[[3]],1,"quantile",prob = c(.025,.5,.975));

