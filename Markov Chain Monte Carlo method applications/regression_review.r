
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

thetamcapm = MCMCregressg(prmibm~prmsp500-1,
                    data = prmdfn);
summarymcmc(thetamcapm,header = T)

sigm = m537::sigma(thetamcapm);
summarymcmc(sigm);

set.seed(1);
n = 500;
Sigma = matrix(.7,nr = 5,nc = 5);
diag(Sigma) = rep(1,5);
L = t(chol(Sigma))
X = L%*%matrix(rnorm(2500),nr = 5,nc = n);
X = t(X);
colnames(X) = paste("x",1:5,sep = "")
beta = c(.5,.6,0,0,0);
y = as.matrix(1 + X %*% beta + rnorm(n));
colnames(y) = "y"
simdatdf = cbind(y,X)
simdatdf = as.data.frame(simdatdf)

modelfrmls = makemodelformulas(yname = "y",
                               xnames = colnames(X))

outls = future_mapply("MCMCregressg",
                      modelfrm = modelfrmls,
                      MoreArgs = list(data = simdatdf),
                      SIMPLIFY = FALSE)
logmarg = logmarglik(outls);
maxmod = which.max(logmarg)
cat("model with the largest marg lik","\n")
modelfrmls[[maxmod]]


ff3frm = prmibm~prmsp500+hml+smb-1;
thetamfft = MCMCregresst(modelfrm = ff3frm,
                    data = prmdfn,
                    nu = 2.1);
summarymcmc(thetamfft)

nug = seq(from = 2.1,
          to = 3.0,
          by = .10)
nug = as.matrix(nug);

outls = future_mapply("MCMCregresst",
                      nu = nug,
                      MoreArgs = list(
                        modelfrm = ff3frm,
                        data = prmdfn),
                      SIMPLIFY = FALSE
)

A = cbind(nug,t(logmarglik(outls)))
colnames(A) = c("nu","logmarg")
print(A)

## Changing multiple arguments


## Prediction
## Sampling the future

fftpredict = predictregresst(thetamfft,
                               prmdff)

## Code for sampling the future

Xf = model.matrix(ff3frm,data = prmdff)
k = dim(Xf)[2]
betam = as.matrix(thetamfft[,1:k])
s2m = thetamfft[,(k+1)]
s = dim(Xf)[1]
m = dim(betam)[1];
nu = attr(thetamfft,"nu");
yfm = matrix(0,nr = m,nc = s)
for (g in 1:m) {
    betag = t(betam[g,,drop = F]);
    s2g = s2m[g];
    tau2g = (nu-2)*s2g/nu;
    prmf = Xf %*% betag + sqrt(tau2g)*rt(s,nu);
    yfm[g,] = prmf;
}

## Predictive likelihood

nt = attr(thetamfft,"trainsize")
thetamfftns = MCMCregresst(ff3frm,data = prmdf,   # ns for n+s
                     trainsize = nt,
                     nu = 2.4)

thetamfft = MCMCregresst(ff3frm,data = prmdfn,  # data up to n
                    nu = 2.4)

diff = logmarglik(thetamfftns) - logmarglik(thetamfft)
diff = as.numeric(diff)
diff

## Change-point regression

library(MCMCpack);

priorff3ls = trainpriorregg(ff3frm,prmdfn);
B0p = solve(priorff3ls$B0_);
B0p = (B0p + t(B0p))/2;

outchange1 = MCMCregressChange(ff3frm,data = prmdfn[(nt+1):n,],
                               m = 1,
                               b0 = priorff3ls$beta0_,
                               B0 = B0p,
                               sigma.mu = priorff3ls$s20_,
                               sigma.var = priorff3ls$s2v_,
                               marginal.likelihood = "Chib95")

probstates1 = as.data.frame(attr(outchange1,"prob.state"))
rnames = rownames(prmdfn)

dts = as.Date(rnames[(nt+1):n])
colnames(probstates1) = c("ps1","ps2")


ggtsplot(probstates1,dts,y = c("ps1","ps2"));


