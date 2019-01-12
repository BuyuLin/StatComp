## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  echo = TRUE
)
set.seed(1)

## ----example1------------------------------------------------------------
n = 3+ rnorm(1)
n

## ----example2------------------------------------------------------------
x = "Double quotes\" delimitate R's strings"
x
cat(x)

## ----myexample, eval = FALSE---------------------------------------------
#  data = read.table("C:\Users\lenovo\Desktop\...")

## ----example3------------------------------------------------------------
a = expand.grid(h = c(60, 80), w = c(100, 300), sex = c("Male", "Female"))

## ----example continue----------------------------------------------------
class(a)

## ----example4------------------------------------------------------------
x = rnorm(10)
y = rnorm(10)
plot(x,y)

## ----assignment 3.5------------------------------------------------------
x = 0:4
p = c(1,2,2,2,3)/10
cp = cumsum(p)
m = 1e3
r = numeric(m)
r = x[findInterval(runif(m), cp)+1]
## construct a relative frequency table
table(r)/m
a = as.vector(table(r))
## compare the empirical with the theretical probabilities
a/sum(a)/p

## ----assignment3.5 continue----------------------------------------------
## repeat using the R sample function
b = sample(0:4, m, prob = p, replace = TRUE)
table2 = table(b)
#relative frequency table
table2
c = as.vector(table2)
# compare
c/sum(c)/p

## ----findInterval()------------------------------------------------------
a = c(-1,1,3)
m = c(-2,-1, 4)
findInterval(m,a)

## ----random_generator----------------------------------------------------
n = 1000
k = 0
j = 0
y = numeric(n)
while(k<n){
  j = j+1
  u = runif(1)
  x = runif(1)
  if(x^2*(1-x)>u){
    k = k+1
    y[k] = x
  }
}
j

## ----plot----------------------------------------------------------------
plot1 = hist(y, plot = F)
z = rbeta(1000, 3, 2)
plot2 = hist(z, plot = F)
plot(plot1)
plot(plot2,col = rgb(1,0,1,1/4),add = T)

## ----more efficient generator--------------------------------------------
n = 1000
k = 0
j = 0
y = numeric(n)
while(k<n){
  j = j+1
  u = runif(1)
  x = runif(1)
  if(x^2*(1-x)>4/9*u){
    k = k+1
    y[k] = x
  }
}
j

## ----histogram-----------------------------------------------------------
plot1 = hist(y, plot = F)
z = rbeta(1000, 3, 2)
plot2 = hist(z, plot = F)
plot(plot1)
plot(plot2,col = rgb(1,0,1,1/4),add = T)

## ----generat-------------------------------------------------------------
n = 1000
r=  4
beta = 2
lambda = rgamma(n, r, beta)
x = rexp(n, lambda)

## ----Pareto distribution-------------------------------------------------
f = function(x){
  2*(1/(1-x)^0.25-1)
}
u = runif(1000)
y = f(u)
figure1 = hist(x, plot = F, breaks = 40)
figure2 = hist(y, plot = F, breaks = 20)
plot(figure1)
plot(figure2, col = 'red', add =T)

## ----5.4-----------------------------------------------------------------
f = function(x,m){
  u = runif(m)
  30*mean(x^3*u^2*(1-x*u)^2)
}
x = (1:9)/10
m = 10000
result = c(NULL)
for(i in 1:9){
  result = c(result, f(x[i], m))
}
result

## ------------------------------------------------------------------------
another = pbeta(x, 3, 3)
result/another

## ------------------------------------------------------------------------
z = rbeta(m, 3,3)
dim(x) = length(x)
p = apply(x, MARGIN = 1, FUN = function(x,z){mean(z<x)}, z = z)
result/p

## ------------------------------------------------------------------------
f = function(m, sigma){
  u = runif(m/2)
  x = sqrt(-2*sigma^2*log(1-u))
  x_ = sqrt(-2*sigma^2*log(u))
  c(x, x_)
}
m = 10000
sigma = 2
result = f(m, sigma);
X = result[1:(m/2)]
X_ = result[(m/2+1):m]
var1 = sd((X+X_)/2)
X1 = sqrt(-2*sigma^2*log(1-runif(m/2)))
X2 = sqrt(-2*sigma^2*log(1-runif(m/2)))
var2 = sd((X1+X2)/2)
var1/var2

## ------------------------------------------------------------------------
m = 10000
u = runif(m)
cov = 2*((mean(sqrt(log(1-u)*log(u)))-mean(sqrt(-log(u)))^2))
sqrt((4-pi+2*cov)/((4-pi)*2))

## ------------------------------------------------------------------------
m = 100000
g = function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
}
u = rnorm(m)
simu1 = g(u)/dnorm(u)
u = sqrt(-2*log(runif(m)))
simu2 = g(u)/(u*exp(-u^2/2))
plot(g, xlim = c(1,10))
plot(function(x){1/sqrt(2*pi)*exp(-x^2/2)}, xlim = c(1,10),add = TRUE, col = 'red')
plot(function(x){x*exp(-x^2/2)}, xlim = c(1,10),add = TRUE, col = 'yellow')
sd(simu1)
sd(simu2)

## ------------------------------------------------------------------------
m = 10000
g = function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
}
u = rnorm(m)
simu1 = g(u)/dnorm(u)
u = sqrt(-2*log(runif(m)))
simu2 = g(u)/(u*exp(-u^2/2))
mean(simu1)
mean(simu2)

## ------------------------------------------------------------------------
m = 10000
g = function(x){
  x^2/sqrt(2*pi)*exp(-x^2/2)*(x>1)
}
n = 20
theta = matrix(1:(2*n), nrow = 2)
for (i in 1:n){
  u = rnorm(m)
  simu1 = g(u)/dnorm(u)
  theta[1,i] = mean(simu1)
  u = sqrt(-2*log(runif(m)))
  simu2 = g(u)/(u*exp(-u^2/2))
  theta[2,i] = mean(simu2)
}
sd(theta[1,])
sd(theta[2,])

## ------------------------------------------------------------------------
theta[1,]
theta[2,]

## ----6.9-----------------------------------------------------------------
Gini = function(x){
  x = sort(x)
  x.bar = mean(x)
  n = length(x)
  a = 1:n
  a = 2*a-(n+1)
  G = 1/(n^2*x.bar)*sum(a*x)
  G
}
m = 1e3
n = 1e2
lognormal.simu = uniform.simu = bernoulli.simu = numeric(m)
for(i in 1:m){
  x = rlnorm(n)
  y = runif(n)
  z = sample(0:1, n, replace = TRUE)
  lognormal.simu[i] = Gini(x)
  uniform.simu[i] = Gini(y)
  bernoulli.simu[i] = Gini(z)
}
summary(lognormal.simu)
lognormal.simu = sort(lognormal.simu)
lognormal.simu[((1:9)/10)*m]
plot(density(lognormal.simu))
plot(density(uniform.simu))
plot(density(bernoulli.simu))

## ------------------------------------------------------------------------
summary(uniform.simu)
plot(density(uniform.simu))

## ------------------------------------------------------------------------
library(boot)
sigma = 1
m = 100
n = 100
boot.Gini = function(x,i) Gini(x[i])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2) 
for(i in 1:m){
  x = rlnorm(n,0,sigma)
  de = boot(data = x, statistic = boot.Gini, R = 100)
  ci = boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]            
  ci.perc[i,]<-ci$percent[4:5]
}
G = (pnorm(0.5, 0, sqrt(0.5))-0.5)*2
cat('norm =',mean(ci.norm[,1]<=G & ci.norm[,2]>=G), 
    'basic =',mean(ci.basic[,1]<=G & ci.basic[,2]>=G), 
    'perc =',mean(ci.perc[,1]<=G & ci.perc[,2]>=G))


## ------------------------------------------------------------------------
sigma = 1
m = 100
n = 500
boot.Gini = function(x,i) Gini(x[i])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,m,2) 
for(i in 1:m){
  x = rlnorm(n,0,sigma)
  de = boot(data = x, statistic = boot.Gini, R = 200)
  ci = boot.ci(de,type=c("norm","basic","perc"))
  ci.norm[i,]<-ci$norm[2:3]
  ci.basic[i,]<-ci$basic[4:5]            
  ci.perc[i,]<-ci$percent[4:5]
}
G = (pnorm(0.5, 0, sqrt(0.5))-0.5)*2
cat('norm =',mean(ci.norm[,1]<=G & ci.norm[,2]>=G), 
    'basic =',mean(ci.basic[,1]<=G & ci.basic[,2]>=G), 
    'perc =',mean(ci.perc[,1]<=G & ci.perc[,2]>=G))

## ------------------------------------------------------------------------
mu = 0
sigma = 1
M = 1000
n = 50
Sig1 = Sig2 = Mu_hat = Sigma_hat = result = G1 = G2 = numeric(M)
for (i in 1:M) {
  x = rlnorm(n, mu, sigma)
  y = log(x)
  S = sd(y)
  Sig1[i] = sqrt((n - 1) * S^2 / qchisq(0.025, n - 1, lower.tail = FALSE))
  Sig2[i] = sqrt((n - 1) * S^2 / qchisq(1 - 0.025, n - 1, lower.tail = FALSE))
}

for(i in 1:M){
  G1[i] = (pnorm(Sig1[i]/2, 0, sqrt(0.5))-0.5)*2
  G2[i] = (pnorm(Sig2[i]/2, 0, sqrt(0.5))-0.5)*2
}
G = (pnorm(0.5, 0, sqrt(0.5))-0.5)*2
mean(G1<=G & G2>=G)

## ------------------------------------------------------------------------
library(MASS)
m = 1e3
Sigma = matrix(c(2,0.2,0.2,1),2,2)
result = matrix(1:(3*m), nrow = 3)
for(i in 1:m){
  mnorm = mvrnorm(n = 100, rep(0,2), Sigma)
  x = mnorm[,1]
  y = mnorm[,2]
  result[1,i] = cor.test(x,y, alternative = "two.sided", method = 'pearson')$p.value
  result[2,i] = cor.test(x, y, alternative = "two.sided", method = 'kendal')$p.value
  result[3,i] = cor.test(x, y, alternative = "two.sided", method = 'spearman')$p.value
}
for(i in 1:3){
  print(mean(result[i,]<=0.05))
}

## ------------------------------------------------------------------------
for(i in 1:m){
  x = rchisq(100, df = 3)
  y = 1/10*x +9/10*rchisq(100, df = 3)
  result[1,i] = cor.test(x,y, alternative = "two.sided", method = 'pearson')$p.value
  result[2,i] = cor.test(x, y, alternative = "two.sided", method = 'kendal')$p.value
  result[3,i] = cor.test(x, y, alternative = "two.sided", method = 'spearman')$p.value
}
for(i in 1:3){
  print(mean(result[i,]<=0.05))
}

## ------------------------------------------------------------------------
m = 1e3
for(i in 1:5){
  Sigma = matrix(c(1,i/10,i/10,1),2,2)
  result = matrix(1:(3*m), nrow = 3)
  for(i in 1:m){
    mnorm = mvrnorm(n = 100, rep(0,2), Sigma)
    x = mnorm[,1]
    y = mnorm[,2]
    result[1,i] = cor.test(x,y, alternative = "two.sided", method = 'pearson')$p.value
    result[2,i] = cor.test(x, y, alternative = "two.sided", method = 'kendal')$p.value
    result[3,i] = cor.test(x, y, alternative = "two.sided", method = 'spearman')$p.value
  }
  for(i in 1:3){
    print(mean(result[i,]<=0.05))
  }
}

## ----7.1-----------------------------------------------------------------
library(bootstrap)
n = length(law$GPA)
cor.jack = numeric(n)
cor = cor(law$LSAT, law$GPA)
for(i in 1:n)
  cor.jack[i] = cor(law$LSAT[-i], law$GPA[-i])
bias.jack = (n-1)*(mean(cor.jack)-cor)
var.jack = (n-1)^2/n*var(cor.jack)
se.jack = sqrt(var.jack)
bias.jack
se.jack

## ----7.1(2)--------------------------------------------------------------
B = 200
n = nrow(law)
R = numeric(B)
for(b in 1:B){
  i = sample(1:n, size = n, replace = TRUE)
  LSAT = law$LSAT[i]
  GPA = law$GPA[i]
  R[b] = cor(LSAT, GPA)
}
se.boot = sd(R)
bias.boot = mean(R)-cor
se.boot
bias.boot

## ----7.5-----------------------------------------------------------------
library(boot)
boot.mean = function(x,i) mean(x[i,])
ci.norm<-ci.basic<-ci.perc<-ci.bca<-matrix(NA,1,2)
de <- boot(data=aircondit,statistic=boot.mean, R = 999)
ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
ci.norm[1,]<-ci$norm[2:3]
ci.basic[1,]<-ci$basic[4:5]
ci.perc[1,]<-ci$percent[4:5]
ci.bca[1,]<-ci$bca[4:5]
cat('norm =',ci.norm[1,], 'basic =',ci.basic[1,], 'perc =',ci.perc[1,], 'BCa =',ci.bca[1,])

## ----7.5(2)--------------------------------------------------------------
lambda = 0.1
m = 1e2
n = 1e2 ##iteration numbers
data.simu = rexp(m, lambda)
cover.norm = cover.basic = cover.perc = cover.bca =0

## ------------------------------------------------------------------------
boot.mean = function(x,i) mean(x[i])
for(i in 1:n){
  de <- boot(data = data.simu,statistic=boot.mean, R = 999)
  ci <- boot.ci(de,type=c("norm","basic","perc","bca"))
  ci.norm[1,]<-ci$norm[2:3]
  ci.basic[1,]<-ci$basic[4:5]
  ci.perc[1,]<-ci$percent[4:5]
  ci.bca[1,]<-ci$bca[4:5]
  cover.norm = cover.norm+I(ci.norm[1,1]<=1/lambda & 1/lambda<=ci.norm[1,2])
  cover.basic = cover.basic+I(ci.basic[1,1]<=1/lambda & 1/lambda<=ci.basic[1,2])
  cover.perc = cover.perc+I(ci.perc[1,1]<=1/lambda & 1/lambda<=ci.perc[1,2])
  cover.bca = cover.bca+I(ci.bca[1,1]<=1/lambda & 1/lambda<=ci.bca[1,2])
}
cover.norm/n
cover.basic/n
cover.perc/n
cover.bca/n

## ----7.8-----------------------------------------------------------------
co = cov(scor)
n = length(scor$mec)
eig = eigen(co)$values
theta = eig[1]/sum(eig)
theta.jack = numeric(n)
for(i in 1:n){
  co = cov(scor[-i,])
  eig = eigen(co)$values
  theta.jack[i] = eig[1]/sum(eig)
}
bias.jack = (n-1)*(mean(theta.jack)-theta)
var.jack = (n-1)^2/n*var(theta.jack)
se.jack = sqrt(var.jack)
bias.jack
se.jack

## ----7.8(2)--------------------------------------------------------------
B = 200
theta.boot = numeric(B)
for(j in 1:B){
  i = sample(1:n, n, replace = TRUE)
  co = cov(scor[i,])
  eig = eigen(co)$values
  theta.boot[j] = eig[1]/sum(eig)
}
bias.boot = mean(theta.boot)-theta
se.boot = sd(theta.boot)
bias.boot
se.boot

## ----7.11----------------------------------------------------------------
library(DAAG)
chemical = ironslag$chemical
magnetic = ironslag$magnetic
n = length(magnetic)
e1 <- e2 <- e3 <- e4 <- matrix(0, nrow = n, ncol = n)

## ------------------------------------------------------------------------
for(i in 1:(n-1))
  for(j in (i+1):n){
    y = magnetic[c(-i, -j)]
    x = chemical[c(-i, -j)]
    J1 = lm(y ~ x)
    yhat1 = J1$coef[1]+J1$coef[2]*chemical[c(i,j)]
    e1[i,j] = mean((magnetic[c(i,j)]-yhat1)^2)
    
    J2 = lm(y ~ x+I(x^2))
    yhat2 = J2$coef[1]+J2$coef[2]*chemical[c(i,j)]+J2$coef[3]*chemical[c(i,j)]^2
    e2[i,j] = mean((magnetic[c(i,j)]-yhat2)^2)
    
    J3 = lm(log(y) ~ x)
    logyhat3 = J3$coef[1]+J3$coef[2]*chemical[c(i,j)]
    yhat3 = exp(logyhat3)
    e3[i,j] = mean((magnetic[c(i,j)]-yhat3)^2)
    
    J4 = lm(log(y) ~ log(x))
    logyhat4 = J4$coef[1]+J4$coef[2]*log(chemical[c(i,j)])
    yhat4 = exp(logyhat4)
    e4[i,j] = mean((magnetic[c(i,j)]-yhat4)^2)
    
  }
c(2/(n*(n-1))*sum(e1),2/(n*(n-1))*sum(e2),2/(n*(n-1))*sum(e3),2/(n*(n-1))*sum(e4))


## ----1(1)----------------------------------------------------------------
attach(chickwts)
X = sort(as.vector(weight[feed == "soybean"]))
Y = sort(as.vector(weight[feed == 'linseed']))
R = 999
z = c(X,Y)
K = 1:26
n = length(X)
m = length(Y)
reps = numeric(R)

## ----1(2)----------------------------------------------------------------
CMstat = function(x,y){
  x = sort(x)
  y = sort(y)
  F = function(x0){
    
    findInterval(x0, x)/n
  }
  G = function(y0){
    findInterval(y0, y)/m
  }
  m*n/(m+n)^2*(sum((F(x)-G(x))^2)+sum((F(y)-G(y))^2))
}
for(i in 1:R){
  k = sample(K, size = n, replace = FALSE)
  x1 = z[k]
  y1 = z[-k]
  reps[i] = CMstat(x1, y1)
  t0 = CMstat(X,Y)
  p.value = mean(c(t0, reps)>=t0)
}
p.value

## ----2(1)----------------------------------------------------------------
library(RANN)
library(boot)
library(energy)
library(Ball)
n1 = 50
n2 = 30
p = 2
m = 200
k = 3
R = 200
n = n1+n2
N = c(n1, n2)

Tn = function(z, ix, sizes, k){
  n1 = sizes[1]
  n2 = sizes[2]
  z = z[ix, ]
  NN = nn2(data = z, k = k+1)
  block1 = NN$nn.idx[1:n1, -1]
  block2 = NN$nn.idx[(n1+1):n, -1]
  i1 = sum(block1 < n1+.5)
  i2 = sum(block2 > n1+.5)
  (i1+i2)/(k*n)
}

eqdist.nn = function(z, sizes, k, R){
  boot.obj = boot(data = Z, statistic = Tn, R = R, sim = 'permutation', sizes = sizes, k = 3)
  ts = c(boot.obj$t0, boot.obj$t)
  p.value = mean(ts>=ts[1])
  list(statistic = ts[1], p.value = p.value)
}

p.values = matrix(NA, m ,3)
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2), rnorm(n2, sd = 1.5))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----2(2)----------------------------------------------------------------
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = 0.5), rnorm(n2, sd = 1.5))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----2(2)_2--------------------------------------------------------------
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = 0.1), rnorm(n2, sd = 1.5))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----2(2)_3--------------------------------------------------------------
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2, mean = 0.3, sd = 1.3), rnorm(n2, sd = 1.5))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.05
pow = colMeans(p.values<alpha)
pow

## ----3(1)----------------------------------------------------------------
for(i in 1:m){
  X = matrix(rt(n1*p, df = 1), ncol = p)
  Y = cbind(rt(n2, df = 2), rt(n2, df = 3))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.1
pow = colMeans(p.values<alpha)
pow

## ----3(2)----------------------------------------------------------------
library(MASS)
for(i in 1:m){
  p1 = 0.7
  p2 = 0.3
  U1 = runif(n1)
  U2 = runif(n2)
  X = ((U1<p1)*mvrnorm(n1, mu = c(0,0), Sigma = diag(c(1,1)))+(1-(U1<p1))*mvrnorm(n1, mu = c(1,1), Sigma = matrix(c(2,1.3,1.3,1), nrow = 2)))
  Y = ((U2<p2)*mvrnorm(n2, mu = c(0,0), Sigma = diag(c(1,1)))+(1-(U2<p2))*mvrnorm(n2, mu = c(1,1), Sigma = matrix(c(2,1.3,1.3,1), nrow = 2)))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.1
pow = colMeans(p.values<alpha)
pow

## ----3(3)----------------------------------------------------------------
library(MASS)
for(i in 1:m){
  p1 = 0.4
  p2 = 0.4
  U1 = runif(n1)
  U2 = runif(n2)
  X = ((U1<p1)*mvrnorm(n1, mu = c(0,0), Sigma = diag(c(1,1)))+(1-(U1<p1))*mvrnorm(n1, mu = c(1,1), Sigma = matrix(c(1,0,0,1), nrow = 2)))
  Y = ((U2<p2)*mvrnorm(n2, mu = c(0.5,0), Sigma = diag(c(2,1)))+(1-(U2<p2))*mvrnorm(n2, mu = c(1,1.2), Sigma = matrix(c(2,1.3,1.3,1), nrow = 2)))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.1
pow = colMeans(p.values<alpha)
pow

## ----4-------------------------------------------------------------------
n1 = 100
n2 = 10
N = c(n1, n2)
n = n1+n2
for(i in 1:m){
  X = matrix(rnorm(n1*p), ncol = p)
  Y = cbind(rnorm(n2), rnorm(n2, mean = 0.5,sd = 1.8))
  Z = rbind(X, Y)
  p.values[i,1] = eqdist.nn(Z, sizes = N, k, R = R)$p.value
  p.values[i,2] = eqdist.etest(Z, sizes = N, R = R)$p.value
  p.values[i,3] = bd.test(x = X, y = Y, R = R, seed = i)$p.value
}
alpha = 0.1
pow = colMeans(p.values<alpha)
pow

## ------------------------------------------------------------------------
burnin = 4000
N = 100000
rw.Metropolis = function(n, sigma, x0, N){
  x = numeric(N)
  x[1] = x0
  u = runif(N)
  k = 0
  for(i in 2:N){
    y = rnorm(1, x[i-1], sigma)
    if(u[i]<=(dt(y, n)/dt(x[i-1], n))){
      x[i] = y
      k = k+1
    }else{
      x[i] = x[i-1]
    }
  }
  return(list(x = x, k=k))
}

result = rw.Metropolis(n = 1, sigma = 0.9, x0 = 25, N = N)
simulation = result$x[(burnin+1):N]

quant.simu = quantile(simulation, (5:95)/100)
quant.true = qcauchy((5:95)/100)
plot(quant.simu, quant.true)
g = function(x) x
curve(g, -6,6, add = TRUE, )

## ------------------------------------------------------------------------
data = rt(10000, df = 1)
quant.data = quantile(data, (5:95)/100)
quant.true = qcauchy((5:95)/100)
plot(quant.simu, quant.true)
g = function(x) x
curve(g, -6,6, add = TRUE, )

## ------------------------------------------------------------------------
result = lm(quant.true~quant.simu)
result$coefficients

## ------------------------------------------------------------------------
quant.simu = quantile(simulation, (10:90)/100)
quant.true = qcauchy((10:90)/100)
result = lm(quant.true~quant.simu)
result$coefficients

## ------------------------------------------------------------------------
N = 10000
burnin = 2000
w = 1
x = numeric(N)
x[1] = 0.5
f <- function(y) {
  I(y>0 & y<1)*(0.5 + y/4)^125 * ((1-y)/4)^18 * ((1-y)/4)^20 * (y/4)^34
}
for (i in 2:N) {
  k = 0
  u = runif(1)
  x1 = runif(1, 0, w)
  if(u < f(x1)/f(x[i-1])){
    x[i] = x1
    k = k+1
  }
  else x[i] = x[i-1]
}

xp = x[(burnin+1):N]
beta = mean(xp)
print(round(c(0.5 + beta/4, (1-beta)/4, (1-beta)/4, beta/4), 3))
print(round(c(125, 18, 20, 34)/sum(c(125, 18, 20, 34)),3))

hist(xp, freq = F)

## ------------------------------------------------------------------------
plot(x, type = "l", xlab = "x", xlim = c(1, N), ylim = range(x))

## ------------------------------------------------------------------------
nlm(f = function(beta){
  -I(beta>0 & beta<1)*(125*log(0.5+beta/4)+38*log((1-beta)/4)+34*log(beta/4))
}, p = 0.5)$estimate
beta

## ------------------------------------------------------------------------
N = 2100
K = 50
burnin = 2000
n = N-burnin
w = 1
x = matrix(NA, nrow = K, ncol = N)
x[,1] = 0.5
f <- function(y) {
  I(y>0 & y<1)*(0.5 + y/4)^125 * ((1-y)/4)^18 * ((1-y)/4)^20 * (y/4)^34
}
for (i in 2:N) {
  u = runif(K)
  x1 = runif(K, 0, w)
  flag = u<f(x1/x[,(i-1)])
  x[,i] = I(u<f(x1)/f(x[,(i-1)]))*x1+I(u>=f(x1)/f(x[,(i-1)]))*x[,(i-1)]
}
samp = x[,(burnin+1):N]
row.mean = rowMeans(samp)
B_n = var(row.mean)*n
W_n = mean((samp-row.mean)^2)
Phi = (n-1)/n*W_n+1/n*B_n
R = sqrt(Phi/W_n)
R

## ----11.4----------------------------------------------------------------
k = c(4:25, 100, 500, 1000)
a = numeric(length(k))
for(i in (1:length(k))){
  f = function(a){
    pt(sqrt(a^2*(k[i]-1)/(k[i]-a^2)), df = k[i]-1, lower.tail = FALSE, log.p = T) / pt(sqrt(a^2*k[i]/(k[i]+1-a^2)), df = k[i], lower.tail= FALSE, log.p = T) -1
  }
  a[i] = uniroot(f, c(0.001, sqrt(k[i])-0.00001))
}
print(a)

## ------------------------------------------------------------------------
log(1e-330)
log(1e-115)+log(1e-115)

## ------------------------------------------------------------------------
1e-330-1e-331
log(1e-330)-log(1e-331)
log(1e-115)+log(1e-115)-(log(1e-115)+log(1e-116))

## ----echo=FALSE, eval=TRUE-----------------------------------------------
dat <- rbind(Genotype=c('AA','BB','OO','AO','BO','AB','Sum'),
              Frequency=c('p^2','q^2','r^2','2pr','2qr','2pq',1),
              Count=c('nAA','nBB','nOO','nAO','nBO','nAB','n'))
knitr::kable(dat,format='latex')

## ------------------------------------------------------------------------
nA_ = 28; nB_ = 24; nOO = 41;nAB = 70
l = c(NULL)

ll = function(p,q,r){
  -(nA_*log(p^2+2*p*r)+nB_*log(q^2+2*q*r)+2*nOO*log(r)+nAB*log(2*p*q))
}

pi1 = 0.2
pi2 = 0.2
nAA = nA_*pi1
nBB = nB_*pi2
nAO = nA_-nAA
nBO = nB_-nBB
p = 2*nAA+nAO+nAB
q = 2*nBB+nBO+nAB
r = 2*nOO+nAO+nBO
sum = p+q+r
p = p/sum
q = q/sum
r = r/sum

print(c(p,q,r))

l = c(l, ll(p,q,r))


EM = function(tol = 1e-12){
  iter = 1
  flag = TRUE
  while(flag == TRUE){
    pi1 = p/(p+2*r)
    pi2 = q/(q+2*r)
    nAA = nA_*pi1
    nBB = nB_*pi2
    nAO = nA_-nAA
    nBO = nB_-nBB
    p.new = 2*nAA+nAO+nAB
    q.new = 2*nBB+nBO+nAB
    r.new = 2*nOO+nAO+nBO
    sum = p.new+q.new+r.new
    p.new = p.new/sum
    q.new = q.new/sum
    r.new = r.new/sum
    flag = max(abs(p.new-p), abs(q.new-q), abs(r.new-r))>tol
    iter = iter+1
    p = p.new
    q = q.new
    r = r.new
    l = c(l, ll(p,q,r))
  }
  list(p = p, q = q, r = r, iter = iter, ll = l)
}

result = EM()

result

plot(result$ll)


## ------------------------------------------------------------------------
nlm(f = function(beta){
  p = beta[1]; q = beta[2]; r = 1-p-q
  -(nA_*log(p^2+2*p*r)+nB_*log(q^2+2*q*r)+2*nOO*log(r)+nAB*log(2*p*q))
}, p = c(0.4,0.3))

## ---- eval = FALSE-------------------------------------------------------
#  eta = 0
#  theta = seq(1, 10, length.out = 10)
#  n = 100
#  f = function(x, eta, theta){
#    1/(theta*pi*(1+((x-eta)/theta)^2))
#  }
#  v = matrix(NA, length(theta), n)
#  q = seq(-20, 20, length.out = n)
#  for (i in 1:length(theta)) {
#    for (j in 1:n) {
#      v[i, j] = integrate(f, lower = -Inf, upper = q[j], theta = theta[i], eta)$value
#    }
#  }
#  par(mfrow = c(4, 5))
#  g = function(x) {x}
#  for (i in 1:length(theta)) {
#    qc = pcauchy(q, scale = theta[i])
#    qqplot(qc, v[i, ], xlab = "Cauchy Quantiles", ylab = "Integration Quantiles")
#    curve(g, add = T, type = "l", col = "red")
#  }

## ------------------------------------------------------------------------
attach(mtcars)
formulas<-list(
  mpg ~ disp,
  mpg ~ I(1/disp),
  mpg ~ disp+wt,
  mpg ~ I(1/disp)+wt
)
par(mfrow = c(2,2))

models1 = lapply(formulas, lm)
print(models1)

for(item in formulas)
  print(lm(item))


## ------------------------------------------------------------------------
bootstraps <- lapply(1:10, function(i) {   rows <- sample(1:nrow(mtcars), rep = TRUE)
  mtcars[rows, ]
}) 

for(i in 1:10){
  mpg = bootstraps[[i]]$mpg
  disp = bootstraps[[i]]$disp
  print(lm(mpg~disp))
}

models2 = lapply(bootstraps, function(o){
  lm(o$mpg~o$disp)})
print(models2)

## ------------------------------------------------------------------------
replicate(10, expr = {
  rows <- sample(1:nrow(mtcars), rep = TRUE)
  a = mtcars[rows, ]
  mpg = a$mpg
  disp = a$disp
  lm(mpg~disp)
})

## ------------------------------------------------------------------------
rsq <- function(mod) summary(mod)$r.squared
lapply(models1, rsq)
lapply(models2, rsq)

## ------------------------------------------------------------------------
trials <- replicate(
  100,
  t.test(rpois(10, 10), rpois(7, 10)),
  simplify = FALSE
)
sapply(trials, function(o) o$p.value)

## ------------------------------------------------------------------------
for(i in 1:100)
  print(trials[[i]]$p.value)

## ----eval = FALSE--------------------------------------------------------
#  library(parallel)
#  mcvMap <- function(f, FUN.VALUE , ...) {
#      out <- mcMap(f, ...)
#      vapply(out, identity, FUN.VALUE)
#  }

## ------------------------------------------------------------------------
chisq.test1 <- function(x, y){
  m = table(x, y)
  margin1 <- rowSums(m)
  margin2 <- colSums(m)
  n <- sum(m)
  me <- tcrossprod(margin1, margin2) / n
  x_stat = sum((m - me)^2 / me)
  x_stat
}


## ------------------------------------------------------------------------
x = sample(1:3, replace = TRUE, size = 50)
y = sample(1:3, replace = TRUE, size = 50)
chisq.test1(x, y)
chisq.test(x,y)

## ------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  suppressWarnings(chisq.test(x,y)),
  chisq.test1(x,y)
)

## ------------------------------------------------------------------------
table1 = function(x,y){
  rowname = unique(x)
  colname = unique(y)
  a = matrix(0, length(rowname), length(colname), dimnames = list(rowname, colname))
  for(i in seq_along(x)){
    m = which(rowname == x[i])
    n = which(colname == y[i])
    a[m,n] = a[m,n]+1
  }
  
  class(a) = 'table'
  a
}

## ------------------------------------------------------------------------
a = 1:5
b = sample(1:5)
table(a,b)
table1(a,b)

## ------------------------------------------------------------------------
library(microbenchmark)
microbenchmark(
  table(x,y),
  table1(x,y)
)

## ------------------------------------------------------------------------
chisq.test2 <- function(x, y){
  m = table1(x, y)
  margin1 <- rowSums(m)
  margin2 <- colSums(m)
  n <- sum(m)
  me <- tcrossprod(margin1, margin2) / n
  x_stat = sum((m - me)^2 / me)
  x_stat
}

## ----test the speed------------------------------------------------------
microbenchmark(
  suppressWarnings(chisq.test(x,y)),
  chisq.test1(x,y),
  chisq.test2(x,y)
)

## ------------------------------------------------------------------------
N = 10000
burnin = 2000
RMCMC = function(N){
  x = numeric(N)
  x[1] = 0.5
  f <- function(y) {
    I(y>0 & y<1)*(0.5 + y/4)^125 * ((1-y)/4)^18 * ((1-y)/4)^20 * (y/4)^34
  }
  for (i in 2:N) {
    k = 0
    u = runif(1)
    x1 = runif(1)
    if(u < f(x1)/f(x[i-1])){
      x[i] = x1
      k = k+1
    }
    else x[i] = x[i-1]
  }
  x
}
x = RMCMC(N)
xp = x[(burnin+1):N]
hist(xp)
q1 = quantile(xp, seq(0.05,0.95,0.025))

## ---- eval = FALSE-------------------------------------------------------
#  library(Rcpp)
#  sourceCpp('/Users/Daniel/Desktop/MCMC.cpp')
#  
#  
#  y = numeric(N)
#  MCMC(y, N)
#  hist(y)
#  yp = y[(burnin+1):N]
#  q2 = quantile(yp, seq(0.05,0.95,0.025))
#  qqplot(q1,q2)
#  g = function(x)
#    x
#  curve(g, from = 0.05, to = 0.95, add = TRUE, col = 'red')

## ---- eval=FALSE---------------------------------------------------------
#  library(microbenchmark)
#  microbenchmark(
#    x = RMCMC(1000),
#    MCMC(y,1000)
#  )

