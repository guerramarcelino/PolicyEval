set.seed(123)
## True model
error<-rnorm(n=300,0,1)
error
X<-runif(n=300,min=0, max=4)
X

Y=3+0.2*X+error
Y
## My model
# Y=beta0+beta1X+error

reg1<-lm(Y~X)
summary(reg1)

### Do we reject the null that beta=.2?

t=(.16699-.2)/0.0486
t

########################################### What if we get multiple samples and run these regressions again?
tstat<-matrix(NA, nrow=1000, ncol=1)
for (i in 1:1000){
X=runif(300,0,4)
Y=3+0.2*X+rnorm(300,0,1)
model=lm(Y~+X)
tstat[i,]=(coef(summary(model))[2,1]-0.2)/coef(summary(model))[2,2]
}

table(tstat>1.96 | tstat< -1.96)

hist(tstat)

