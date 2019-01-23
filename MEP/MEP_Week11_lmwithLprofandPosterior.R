#data section
x=1:10
y=c(3.06,2.3,3.53,4.5,6.46,2.98,5.23,5.87,6.39,5.08)




plot(x,y,pch=19,col="orange")
abline(lm(y~x),col="steelblue",lwd=4)
fit_lm=lm(y~x)

#assumed error structure= normal
#assumed variance= normal, meaning that it should have a mean of 0
#calculate the variance of the residcuals, best guess of distribution


ls(fit_lm)

summary(fit_lm)

#Call:
#  lm(formula = y ~ x)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.7334 -0.8012  0.1252  0.4759  2.0934 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   2.6327     0.7784   3.382  0.00961 **
#  x             0.3468     0.1255   2.764  0.02451 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.139 on 8 degrees of freedom
#Multiple R-squared:  0.4885,	Adjusted R-squared:  0.4246 
#F-statistic: 7.641 on 1 and 8 DF,  p-value: 0.02451


# variance
var(fit_lm$residuals)
#1.154133

slope=fit_lm$coefficients[2]

int=fit_lm$coefficients[1]

se.slope=coef(summary(fit_lm))[,2][2]

se.int=coef(summary(fit_lm))[,2][1]

tvalue=qt(0.975,length(x)-2)

slope95CI=c(slope-se.slope*tvalue,slope+se.slope*tvalue)
int95CI=c(int-se.int*tvalue,int+se.int*tvalue)
print(slope95CI)
print(int95CI)

#chi squared 
#assuming variance 


# calculating sum of squares,sum egative log likelyhood

nll=function(theta)
{
	a=theta[1]#intercecpt
	b=theta[2]# slope
	sig=theta[3]# stadard deviatio
	predy=a+b*x
	nll=-sum(dnorm(y,predy,sig,log=TRUE)) #log likelyhood
	return(nll)
}
theta=c(3,0.3,1)
fit_nll=optim(theta,nll,hessian=TRUE)#solver-ish,can use differet optims, asside from hessian 
fit_nll
ls(fit_nll)

#output 

#$par
#[1] 2.6324464 0.3468272 1.0191708
#^ similar to the paramters we calculated before  

#$value
#[1] 14.37933 , AIC = 2 * 14.37933 +  (2*6)

#$counts
#function gradient 
#102       NA 

#$convergence
#[1] 0

#$message
#NULL

#$hessian


#[,1]          [,2]          [,3]
#[1,] 9.627335e+00  52.950342803  7.335554e-05
#[2,] 5.295034e+01 370.652399615 -5.728456e-03
#[3,] 7.335554e-05  -0.005728456  1.925514e+01

fit_nll$par[3]
#1.019171

fit_nll$par[3]^2
#1.038709


JTJinv=solve(fit_nll$hessian)#Jaco
covar=sum(fit_lm$residuals^2)/(length(x)-2)*JTJinv
sqrt(diag(covar))

covar
#[,1]          [,2]          [,3]
#[1,]  6.293745e-01 -8.991065e-02 -2.914636e-05
#[2,] -8.991065e-02  1.634739e-02  5.205922e-06
#[3,] -2.914636e-05  5.205922e-06  6.743134e-02
#margialvariaceofthe,extractthevariaceaddoasqaureroot

diag(covar)
#[1] 0.62937452 0.01634739 0.06743134

sqrt(diag(covar))
#[1] 0.7933313 0.1278569 0.2596755

summary(fit_lm)

#Call:
#  lm(formula = y ~ x)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-1.7334 -0.8012  0.1252  0.4759  2.0934 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   2.6327     0.7784   3.382  0.00961 **
#  x             0.3468     0.1255   2.764  0.02451 * 
#  ---
#  Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 1.139 on 8 degrees of freedom
#Multiple R-squared:  0.4885,	Adjusted R-squared:  0.4246 
#F-statistic: 7.641 on 1 and 8 DF,  p-value: 0.02451


#Likelihood profiles
#Likelhood associated with the parameters of interested, evulated at the max likelyhood estimates of the other paremeters of the models
#Creating a profile of the likehood models, chi sqaured to estimate parameter models


funprofb=function(theta) #profile the intercept
{
	a=theta[1]
	sig=theta[2] #standard deviation
	predy=a+bb*x  # bb ina vector in a loop
	nll=-sum(dnorm(y,predy,sig,log=TRUE)) 
}

funprofa=function(theta) # profile the slope
{
	b=theta[1]
	sig=theta[2]
	predy=aa+b*x
	nll=-sum(dnorm(y,predy,sig,log=TRUE))
}
va=seq(.5,5,length=100)
vb=seq(-0.5,1,length=100)
nlla=vector(length=100)
nllb=vector(length=100)

for(i in 1:100) # looping
{
	aa=va[i]
	bb=vb[i]
	theta=c(3,1)
	fit=optim(theta,funprofb) #profiling b, looking for a
	nllb[i]=fit$value
	theta=c(0.3,1)
	fit=optim(theta,funprofa) #profiling a, looking for b
	nlla[i]=fit$value
}
chia=pchisq(2*(nlla-min(nlla)),1) #
chib=pchisq(2*(nllb-min(nllb)),1) #
range(va[which(chia<0.975)])
# 0.8636364 4.4090909
range(vb[which(chib<0.975)]) # place bounds on the confidence intervals, calculating the intercept and slope to see if this range will be similar, probability of parameters but not parameters or ranges is what we are looking for
# 0.06060606 0.62121212

#Alternative way, bootstrap, resampling the data and then bootstrap it

#sample(x, size, replace=TRUE, prob= NULL)
newx=sample(x,10, replace=TRUE)
newx

newy=sample(y,10, replace=TRUE)
newy

summary(lm(newy~newx))

vslope=vector(length=1000)
vint=vector(length=1000)
for(i in 1:1000)
{
new=sample(1:10,10,replace=TRUE)
vint[i]=lm(y[new]~x[new]) $ coefficients[1]
vslope[i]= lm(y[new]~x[new]) $ coefficients[2]
}

#quantile it later... 

#log likelhood 

par(mfcol=c(1,2))
plot(va,nlla)
plot(va,exp(nlla))


par(mfcol=c(1,2))
plot(vb,nlla)
plot(vb,exp(nlla))

par(mfcol=c(1,2))
plot(va,chia)
plot(va,exp(chia))


par(mfcol=c(1,2))
plot(vb,chia)
plot(vb,exp(chib))




par(mfcol=c(1,2))
plot(va,chia)
plot(vb,chib)
abline(h=0.95, col="red")

hist(rchisq(1000,1))
# We can see that 

quantile(rchisq(1000,1), probs=0.975)
#97.5% and 5.13, if it's more than 2% than there is a significant difference, the models will be significantly different
# but if falls with in the 2% difference of the AIC model, they are not significantly different

#posterior distribution, mapping out te post dist of the parameters
require(MCMCpack)

posterior=function(theta)
{
	a=theta[1]
	b=theta[2]
	sig=exp(theta[3])
	predy=a+b*x
	#ll=sum(dnorm(y,predy,sig,log=TRUE))#could have time prior
	ll=sum(dnorm(y,predy,sig,log=TRUE))+dnorm(b,0.5,0.2,log=TRUE)#this has a prior, log likelihood, but# cause there is not a lot of prior information
	return(ll)
}

theta=c(3,0.3,log(1))
fit_MCMC=MCMCmetrop1R(posterior, theta, burnin = 500, mcmc = 20000, thin = 1,
             tune = 1.5, verbose = 0, seed=NA, logfun = TRUE,
             force.samp = FALSE, V = NULL, optim.method = "BFGS",
             optim.lower = -Inf, optim.upper = Inf,
             optim.control = list(fnscale = -1, trace = 0, REPORT = 10,
                                  maxit=500))
plot(fit_MCMC)
apply(fit_MCMC,2,quantile,probs=c(0.025,0.975))



# with the db in like 261, it will allow us to add in prior data, to see if there is a difference between the current data and the conditional data, galman et al, gazy turtles 


#var1       var2       var3
#2.5%  0.990377 0.07725499 -0.2762627
#97.5% 4.400303 0.62334389  0.7419846

#var1       var2       var3
#2.5%  0.8789061 0.07257341 -0.2619177
#97.5% 4.3245098 0.63243843  0.7748862


