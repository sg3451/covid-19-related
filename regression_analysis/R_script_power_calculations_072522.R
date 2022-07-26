getwd()

install.packages("pwr")
library(pwr)

#power calculations for multivariable regression
#assume R2=0.2 
#n = number of samples
#u = number of variables in model
#v(number of error degrees of freedom) = n-u-1
#f2(effect size) = (R2/1-R2), R2=coefficient of determination(% variance explained)

pwr.f2.test(u=16,v=179, f2=0.2/(1-0.2),sig.level=0.0001)
