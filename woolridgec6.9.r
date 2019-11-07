# Installing package woolridge to get datasets
#install.packages('woolridge')
#install.packages('car')
#install.packages('lmtest')
#install.packages('mass')
# Loading dataset
library(wooldridge)
library(car)
rm(list = ls())
data('nbasal')

names(nbasal)
summary(nbasal)
model1 = lm(points~exper+age+coll+expersq,data = nbasal)
print(model1)
resid1 = resid(model1)
fitted1 = fitted(model1)
plot(fitted1,resid1^2,xlab='points',ylab='residuals square',type='p',pch=19,main = 'Model 1')
bptest(model1)
summary(model1)
out = tidy(model1)
write.csv(out,file = 'result.csv')
nbasal[nbasal$exper>15,]
# number of players having experience greater than 15 years
sum(nbasal$exper>15)

#model2 adding quadratic term for age

model2 = lm(points~exper+age+coll+expersq+agesq,data = nbasal)
resid2 = resid(model2)
fitted2 = fitted(model2)
plot(fitted2,resid2^2,xlab='points',ylab='residuals square',type='p',pch=19,main = 'Model 2')
bptest(model2)
print(model2)
summary(model2)
out1 = tidy(model2)
write.csv(out1,file = 'res1.csv')

# model for estimating wage
model3 = lm(lwage~points+exper+expersq+age+coll,data = nbasal)
resid3 = resid(model3)
fitted3 = fitted(model3)
plot(fitted3,resid3^2,xlab='log(wage)',ylab='residuals square',type='p',pch=19,main = 'Model 3')
bptest(model3)
print(model3)
summary(model3)
out2 = tidy(model3)
write.csv(out2,file = 'res2.csv')

#robust regressor model
#regressing residuals squares first
tempmodel = lm(log(resid3^2)~points+exper+expersq+age+coll,data = nbasal)
tempfitted = fitted(tempmodel)
weight = exp(-tempfitted/2)
#now using the weights doing generalized linear models 
model4 = lm(lwage~points+exper+expersq+age+coll,weights = weight, data = nbasal)
resid4 = resid(model4)
fitted4 = fitted(model4)
plot(fitted4,resid4^2,xlab='log(wage)',ylab='residuals square',type='p',pch=19,main = 'Model 4')
bptest(model4)
print(model4)
summary(model4)
out3 = tidy(model4)
write.csv(out3,file = 'res3.csv')

# joint significance test
linearHypothesis(model3,c("age=0","coll=0"))


