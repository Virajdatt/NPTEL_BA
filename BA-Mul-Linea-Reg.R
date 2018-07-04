df = read.csv(file.choose())
df = df[, !apply(is.na(df ),2,all)]
head(df)
 age = 2017-df$Mfg_Year
 df = cbind(df, age)
 
 dfb =df
 df = df[, -c(1,2,3,11)]
str(df)

df$Transmission=as.factor(df$Transmission)
str(df)

#Partitinoning (60,40)

partidx = sample(1:nrow(df), 0.6*nrow(df), replace = F)
dftrain = df[partidx,]
dftest = df[-partidx,]

mod = lm(Price ~ ., dftrain)
names(mod)
summary(mod)
anova(mod)

#Measures of Goodness of fit 
gf = c(mod$df.residual, summary(mod)$r.squared, summary(mod)$sigma,anova(mod)["Residuals","Sum Sq"]
gf = as.data.frame(gf, optional = T)
rownames(df)=c("Residual df","Multiple R-squared","Std Dev","Residual SS")
gf 

modtest = predict(mod,dftest[, -4])
Residual = dftest$Price- modtest

head(data.frame("Actual Value"=dftest$Price, "Predicted Value"=modtest, Residual))

library(rminer)

M = mmetric(dftrain$Price,mof$fitted.values,c("SSE","RMSE","ME"))

print(round(M, digits + 6), na.print = "")
mmetric(dftest$Price, modtest, c("SSE","RMSE","ME"))

range(Residual)
boxplot(Residual, maintainer = "Box plot of residuals", ylab = "Residual", ylim =c(-6,7), las =1)

quantile(Residual, probs = c(0.25, 0.75))

mean(df$Price)
median(df$Price)
hist(df$Price, main="", xlab="Price") #right-skewed

#Normal Probability Plot

qqnorm(df$Price)
qqline(df$Price)


hist(log(df$Price), main = "", xlab = "Price")

mod2= lm(log(Price)~., dftrain)
modtest2 = predict(mod2,dftest[, -4])
mmetric(dftest$Price, modtest2, c("SSE","RMSE","ME"))

#Variable Selection 
library(leaps)
mod3 = regsubsets(Price ~ .,data = dftrain,nbest = 1,nvmax = NULL,force.in = NULL,force.out = NULL, method = "exhaustive",intercept = T)
mod3summ = summary(mod3)
countspch = function(x) sum(x == "*")
om = as.integer(apply(mod3summ$outmat,1,countspch)); om

#co-effcients of subset models 
coef(mod3, 1:8)


#Partial, iteravite search 
#Forward selection 

mod4 = regsubsets(Price ~ .,data = dftrain,nbest = 1,nvmax = NULL,force.in = NULL,force.out = NULL, method = "forward",intercept = T)
mod4summ = summary(mod4)
om1 = as.integer(apply(mod4summ$outmat,2,countspch)); om1

coef(mod4, 1:8)

#Backward elemination 
mod5 = regsubsets(Price ~ .,data = dftrain,nbest = 1,nvmax = NULL,force.in = NULL,force.out = NULL, method = "backward",intercept = T)
mod5summ = summary(mod5)
om2 = as.integer(apply(mod5summ$outmat,2,countspch)); om2
coef(mod5, 1:8)


#Sequential Replacment 

mod6 = regsubsets(Price ~ .,data = dftrain,nbest = 1,nvmax = NULL,force.in = NULL,force.out = NULL, method = "seqrep",intercept = T)
mod6summ = summary(mod6)
om3 = as.integer(apply(mod6summ$outmat,2,countspch)); om3

coef(mod6, 1:8)


#Sequential Regresion 

mod7 = step(lm(Price ~ .,data = dftrain), direction = "both")
mod6summ = summary(mod6)
om3 = as.integer(apply(mod6summ$outmat,2,countspch)); om3

coef(mod6, 1:8)




































































