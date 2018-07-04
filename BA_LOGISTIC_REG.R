## Probability, odds and logit
# odds = p/(1-p)

curve(p/(1-p), from = 0, to = 1, type = "l", xname = "p",
      xlab = "Probability of success", ylab = "Odds")
# logit = log(odds) = log(p/(1-p))

curve(log(p/(1-p)), from = 0, to = 1, type = "l", xname = "p", las = 1,
      xlab = "Porbability of success", ylab = "logit", xaxt = "n")
axis(1, pos = 0)

library(xlsx)

# Promoffers.xlsx
dfll = read.csv(file.choose())
dfll = dfll[,!apply(is.na(dfll),2,all)]
View(dfll)

str(dfll)
dflb = dfll
dfll = dfll[,-5]



dfll$Promoffer = as.factor(dfll$Promoffer)
dfll$Online = as.factor(dfll$Online)
str(dfll)

# Partitioning: Tr: Tr = 60%:40%
partidx = sample(1:nrow(dfll), 0.6*nrow(dfll), replace = F)
dfltrain = dfll[partidx,]
dfltest = dfll[-partidx,]
str(dfltrain)
str(dfltest)

#Model with a single predictor: Income
modl = glm(Promoffer ~ Income, family = binomial(link = "logit"),
          data = dfltrain)
summary(modl)

b0 = unname(modl$coefficients[1]); b0
b1 = unname(modl$coefficients[2]); b1

#Fitted model
# "p(Promoffer = Yes|Income = x)" = 1/(1+exp(-(b0 + b1*x)))

range(dfltrain$Income)

plot(dfltrain$Income, as.numeric(as.character(dfltrain$Promoffer)),# PromoOffer is categorical var so its first converted into char and then into numerical 
     type = "p", xlab = "Income", ylab = "Promoffer",
     pch = 16, xlim = c(0, 250))

curve(1/(1+exp(-(modl$coefficients[[1]] + modl$coefficients[[2]]*x))),
      xlim = c(0,250), type = "l", xname = "x", add = T)


# Model with all predictors
mod1 = glm(Promoffer ~ ., family = binomial(link = "logit"),
           data = dfltrain)
mod1$residuals

#Options(scipen = 999)
summary(mod1)

# p = odds/(1+odds)
curve(odds/(1+odds), from = 0, to = 100, type = "l", xname = "odds",
      xlab = "Odds", ylab = "Probability of success")

# p = exp(logit)/(1+exp(logit))
curve(exp(logit)/(1+exp(logit)), from = -100, to = 100, type = "l",
      xname = "logit", las = 1,
      xlab = "logit", ylab = "Probability of success")

# Score test partition for porbability values
modtest =  predict(mod1, dfltest[, -c(3)], type = "response")
# Score test partition for logit values
modtest1 = predict(mod1, dfltest[, -c(3)], type="link")
# Classify observations using a cutoff value of 0.5
modtestc = ifelse(modtest > 0.5, 1,0)

table("Actual value" = dfltest$Promoffer, "Predicted value" = modtestc)

#Classification accuracy
mean(modtestc == dfltest$Promoffer)

#Misclassification error
mean(modtestc != dfltest$Promoffer)

DF =(data.frame("Predicted class" = modtestc,
                "Actual class" = dfltest$Promoffer,
                "Prob for 1(success)" = modtest,
                "log odds" = modtest1,
                dfltest[, -3], check.names = F))

# Cumulative Life curve
dfllift = data.frame("Probability of class 1" = modtest,
                     "Actual Class" = 
                       as.numeric(as.character(dfltest$Promoffer)),
                     check.names = F)


dfllift = dfllift[order(dfllift[,1], decreasing = T), ]
head(dfllift)
CumActualClass = cumsum(dfllift[,2])

dfllift=cbind(dfllift, CumActualClass)
head(dfllift)

range(1:nrow(dfllift))
range(dfllift$CumActualClass)
plot(1:nrow(dfllift), dfllift$CumActualClass, type = "l",
     xlab = "# cases", ylab = "Cumulative", xlim = c(0,2100), ylim=c(0, 210))
segments(0,0,2000,193, lty = 3)

legend(800, 70, inset = 0.005,
       c("Cumulative personal loan when sorted using predicted values",
         "Cumulative personal loan using average"),
       lty=c(1,2), bty = "n", cex = 0.7, x.intersp=0.3, y.intersp = 0.5)

# Decile chart
globalmean = dfllift$CumActualClass[nrow(dfllift)]/nrow(dfllift); globalmean
decilecases = round(seq(0.1,1,0.1)*nrow(dfllift)); decilecases
j = 0
decile=NULL; decilemean = NULL

for(i in decilecases) {
  j = j+1
  decilemean[j] = dfllift$CumActualClass[i]/i
  decile[j] = decilemean[j]/globalmean
}

range(decile)
barplot(decile, names.arg = as.factor(seq(1,10,1)), xlab = "Deciles",
        ylab = "Decile mean/Global mean", ylim=c(0,10))

#Measures of goodness of fit
gf = c(mod1$residuals, mod1$deviance,
       100*table(dfltrain$Promoffer)[["1"]]/
         length(dfltrain$Promoffer), mod1$iter,
       1 - (mod1$deviance/mod1$null.deviance))
gf=as.data.frame(gf, optional = T)
rownames(gf) = c("Residual dfl", "Std.Dev.Estimate",
                 "%Success in training data",
                 "#Iterations used",
                 "Multiple R-Squared")
gf

#Score training partition for probability values
modtrain = predict(mod1, dfltrain[ , -c(3)], type = "response")
# Score training partition for logit values
modtrain1 = predict(mod1, dfltrain[, -c(3)], type="link")
# Classify observations using a cutoff value of 0.5
modtrainc = ifelse(modtrain > 0.5, 1,0)

table("Actual value" = dfltrain$Promoffer, "Predicted value" = modtrainc)

#Classification accuracy
mean(modtrainc == dfltrain$Promoffer)

#Misclassification error
mean(modtrainc != dfltrain$Promoffer)

# Cumulative Life curve
dfllift2 = data.frame("Probability of class 1" = modtrain,
                      "Actual Class" = 
                        as.numeric(as.character(dfltrain$Promoffer)),
                      check.names = F)
head(dfllift2)
dfllift2 = dfllift2[order(dfllift2[,1], decreasing = T), ]
head(dfllift2)
CumActualClass = cumsum(dfllift2[,2])
dfllift2=cbind(dfllift2, CumActualClass)
head(dfllift2)

range(1:nrow(dfllift2))
range(dfllift2$CumActualClass)
plot(1:nrow(dfllift2), dfllift2$CumActualClass, type = "l",
     xlab = "# cases", ylab = "Cumulative", xlim = c(0,3100), ylim=c(0, 310))
segments(0,0,nrow(dfllift2), dfllift2$CumActualClass[nrow(dfllift2)], lty = 3)

legend(800, 70, inset = 0.005,
       c("Cumulative personal loan when sorted using predicted values",
         "Cumulative personal loan using average"),
       lty=c(1,2), bty = "n", cex = 0.7, x.intersp=0.3, y.intersp = 0.5)






















