kdf = read.csv(file.choose())
kdf = kdf[, !apply(is.na(kdf ),2,all)]
head(df)

plot(kdf$Annual_Income,kdf$Household_Area, las = 1,xlab = "Annual Income", ylab = "Hosehold Area", pch=c(21,19)[as.numeric(kdf$Ownership)])
legend("bottomright", inset = 0.005,c("Owner","Non Owner"),pch = c(19,21), cex = 0.7,x.intersp = 0.5, y.intersp = 0.5)
points(6,20, type ="p", pch = 4)

kdf[,1:2] = scale(kdf[,1:2], center = T, scale = T) 
head(kdf)

#Partioning
partidx = sample(1:nrow(kdf), 15, replace = F)
dftrain = kdf[partidx,]
dftest = kdf[-partidx,]

#Modelling
library(class)
#Building KNN

mod = knn(train =dftrain[,1:2], test = dftest[,1:2], cl = dftrain$Ownership, k = 3 )

summary(mod)
mod

#classification Matrix

table("Actual Value"=mod, "Predicted"=dftest$Ownership)
mean(mod != dftest$Ownership)

#Chossing the value of K 
modtrain = NULL
modtest = NULL
errtest = NULL
errtrain = NULL
 
for(i in 1:15) {
  modtrain = knn(train =dftrain[,1:2], test = dftest[,1:2], cl = dftrain$Ownership, k = i )
  modtest = mod = knn(train =dftrain[,1:2], test = dftest[,1:2], cl = dftrain$Ownership, k = i )
  errtrain[i] = 100*mean(mod != dftrain$Ownership)
  errtest[i] = 100*mean(mod != dftest$Ownership)
}

dfp = data.frame("Value of K"=1:15,"ErrorTraining" = errtrain,"ErrorValidation" = errtest)

round(dfp, digits =2)
plot(dfp$Value.of.K, dfp$ErrorValidation, las = 1, type = "l", xlab = "value of k", ylab = "Validation Error", ylim = c(0,80))
lines(dfp$Value.of.K, dfp$ErrorTraining)

min(errtest)
bestk = dfp[which.min(errtest),1]; bestk
