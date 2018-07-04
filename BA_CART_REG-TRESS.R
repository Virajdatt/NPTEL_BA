# Decision tree for Regression i.e Regression tree

library(xlsx)

#UsedCars.xlsx

df = read.csv(file.choose())
df = df[, !apply(is.na(df),2, all)]
head(df)

Age = 2017 - df$Mfg_Year
df = cbind(df, Age)

dfb = df
df = df[,-c(1,2,3,11)]

str(df)
df$Transmission = as.factor(df$Transmission)
str(df)

# partitioning (60%, 40%)
partidx = sample(1:nrow(df), 0.6*nrow(df), replace = F)
dftrain = df[partidx,]
dftest = df[-partidx,]

library(rpart)
mod = rpart(Price ~ ., method = "anova", data = dftrain,
            control = rpart.control(cp = 0, minsplit = 2, minbucket = 1,
                                    maxcompete = 0, maxsurrogate = 0,
                                    xval = 0))
# No. of Decision nodes
nrow(mod$splits)

#No.of terminal Nodes
nrow(mod$frame) - nrow(mod$splits)

#Pruning Process
toss1 = as.integer(row.names(mod$frame)); toss1
DFP = data.frame("toss" = toss1, "Svar" = mod$frame$var,
                 "CP" = mod$frame$complexity); DFP
DFP1 = DFP[DFP$Svar != "<leaf>", ]; DFP1 #leaf nodes remov

# Nested sequence of splits based on complexity
DFP2 = DFP1[order(DFP1$CP, -DFP1$toss, decreasing = T), ]; DFP2

rownames(DFP2) = 1:nrow(DFP2); DFP2

toss2 = DFP2$toss
#Counter for nodes to be snipped off

i = 1
modsplitv = list(); modstrainv = list(); modstestv = list()
ErrTrainv = NULL; ErrTestv = NULL

#install.packages("rminer")
library(rminer)

for (x in DFP2$Svar) {
  if (i <= length(toss2)) {
    toss3 = toss2[i:length(toss2)]
    
    modsplit = snip.rpart(mod, toss = toss3)
    
    ## Now cut down the CP table
    temp = pmax(mod$cptable[ ,1], DFP2$CP[i])
    keep = match(unique(temp), temp)
    modsplit$cptable = mod$cptable[keep, , drop = FALSE]
    modsplit$cptable[max(keep),1] = DFP2$CP[i]
    
    # Reset the variable importance
    modsplit$variable.importance = importance(modsplit)
    
    modsplitv[i] = list(modsplit)
    
    modstrain = predict(modsplit, dftrain[, -c(4)], type="vector")
    modstrainv[i] = list(modstrain)
    modstest = predict(modsplit, dftest[,-c(4)], type = "vector")
    modstestv = list(modstest)
    
    ErrTrain = mmetric(dftrain$Price, modstrain, c("RMSE"))
    ErrTrainv = c(ErrTrainv, ErrTrain)
    ErrTest = mmetric(dftest$Price, modstest, c("RSME"))
    ErrTestv = c(ErrTestv, ErrTest)
    
  }
  i = i +1
}

#  Error rate vs no.of.splits
DF = data.frame("Decision Nodes" = 0:(nrow(mod$splits)-1),
                "Error Training" = ErrTrainv,
                "Error Test" = ErrTestv, 
                check.names = F);

DF[nrow(mod1$splits),1] = nrow(mod1$splits)
modtrain = predict(mod, dftrain[,-c(4)], type = "vector")
DF[nrow(mod$splits),2] = mmetric(dftrain$Price, modtrain, c("RSME"))
modtest = predict(mod, dftest[,-c(4)], type = "vector")
DF[nrow(mod$splits),3] = mmetric(dftest$Price, modtest, c("RSME"))
DF

#Plot of error rate vs no. of splits
range(100*DF[,2])
range(100*DF[,3])
plot(smooth.spline(DF[,1], 100*DF[,2]), type = "l", ylim=c(0,1300),
     xlab = "Number of splits", ylab = "Error Rate")
lines(smooth.spline(DF[,1], 100*DF[,3]))


importance = function(fit)
{
  ff = fit$frame
  fpri = which(ff$var != "<leaf>") #points to primary splits in ff
  spri = 1 + cumsum(c(0,1 + ff$ncompete[fpri] + ff$nsurrigate[fpri]))
  spri = spri[seq_along(fpri)] # points to primaries in the splits matrix
  nsurr = ff$nsurrogate[fpri] # number of surrogates each has
  
  sname = vector("list", length(fpri))
  sval = sname
  
  ##The importance for primary splits needs to be scaled
  ## it was a printout choice for the anova method to list % improvement in
  ## the sum of squares, an importance calculation needs the total ss
  ## All the other methods report an unscaled change.
  scaled.imp = if(fit$method == "anova")
    fit$splits[spri, "improve"] * ff$dev[fpri]
  else  fit$splits[spri, "improve"]
  
  sdim = rownames(fit$splits)
  for(i in seq_along(fpri)) {
    ##points to surrogates
    if (nsurr[i] > 0L) {
      indx = spri[i] + ff$ncompete[fpri[i]] + seq_len(nsurr[i])
      sname[[i]] = sdim[indx]
      sval[[i]] = scaled.imp[i] * fit$splits[indx, "adj"]
      
    }
  }
  
  import = tapply(c(scaled.imp, unlist(sval)),
                  c(as.character(ff$var[fpri]), unlist(sname)),
                  sum)
  sort(c(import), decreasing = TRUE) # a named vector
  
}
