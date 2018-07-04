# Logistics Regression

df1l= read.csv(file.choose())
df1l = df1l[, !apply(is.na(df1l),2,all)]
df1l = df1l[!apply(is.na(df1l),1,all),]
head(df1l)
str(df1l)

dfb = df1l
typeof(df1l$STD)
df1l$STD = strptime(format(df1l$STD, "%H:%M:%S"), "%H:%M:%S")
df1l$ATD = strptime(format(df1l$ATD, "%H:%M:%S"), "%H:%M:%S")
df1l$STA = strptime(format(df1l$STA, "%H:%M:%S"), "%H:%M:%S")
df1l$ATA = strptime(format(df1l$ATA, "%H:%M:%S"), "%H:%M:%S")
str(df1l)
head(df1l)

#Break departure time into appropriate intervals
range(df1l$ATD)
breaks = seq(strptime("00:00:00", "%H:%M:%S"), strptime("24:00:00", "%H:%M:%S"),
             by = "6 hours")
labelsv = c("0-6","6-12","12-18","18-24")
DEPT = cut(df1l$ATD, breaks=breaks, right = F, labels = labelsv)

df1l = cbind(df1l, DEPT)

df1l$Day = as.factor(df1l$Day)
levels(df1l$Day)
levels(df1l$Day)=c("Sunday", "Monday")
df1l$FLTIME = as.difftime(as.character(df1l$FLTIME))

str(df1l)
head(df1l)

dfb1 = df1l
df1l = df1l[,-c(1,3,5:8)]
str(df1l)
head(df1l)

df1l[sample(1:nrow(df1l),20, replace=F),]

levels(df1l$Flight.Status)
levels(df1l$Flight.Status) = c(1,0)
head(df1l$Flight.Status)
df1l$Flight.Status = relevel(df1l$Flight.Status, ref = "0")
str(df1l$Flight.Status)
head(df1l$Flight.Status)

# Partioning: 90%, 10%
partidx = sample(1:nrow(df1l), 0.9*nrow(df1l), replace = F)
df1ltrain = df1l[partidx,]
df1ltest = df1l[-partidx,1]

mod2 = glm(Flight.Status ~ ., family = binomial(link = "logit"), data = df1ltrain)

#Options(scipen = 999)
summary(mod2)

#Classify observations using a cutoff value of 0.5
mod2trainc = ifelse(mod2$fitted.values > 0.5,1,0)

table("Actual value" = df1ltrain$Flight.Status, "Predicted value" = mod2trainc)
#Classification accuracy
mean(mod2trainc == df1ltrain$Flight.Status)
#Misclassification error
mean(mod2trainc !=df1ltrain$Flight.Status)









