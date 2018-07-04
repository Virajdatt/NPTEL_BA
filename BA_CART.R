sdf = read.csv(file.choose())
sdf = sdf[,!apply(is.na(sdf),2,all)]
str(df)

data.frame("Household Number"=1:20,"Annual Income(lakhs)"=sdf$Annual_Income,"Household Area"=sdf$Household_Area,"Ownership of Sedan Car"=sdf$Ownership,check.names = F)
par()

range(sdf$Annual_Income)
range(sdf$Household_Area)

plot(sdf$Annual_Income,sdf$Household_Area, las =1,xlab ="Annual Income(Lakhs)", ylab = "Household_area",
     xlim = c(3,12), ylim = c(13,26), pch=c(21,19)[as.numeric(df$Ownership)])
legend("bottomright", inset = 0.005, c("owner","Non-Owner"), pch=c(19,21),cex=0.7,x.intersp = 0.5,y.intersp = 0.5)

#Intuvitive First  Split 
abline(v=18.8)

#Possible set of split values for numerical vars is midpoints between pairs of consecutive values for a var, 
#which are ranked as per the impurity reduction
sort(sdf$Annual_Income)
head(sort(sdf$Annual_Income),-1) +diff(sort(sdf$Annual_Income)/2)
sort(sdf$Household_Area)
head(sort(sdf$Household_Area),-1) +diff(sort(sdf$Household_Area)/2)
p1=seq(0,1,0.1)
gini = NULL
for(i in 1:length(p1))
{
  gini[i] = 1-(p1[i]^2 + (1-p1[i])^2)
}
plot(p1,gini,ylab = "Gini Index", type = "l")

#plot of entropy vs p1
entropy = NULL
for(i in 1:length(p1))
{
  entropy[i] = -(p1[i]*log2(p1[i]) + (1-p1[i])*log2(1-p1[i]))
}
plot(spline(p1,entropy), type ="l", xlab = "P1", ylab = "Entropy Measure")

#First split in Sedancar example 
summary(sdf$Ownership)

#impurity before split
giorg = 1 - (10/20)^2 - (10/20)^2
emorg = -(10/20)*log2(10/20)-(10/20)*log2(10/20)

#upper rectangle 
giniur = 1-(7/10)^2 -(3/10)^2
entru = -(7/10)*log2(7/10)-(3/10)*log(3/10)
ginilr = giniur
entrl = entru 

ginisplit1 = (10/20)*giniur + (10/20)*ginilr
emsplit1 = (10/20)*entru + (10/20)*entrl

ginidelta = ginisplit1 - giorg
emdelta = emsplit1 - emorg

#second segment 
segments(7,0,7,18.8)

#final segment 
segments(5.8,18.8,5.8,26)
segments(5.8,19.5,13,19.5)

library(rpart)

#method = "class" for a calssification tree
#method = "anova" for a regression tree

mod = rpart(Ownership~., method = "class", data = sdf, control = rpart.control(cp=0, minsplit = 2, minbucket = 1,
            maxcompete = 0, maxsurrogate = 0 , xval = 0), parms = list(split = "gini"))

plot(mod, uniform = T, branch = 0.3, compress = T, margin = 0.1, nspace = 1)
text(mod, splits = T, use.n = T, all = F, minlength = 0, cex = 0.8)
summary(mod)

library(rpart.plot)
prp(mod, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T,
    Margin = 0, digits = 0, split.cex = 0.8, under.cex = 0.8)

#Nodes are numbered
prp(mod, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T,
    Margin = 0, digits = 0, split.cex = 0.8, under.cex = 0.8, nn = T, nn.cex = 0.6)

#First Split 
modsub = snip.rpart(mod, toss = c(6:7))
prp(modsub, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T,
    Margin = 0, digits = 0, split.cex = 0.8, under.cex = 0.8, nn = T, nn.cex = 0.6)

#First three splits
modsub2 = snip.rpart(mod, toss = c(12:13,24:25))
prp(modsub2, type = 1, extra = 1, under = T, varlen = 0, cex = 0.7, compress = T,
    Margin = 0, digits = 0, split.cex = 0.8, under.cex = 0.8, nn = T, nn.cex = 0.6)

attributes(mod)
summary(mod)

#Promo-Offers 

pf = read.csv(file.choose())
pf = pf[,!apply(is.na(pf),2,all)]
View(pf)
str(pf)




 