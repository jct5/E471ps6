options(scipen=999)

## 1.1
houseprice = read.table("hprice1.raw")
names(houseprice) = c("price","assess","bdrms","lotsize","sqrft","colonial","lprice","lassess","llotsize","lsqrft")
model = lm(price~sqrft+bdrms, data=houseprice)


#a.)
head(houseprice)
summary(houseprice)
summary(subset(houseprice, select=c("price","sqrft","bdrms")))


#b.)
Y = houseprice$price
X = cbind(rep(1,nrow(houseprice)),houseprice$sqrft,houseprice$bdrms)

#c.)
marg.inc.bdrm = model$coef[3]

#Ceteris paribus, we expect house price to raise 
round(marg.inc.bdrm*1000,2)
#dollars for every additional bedroom.


#d.)
marg.inc.bdrm140 = model$coef[3] + 140*model$coef[2]

#Ceteris paribus, we expect house price to raise 
round(marg.inc.bdrm140*1000,2)
#dollars for an additional 140 sqft bedroom.


#e.)
summary(model)

#By the adjusted R^2, 
round(summary(model)$adj.r.squared*100,2)
#percent of variation in price is explained by sq footage and number of bedrooms.


#f.)
yhats = predict(model)

#The predicted selling price for the first house is 
round(yhats[1]*1000,2)


#g.)
resids = houseprice$price-yhats

#The residual for the first house using this model is 
resids[1]
#Therefore, the buyer underpaid for the house by 
abs(round(resids[1]*1000,2))
#dollars.


#h.)


## 1.2
#a.)
subs401k = read.table("401ksubs.raw")
names(subs401k) = c("e401k","inc","marr","male","age","fsize","nettfa","p401k","pira","incsq","agesq")
head(subs401k)

clean.subs401k = subset(subs401k, fsize==1)
sing.per.house = nrow(clean.subs401k)

#There are 
sing.per.house
#single-person households in the data set.


#b.)
initial.model = lm(nettfa~inc+age, data=clean.subs401k)
initial.model

#Ceteris paribus, we expect net total financial assets to increase 
round(initial.model$coef[2]*1000,2)
#dollars for every $1000 increase in annual income.

#Ceteris paribus, we expect net total financial assets to increase 
round(initial.model$coef[3]*1000,2)
#dollars for every additional year older the respondent is.

#There are not any major surprises in the slope coefficients. 
#The marginal effect of age is higher than income, which may be unexpected. 
#We also see that the estimate for Beta0 is 
initial.model$coef[1]
#which is negative, muddying the interpretation. After all, we do not truly expect 
#to buy a house with 0 sqft and 0 bedrooms, nor do we expect to be paid for buying such a house.


#c.)
summary(initial.model)

X401k = as.matrix(data.frame(rep(1,nrow(clean.subs401k)), clean.subs401k$inc, clean.subs401k$age))

sigma.hat.sq = t(residuals(initial.model))%*%residuals(initial.model)/(nrow(clean.subs401k)-3)
t = (initial.model$coef[3]-1)/((sigma.hat.sq)*t(c(0,0,1))%*%solve(t(X401k)%*%X401k)%*%c(0,0,1))^.5
p = pt(t, nrow(clean.subs401k)-3)

#With a p-value of 
p
#at a significance level of .01, we fail to reject the null hypothesis that Beta2 = 1.


