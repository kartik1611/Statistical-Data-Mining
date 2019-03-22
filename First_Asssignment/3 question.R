rm(list = ls(all=TRUE))
library(ElemStatLearn)
library(MASS)
library(corrplot)
library(arules)
# exploring data
head(Boston)
names(Boston)
str(Boston)


data=data.frame(Boston)

# Corelatio plot 
str(data)
correlation<-cor(data)


#plot matrix
corrplot(correlation,method="number") 

# removing chas and zn column as it is not much correlated with other variables

data[["chas"]]<-NULL
data[["zn"]]<-NULL

names(data)

par(mfrow=c(2,2))


# histogram  for crim
hist(data[["crim"]])

data[["crim"]]<-ordered(cut(data[["crim"]],c(0,10,20,90)),labels=c("low_crime","mid_crime","high_crime"))


#indus
hist(data[["indus"]])

data[["indus"]]<-ordered(cut(data[["indus"]],c(0,15,25,50)),labels=c("low_indus","mid_indus","high_indus"))


# nox
hist(data[["nox"]])

data[["nox"]]<-ordered(cut(data[["nox"]],c(0,.55,.7,1)),labels=c("low_nox","mid_nox","high_nox"))

# rm
hist(data[["rm"]])

data[["rm"]]<-ordered(cut(data[["rm"]],c(0,5.5,8,12)),labels=c("low_rm","mid_rm","high_rm"))

# age
hist(data[["age"]])

data[["age"]]<-ordered(cut(data[["age"]],c(0,40,80,100)),labels=c("low_level_age","mid_level_age","high_level_age"))

# dis

hist(data[["dis"]])

data[["dis"]]<-ordered(cut(data[["dis"]],c(0,4,6,8,18)),labels=c("very_low_dis","low_dis","mid_dis","high_dis"))

# rad 

hist(data[["rad"]])

data[["rad"]]<-ordered(cut(data[["rad"]],c(0,6,40)),labels=c("near_rad","extreme_rad"))

# tax
hist(data[["tax"]])

data[["tax"]]<-ordered(cut(data[["tax"]],c(0,300,500,800)),labels=c("low_tax","mid_tax","high_tax"))

#ptratio
hist(data[["ptratio"]])

data[["ptratio"]]<-ordered(cut(data[["ptratio"]],c(12,13,18,25)),labels=c("low_ptratio","mid_ptratio","high_ptratio"))

# black
hist(data[["black"]])

data[["black"]]<-ordered(cut(data[["black"]],c(0.32,300,499)),labels=c("low_black_density","high_black_density"))

#lstat
hist(data[["lstat"]])

data[["lstat"]]<-ordered(cut(data[["lstat"]],c(0,10,30,40)),labels=c("low_lstat","mid_lstat","high_lstat"))

#medv
hist(data[["medv"]])

data[["medv"]]<-ordered(cut(data[["medv"]],c(0,25,40,60)),labels=c("low_medv","mid_medv","high_medv"))

head(data)
names(data)


# convert to binary incidence matrix
par(mfrow=c(1,1))

boston_mat<-as((data),"transactions")
summary((boston_mat))

itemFrequencyPlot(boston_mat,support=.01,cex.names=.8)

#apply apriori rules
rules<-apriori(boston_mat,parameter = list(support=.005,confidence =.5))

summary(rules)
# closer look of rules
#part c

rules_1 <- subset(rules, subset = rhs %in% "crim=low_crime" & lhs %in% "dis=very_low_dis" & lift >.5)
summary(rules_1)
inspect(head(sort(rules_1, by ='lift'),n = 6))

#part d
rulesLowPTRatio <- subset(rules, subset = rhs %in% "ptratio=low_ptratio" & lift >.7)

summary(rulesLowPTRatio)
inspect(head(sort(rulesLowPTRatio, by ='lift'),n = 6))

#part e
#regression model
model = lm(Boston$ptratio~.,data=Boston)
summary(model)

