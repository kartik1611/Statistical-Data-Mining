# 
rm(list=ls(all=TRUE))


library(ElemStatLearn)
library(rpart)
data("marketing")

## marketing data
new_data=marketing

str(new_data)

head(new_data)

## checking na values 

sum(is.na(new_data))

## replacing na value with corresponding  median value 

for(i in 1:ncol(new_data))
{
  new_data[is.na(new_data[,i]), i] <- median(new_data[,i], na.rm = TRUE)
  
}

column_name = c("Sex", 
                "Martial_Status",
                "Householder_Status", 
                "Age", 
                "Education",
                "Ethinic_Classification", 
                "Occupation", 
                "Income", 
                "Years_In_BayArea", 
                "Dual_Incomes", 
                "Numbers_in_Household", 
                "Number_of_Children",  
                "Type_of_Home",  
                "Language_in_Home")

## number of columns 
col_rows=8993

# generating all sample variables 


Sex = sample(c(1,2), col_rows,replace = T)

Income = sample(seq(1,9), col_rows, replace = T)

Number_of_Children = sample(seq(1,9), col_rows, replace = T)

Ethinic_Classification = sample(seq(1,8), col_rows, replace = T)

Years_In_BayArea = sample(seq(1,5), col_rows, replace = T)

Martial_Status = sample(seq(1,5), col_rows, replace = T)

Dual_Incomes = sample(seq(1,3), col_rows, replace = T)

Type_of_Home = sample(seq(1,5), col_rows, replace = T)

Occupation = sample(seq(1,9), col_rows, replace = T)

Education = sample(seq(1,6), col_rows, replace = T)

Language_in_Home = sample(seq(1,3), col_rows, replace = T)

Numbers_in_Household = sample(seq(1,9), col_rows, replace = T)

Householder_Status = sample(seq(1,3), col_rows, replace = T)

Age = sample(seq(1,7), col_rows, replace = T)

 
data_frame = data.frame(Sex,
                        Income, 
                        Martial_Status,
                        Age, 
                        Education, 
                        Occupation,  
                        Years_In_BayArea, 
                        Dual_Incomes, 
                        Numbers_in_Household,
                        Number_of_Children, 
                        Householder_Status, 
                        Type_of_Home, 
                        Ethinic_Classification, 
                        Language_in_Home)

names(data_frame) = column_name

data_frame$target = 1



new_data_frame = data_frame

for(i in 1:ncol(new_data_frame)){
  new_data_frame[,i] = sample(new_data_frame[,i], nrow(new_data_frame), replace = F)
}


new_data_frame$target = 0

final_data = rbind(new_data_frame, data_frame); 

#changing the categorical variables

for(i in 1:ncol(final_data)){
  
  final_data[,i] <- as.factor(final_data[,i])
  
}


#building decision tree model

model.control <- rpart.control(minsplit = 5, xval = 10, cp = 0.002)

model = rpart(target~., final_data,control = model.control)
summary(model)
model.predict = predict(model, final_data[,-c(15)])
model.predict
