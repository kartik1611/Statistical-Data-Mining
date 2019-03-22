rm(list=ls(all=TRUE))

set.seed(1)

library(recommenderlab)

data("MovieLense")

movie_data <- MovieLense

## analyzing data 
head(movie_data)

dim(movie_data) ## 943* 1664


# Normalizing the data

movie_data_norm <- normalize(movie_data)


image(movie_data_norm[1:200,1:200],main= "Normalized Scaled version of the  Ratings")

getRatingMatrix(movie_data)[1:3,1:3]

hist(getRatings(movie_data), main= "ratings",breaks= 10)
hist(colCounts(movie_data), breaks = 50, main = "Per Movie Rating")
hist(rowCounts(movie_data), breaks = 50, main = "Ratings by Users")


# Making Recommender using UBCF

recomm_movie <- Recommender(movie_data, method= "UBCF")
summary(recomm_movie)

# predicting best 7 values 

best_pred <- predict(recomm_movie, movie_data, n = 7)

best_movie <- bestN(best_pred, n=7)

best_movies_pred <- as(best_movie, "list")

best_movies_pred[1:7]



#  question 1 part b 


# cross validation 
eval_cal<- evaluationScheme(movie_data,method = "cross-validation",given = 15, train=0.5, goodRating=4, k=5)

# recommender model 

model<- Recommender(getData(eval_cal,"train"), "UBCF")
summary(model)

Prediction<- predict(model, getData(eval_cal, "known"), type="ratings")

# converting in matrix
new_pred <- as(Prediction, "matrix")

#Top 5
new_pred[1:5,1:5]

error_cal<- rbind(calcPredictionAccuracy(Prediction, getData(eval_cal,"unknown")))
error_cal



