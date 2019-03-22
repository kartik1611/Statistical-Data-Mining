rm(list = ls(all=TRUE))
library(recommenderlab)
setwd("F://buffalo//R//sdma spring//r")


matrix <- read.delim("matrix.csv", header = TRUE, sep = ",")

matrix


matrix <- data.matrix(matrix)

# convert to rating matrix 
rrm_matrix <- as(matrix,"realRatingMatrix")

# checking class of the matrix
class(rrm_matrix)

getRatingMatrix(rrm_matrix)


# predicting using cosine

cos_recc <- Recommender(rrm_matrix, method = "IBCF",param= list(method= "Cosine"))

cosine_predict <- predict(cos_recc, rrm_matrix, type="ratings")

as(cosine_predict,"matrix")


# predicting using pearson

pearson_recc <- Recommender(rrm_matrix, method = "UBCF",param= list(method= "Pearson"))

pearson_predict <- predict(pearson_recc, rrm_matrix, type="ratings")

as(pearson_predict,"matrix")



