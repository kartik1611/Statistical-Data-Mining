rm(list=ls(all="TRUE"))
library('caret')
library("multtest")
library("fpc")
library("cluster")
library("bootcluster")
library("fossil")
setwd("F://buffalo//R//sdma spring//r//1st_assignment_sdma//Second_Assisgnment//HW_2_UB50291058")

load('primate.scapulae.rda')

# Removing NA
str(primate.scapulae)

sum(is.na(primate.scapulae$gamma))
primate.scapulae <- subset(primate.scapulae, select = -c(gamma))
sum(is.na(primate.scapulae$gamma))


new_data = primate.scapulae[,-ncol(primate.scapulae)]
str(new_data)

new_data = new_data[,-ncol(new_data)]
str(new_data)

# scaling the attributes
new_data = scale(new_data)

# calculate the distance for hierarchical clustering 
dist <- dist(new_data)
dim(as.matrix(dist))


# Single hierarchical clustering 
heri_clust_single <- hclust(dist, method = "single")
plot(heri_clust_single, hang = -1, labels = primate.scapulae$classdigit)
table(cutree(heri_clust_single, k=5), primate.scapulae$classdigit)
confusionMatrix(as.factor(cutree(heri_clust_single, k=5)), as.factor(primate.scapulae$classdigit))


# Average hierarchical clustering 
heri_clust_avg <- hclust(dist, method = "ave")
plot(heri_clust_avg, hang = -1, labels = primate.scapulae$classdigit)
table(cutree(heri_clust_avg, k=5), primate.scapulae$classdigit)
confusionMatrix(as.factor(cutree(heri_clust_avg, k=5)), as.factor(primate.scapulae$classdigit))


# Complete hierarchical clustering 
heri_clust_compl <- hclust(dist, method = "complete")
table(cutree(heri_clust_compl, k=5), primate.scapulae$classdigit)
confusionMatrix(as.factor(cutree(heri_clust_compl, k=5)), as.factor(primate.scapulae$classdigit))


# partb 3 question

kmediods <- pamk(new_data,5)

# optimal value of k
kmediods$nc

# results in tabular form 
table(kmediods$pamobject$clustering, primate.scapulae$classdigit)

# plot the results for k= 2
layout(matrix(c(1,2), 1, 2))
plot(kmediods$pamobject)

confusionMatrix(as.factor(kmediods$pamobject$clustering), as.factor(primate.scapulae$classdigit))


