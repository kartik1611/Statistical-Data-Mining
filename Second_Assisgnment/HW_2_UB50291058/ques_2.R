rm(list = ls(all=T))
setwd("F:\\buffalo\\R\\sdma spring\\r\\1st_assignment_sdma\\Second_Assisgnment")

#a) loading data

data <- read.csv('Ch10Ex11.csv',header =TRUE)

#b) Apply hierarchical clustering to the samples using correlation-based distance
comp_model <- hclust(as.dist(1 - cor(data)), method = "complete")
plot(comp_model)


single_clus_model = hclust(as.dist(1 - cor(data)), method = "single")
plot(single_clus_model)

avg_clus_model = hclust(as.dist(1 - cor(data)),method = "average")
plot(avg_clus_model)


#c) using pca

data_transpose = t(data)
pca_data= prcomp(data_transpose)
head(pca_data$rotation)

load_overall = apply(pca_data$rotation, 1, sum)
row= order(abs(load_overall), decreasing = TRUE)
row[1:10]
