# ques 4
library(kohonen)
library(caret)

setwd("F://buffalo//R//sdma spring//r//1st_assignment_sdma//Second_Assisgnment")
cancer_data <- read.csv('cancer_data.csv',header =TRUE)


names(cancer_data) <- c("Sample code number", "Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape",
                       "Marginal Adhesion ","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin",
                       "Normal Nucleoli","Mitoses","Class")

str(cancer_data)

# converting to numeric 
cancer_data <- as.numeric(unlist(cancer_data))

# scaling the data 
cancer_data_scaled <- scale(cancer_data)


# som_map_grid

som_map_grid <- somgrid(xdim = 5, ydim = 5, topo = "hexagonal")
cancer_data_som <- som(cancer_data_scaled, grid = som_map_grid, rlen = 2000)

# codes 
som_codes <- cancer_data_som$codes[[1]]


plot(cancer_data_som, type = "changes")


plot(cancer_data_som, type = "count")

# rainbow func

cool_Blue_Hot_Red <- function(n, alpha = 1){rainbow(n, end=4/6, alpha = alpha)[n:1]}

# U matix Plot
plot(cancer_data_som, type = "dist.neighbours", palette.name = cool_Blue_Hot_Red)

# clustering of the codes 
dist<-dist(som_codes )
hc<-hclust(dist(som_codes))
plot(hc)

som_cluster=cutree(hc,h=7)

my_col=c("red","green","blue")
my_bhcol<-my_col[som_cluster]

plot(cancer_data_som,type = "mapping",col="black",bgcol = my_bhcol)
plot

