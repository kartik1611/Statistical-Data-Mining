#10a
set.seed(123)
heri_comp <- hclust(dist(USArrests), method = "complete")
plot(heri_comp)

#10b
cutree(heri_comp, 3)
table(cutree(heri_comp, 3), cutree(heri_comp, 3))

#10c
scaled_data <- scale(USArrests)
heri_comp_scaled <- hclust(dist(scaled_data), method = "complete")
plot(heri_comp_scaled)

#10d
cutree(heri_comp_scaled, 3)
table(cutree(heri_comp_scaled, 3), cutree(heri_comp_scaled, 3))

