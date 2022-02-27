#install necessary packages
install.packages("readxl")
install.packages("tidyverse")
install.packages("gt")
install.packages("cluster")
install.packages("factoextra")
#load packages
library(readxl)
library(tidyverse)
library(gt)
library(cluster)
library(factoextra)
#importing the dataset from excel file
Alum_survey <- read_excel(file.choose())
#exploring the dataset
names(Alum_survey)
summary(Alum_survey)

#Performing segmentation
#Scaling different features
Alum_survey_scaled <- scale(Alum_survey)
## Ward Hierarchical Clustering
# calculate distance matrix with euclidian distance
dis <- dist(Alum_survey_scaled, method = "euclidean")
#clustering algorithm
fit <- hclust(dis, method="ward.D2")
# display dendrogram
plot(fit)
cluster <- cutree(fit, k=4)
#explore clusters
cluster
table(cluster)
# draw dendogram with red borders around the 4 clusters
rect.hclust(fit, k=4, border="red")
# Compute k-means with k = 4
set.seed(123)
res.km <- kmeans(Alum_survey_scaled, 4, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster
fviz_cluster(res.km, data = Alum_survey_scaled,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800","#7E3517"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)
# Compute and plot wss for k = 2 to k = 15.
k.max <- 12
data <- Alum_survey_scaled
wss <- sapply(1:k.max, 
              function(k){kmeans(data, k, nstart=50,iter.max = 15 )$tot.withinss})
wss
plot(1:k.max, wss,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#add clusters obtained from dendogram to original data set
df_final <- cbind(Alum_survey, cluster)
names(df_final)
View(df_final)
##Description step
#calculate segment size in percentages
proportions <- table(df_final$cluster)/length(df_final$cluster)
percentages <- proportions*100
percentages
#Exploring mean values in the cluster
segments <- df_final %>%
  group_by(cluster) %>%
  summarise_at(vars(ConstCom,TimelyInf,TaskMgm,DeviceSt,Wellness,Athlete,Style,AmznP,Female,Degree,Income,Age),
               list(M = mean))
segments
#Create simple table with mean values
segments %>%
  gt() %>%
  tab_header(
    title = md("Mean Values for Clusters"))

#Elbow Method for finding the optimal number of clusters
set.seed(123)


