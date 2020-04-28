library(dplyr)
library(tidyr)
library(readr)
library(plotrix)
library(purrr)
library(cluster)
library(gridExtra)
library(grid)
library(NbClust)
library(factoextra)
set.seed(123)

#Import the data and examine its structure to have a general feel of how the data is 
customer = read.csv('Mall_Customers.csv')

#EPLORATORY DATA ANALYSIS
#Let us explore the data further and see the variations in the columns. It is also a good practise to look 
# at the first and last few rows of the data.
str(customer)
summary(customer)
head(customer)
tail(customer)

# Now take the data exploration further by exploring the Gender column. We will visualize the column using
# barchart and pie chart

a = table(customer$Gender)
barplot(a, main = "Visualizing Gender Distribution",
        xlab = "Gender", ylab = "Gender Count",
        col = rainbow(2), legend = rownames(a))

# It can be observed from the barplot that females are more than male customers in this dataset.
# Let us create a pie chart to see what that will also tell us.

percent = round(a/sum(a)*100)
labs = paste(c('Female', 'Male'), ' ', percent, '%', sep = ' ')
pie3D(a, labels = labs, 
      main = 'Pie Chart showing the distribution of the Males and Females')

# It can be seen from the pie chart that indeed the females are the majority with 56% and the males 44%.

# Now we will also explore the Age column to see its distribution.

hist(customer$Age, col = 'green',
     main = 'Histogram showing the Age distribution of customers',
     xlab = 'Age group', ylab = 'Count',
     labels = T)

#Now we can also visualize the income column to see its distribution
hist(customer$Annual.Income..k.., col = 'red',
     main = 'Graph showing the income distribution of customers',
     xlab = 'Income group', ylab = 'Frequency',
     labels = T)

#Analysing the spending score of customers

summary(customer$Spending.Score..1.100.)
hist(customer$Spending.Score..1.100., col = 'purple',
     main = 'Graph Showing the Spending score',
     xlab = 'Spending core group', ylab = 'Frequency',
     labels = T)

#CLUSTERING
#We will find the optimal number of clusters using 3 methods; Elbow, Silhouette and Gap Statistic Methods

# 1.Using the Elbow Method to determine the optimal number of clusters

#function to calculate the total intra cluster sim of squares
iss = function(k) {
  kmeans(customer[,3:5], k, iter.max = 100, nstart = 100, algorithm = 'Lloyd')$tot.withinss
}

k_values = 1:10

iss_values = map_dbl(k_values, iss)

plot(k_values, iss_values, type = 'b', pch = 19, frame = F,
     xlab = 'Number of Clusters(K)',
     ylab = 'Total intra-cluster sum of squares (iss)')

#from the graph, it can be seen that the plot has a bend at 4, so we can choose that as the number of cluster

# 2. Silhouette Method
#Here, we will try it for different values of k

#k=2
k2 = kmeans(customer[,3:5],2, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s2 = plot(silhouette(k2$cluster, dist(customer[,3:5], 'euclidean')))

#k=3
k3 = kmeans(customer[,3:5],3, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s3 = plot(silhouette(k3$cluster, dist(customer[,3:5], 'euclidean')))

#k=4
k4 = kmeans(customer[,3:5],4, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s4 = plot(silhouette(k4$cluster, dist(customer[,3:5], 'euclidean')))

#k=5
k5 = kmeans(customer[,3:5],5, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s5 = plot(silhouette(k5$cluster, dist(customer[,3:5], 'euclidean')))

#k=6
k6 = kmeans(customer[,3:5],6, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s6 = plot(silhouette(k6$cluster, dist(customer[,3:5], 'euclidean')))

#k=7
k7 = kmeans(customer[,3:5],7, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s7 = plot(silhouette(k7$cluster, dist(customer[,3:5], 'euclidean')))

#k=8
k8 = kmeans(customer[,3:5],8, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s8 = plot(silhouette(k8$cluster, dist(customer[,3:5], 'euclidean')))

#k=6
k9 = kmeans(customer[,3:5],9, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s9 = plot(silhouette(k9$cluster, dist(customer[,3:5], 'euclidean')))

#k=6
k10 = kmeans(customer[,3:5],10, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
s10 = plot(silhouette(k10$cluster, dist(customer[,3:5], 'euclidean')))

#Now we determine the optimal number of clusters
fviz_nbclust(customer[,3:5], kmeans, method = 'silhouette')

#3. Gap Statistic Method

st_gap = clusGap(customer[,3:5], FUN = kmeans, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(st_gap)

#Now we will use k=6 as the optimal cluster as that produce the highest averages silhouetter width=0.45
k6 = kmeans(customer[,3:5],6, iter.max = 100, nstart = 50, algorithm = 'Lloyd')
k6

pc_clust = prcomp(customer[,3:5], scale = F)
summary(pc_clust)

#visualizing the clusters

ggplot(customer, aes(Annual.Income..k.., Spending.Score..1.100.))+
  geom_point(stat = 'identity', aes(color = as.factor(k6$cluster))) +
  scale_color_discrete(name = ' ',
                       breaks = c('1','2','3','4','5','6'),
                       labels = c('Cluster 1', 'Cluster 2', 'Cluster 3', 'Cluster 4', 'Cluster 5', 
                                  'Cluster 6')) +
  ggtitle('Clusters of Mall Customers', subtitle = 'Using K-means Clustering')

#clustering using pca
kcols = function(vec){cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])}

#kmeans clusters
digcluster = k6$cluster; dignm = as.character(digcluster);

plot(pc_clust$x[,1:2], col=kcols(digcluster), pch=19, xlab = 'K-means', ylab = 'Classes')
legend('bottomleft', unique(dignm), fill = unique(kcols(digcluster)))
