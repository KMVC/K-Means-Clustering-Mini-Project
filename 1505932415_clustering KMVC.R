# This mini-project is based on the K-Means exercise from 'R in Action'
# Go here for the original blog post and solutions
# http://www.r-bloggers.com/k-means-clustering-from-r-in-action/

# Exercise 0: Install these packages if you don't have them already

# install.packages(c("cluster", "rattle.data","NbClust"))
library(cluster)
library(rattle.data)
library(NbClust)
# Now load the data and look at the first few rows
data(wine, package="rattle.data")
head(wine)

# Exercise 1: Remove the first column from the data and scale
# it using the scale() function
wine <- scale(wine[,-1])


# Now we'd like to cluster the data using K-Means. 
# How do we decide how many clusters to use if you don't know that already?
# We'll try two methods.

# Method 1: A plot of the total within-groups sums of squares against the 
# number of clusters in a K-means solution can be helpful. A bend in the 
# graph can suggest the appropriate number of clusters. 

wssplot <- function(data, nc=15, seed=1234){
	              wss <- (nrow(data)-1)*sum(apply(data,2,var))
               	      for (i in 2:nc){
		        set.seed(seed)
	                wss[i] <- sum(kmeans(data, centers=i)$withinss)}
	                
		      plot(1:nc, wss, type="b", xlab="Number of Clusters",
	                        ylab="Within groups sum of squares")
	   }

wssplot(wine)


# Exercise 2:
#   * How many clusters does this method suggest?
#3, as that is where the graph bends

#   * Why does this method work? What's the intuition behind it?
#the method works because it provides a way to show where the addition of clusters has marginal returns (you don't want
#to add more clusters than necessary), the bend in the curve shows where that drop in returns happens

#   * Look at the code for wssplot() and figure out how it works
#the code for wssplot has several components: it creates a function, using nc to specify the max. # of clusters, 
#and using 1234 as a random number seed. The function works as a for loop, first creating a variable
#wss which is 1 subtracted from the total number of rows in the data set, and then that value should be multipled
#with the sum of variances of the second column of the data 
# the for loop specifies that the index should begin at row 2 and go till nc (which was specified as 15)
#that the seed should be set (again specified earlier as 1234)
#that the changing indexes of i will be the sum of the kmeans clustering on the data, with the number of clusters(centers)
#set to i, for the vector within-cluster sum of squares
#the plot function states to plot the the number of clusters, the within-groups sum of squares and what to label the axes
#providing both lines and points (type = "b")


# Method 2: Use the NbClust library, which runs many experiments
# and gives a distribution of potential number of clusters.

library(NbClust)
set.seed(1234)
nc <- NbClust(wine[,-1], min.nc = 2, max.nc = 15, method="kmeans")
barplot(table(nc$Best.n[1,]),
	          xlab="Number of Clusters", ylab="Number of Criteria",
		            main="Number of Clusters Chosen by 26 Criteria")


# Exercise 3: How many clusters does this method suggest?
3

# Exercise 4: Once you've picked the number of clusters, run k-means 
# using this number of clusters. Output the result of calling kmeans()
# into a variable fit.km
k = 3
seed = 1234
# fit.km <- kmeans(wine[,-1], centers = k, iter.max = 1000 )
fit.km <- kmeans(wine[,-1], centers = 3, iter.max = 1000 )
fit.km
# Now we want to evaluate how well this clustering does.

# Exercise 5: using the table() function, show how the clusters in fit.km$clusters
# compares to the actual wine types in wine$Type. Would you consider this a good
# clustering?

#eliminated wine$type before, so need to get it back
data(wine, package="rattle.data")

#tabe of fit.km and wine$Type
table(fit.km$cluster, wine$Type)

#I would consider this to be fairly good clustering, as most of the elements that were grouped 
#together by Type in wine, are clustered together in fit.km (of 59 in group 1 in wine, 59 are in cluster 3 in
#fit.km, of the 71 in group 2 in wine$Type, 58 are in cluster 2, and of the 48 in group 3 in wine$Type, 48
#are in cluster 1, which means that the clusters are still largely grouped together)

# Exercise 6:
# * Visualize these clusters using  function clusplot() from the cluster library
# * Would you consider this a good clustering?)

clusplot(wine, fit.km$cluster, color = TRUE, shade = TRUE)

#this seems to be an okay clustering, the distances between values in each cluster seem not too large, on average.

