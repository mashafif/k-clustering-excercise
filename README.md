K-clustering problem
================
Afif

Introduction
------------

This mini-project is based on the K-Means exercise from 'R in Action' Go here for the original blog post and solutions <http://www.r-bloggers.com/k-means-clustering-from-r-in-action/>

Exercise 0:
-----------

Install these packages if you don't have them already

``` r
install.packages(c("cluster", "rattle.data","NbClust"))
```

Now load the data and look at the first few rows

``` r
data(wine, package="rattle.data")
head(wine)
```

    ##   Type Alcohol Malic  Ash Alcalinity Magnesium Phenols Flavanoids
    ## 1    1   14.23  1.71 2.43       15.6       127    2.80       3.06
    ## 2    1   13.20  1.78 2.14       11.2       100    2.65       2.76
    ## 3    1   13.16  2.36 2.67       18.6       101    2.80       3.24
    ## 4    1   14.37  1.95 2.50       16.8       113    3.85       3.49
    ## 5    1   13.24  2.59 2.87       21.0       118    2.80       2.69
    ## 6    1   14.20  1.76 2.45       15.2       112    3.27       3.39
    ##   Nonflavanoids Proanthocyanins Color  Hue Dilution Proline
    ## 1          0.28            2.29  5.64 1.04     3.92    1065
    ## 2          0.26            1.28  4.38 1.05     3.40    1050
    ## 3          0.30            2.81  5.68 1.03     3.17    1185
    ## 4          0.24            2.18  7.80 0.86     3.45    1480
    ## 5          0.39            1.82  4.32 1.04     2.93     735
    ## 6          0.34            1.97  6.75 1.05     2.85    1450

Exercise 1:
-----------

Remove the first column from the data and scale it using the scale() function

``` r
df <- scale(wine[-1])
```

Now we'd like to cluster the data using K-Means. How do we decide how many clusters to use if you don't know that already? We'll try two methods.

Method 1: Elbow Method
----------------------

A plot of the total within-groups sums of squares against the number of clusters in a K-means solution can be helpful. A bend in the graph can suggest the appropriate number of clusters.

``` r
wssplot <- function(data, nc=15, seed=1234){
                  wss <- (nrow(data)-1)*sum(apply(data,2,var))
                      for (i in 2:nc){
                set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                    
              plot(1:nc, wss, type="b", xlab="Number of Clusters",
                            ylab="Within groups sum of squares")
       }

wssplot(df)
```

![](k_clustering_files/figure-markdown_github/unnamed-chunk-4-1.png)

Exercise 2:
-----------

*How many clusters does this method suggest?*

**Answer**: 3 to 4 clusters, according to above method.

*Why does this method work? What's the intuition behind it?*

**Answer**: Purpose of this method is to compare the within groups sums of squares of each k-means solution with different number of cluster. "Within group of sum squares" signifies how spread out/how big is the variance of the members of each cluster with its cluster's means or centroid. This means the lower the within group of squares the less spread out the members of the clusters, the better the result is. By plotting the relation between number of clusters and within groups of squares, we can find after how many number of clusters yield the most significance improvement in within group of squares, which in this case specified by a bend in the graph.

Method 2: Use the NbClust library
---------------------------------

NbClust library runs many experiments and gives a distribution of potential number of clusters.

``` r
library(NbClust)
set.seed(1234)
nc <- NbClust(df, min.nc=2, max.nc=15, method="kmeans")
```

![](k_clustering_files/figure-markdown_github/unnamed-chunk-5-1.png)

    ## *** : The Hubert index is a graphical method of determining the number of clusters.
    ##                 In the plot of Hubert index, we seek a significant knee that corresponds to a 
    ##                 significant increase of the value of the measure i.e the significant peak in Hubert
    ##                 index second differences plot. 
    ## 

![](k_clustering_files/figure-markdown_github/unnamed-chunk-5-2.png)

    ## *** : The D index is a graphical method of determining the number of clusters. 
    ##                 In the plot of D index, we seek a significant knee (the significant peak in Dindex
    ##                 second differences plot) that corresponds to a significant increase of the value of
    ##                 the measure. 
    ##  
    ## ******************************************************************* 
    ## * Among all indices:                                                
    ## * 4 proposed 2 as the best number of clusters 
    ## * 15 proposed 3 as the best number of clusters 
    ## * 1 proposed 10 as the best number of clusters 
    ## * 1 proposed 12 as the best number of clusters 
    ## * 1 proposed 14 as the best number of clusters 
    ## * 1 proposed 15 as the best number of clusters 
    ## 
    ##                    ***** Conclusion *****                            
    ##  
    ## * According to the majority rule, the best number of clusters is  3 
    ##  
    ##  
    ## *******************************************************************

``` r
barplot(table(nc$Best.n[1,]),
              xlab="Numer of Clusters", ylab="Number of Criteria",
                    main="Number of Clusters Chosen by 26 Criteria")
```

![](k_clustering_files/figure-markdown_github/unnamed-chunk-5-3.png)

Exercise 3:
-----------

*How many clusters does this method suggest?*

**Answer**: 3 clusters

Exercise 4:
-----------

Once you've picked the number of clusters, run k-means using this number of clusters. Output the result of calling kmeans() into a variable fit.km

``` r
set.seed(1234)
fit.km <- kmeans(df, 3, nstart=25)
fit.km$cluster
```

    ##   [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
    ##  [36] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 3 2 2 2 2 2 2 2 2
    ##  [71] 2 2 2 1 2 2 2 2 2 2 2 2 2 3 2 2 2 2 2 2 2 2 2 2 2 1 2 2 2 2 2 2 2 2 2
    ## [106] 2 2 2 2 2 2 2 2 2 2 2 2 2 3 2 2 1 2 2 2 2 2 2 2 2 3 3 3 3 3 3 3 3 3 3
    ## [141] 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3
    ## [176] 3 3 3

Now we want to evaluate how well this clustering does.

Exercise 5:
-----------

Using the table() function, show how the clusters in comparison to the actual wine types in wine$Type. Would you consider this a good clustering?

``` r
table(fit.km$cluster, wine$Type)
```

    ##    
    ##      1  2  3
    ##   1 59  3  0
    ##   2  0 65  0
    ##   3  0  3 48

From above result I would consider this as a good clustering due to number of error which is only 6 errors.

Exercise 6:
-----------

Visualize these clusters using function clusplot() from the cluster library Would you consider this a good clustering?

``` r
library(cluster)
clusplot(wine[-1], fit.km$cluster, main="Cluster Plot for Wine Data")
```

![](k_clustering_files/figure-markdown_github/unnamed-chunk-8-1.png)

According to above clusplot, I would also consider this as good modelling, due to clear separation between each clusters with only a very minor overlap.
