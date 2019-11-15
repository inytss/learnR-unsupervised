# load library ---
library(dplyr)
library(factoextra)
library(FactoMineR)

# load data ---
property <- read.csv("data_input/nyc.csv", stringsAsFactors = F)

# inspect data structure ---
str(property)

# data cleansing ---
ppt <- property %>% 
  select(-c(X, BOROUGH, BLOCK, LOT, ZIP.CODE)) %>% 
  mutate(LAND.SQUARE.FEET = as.integer(LAND.SQUARE.FEET),
         GROSS.SQUARE.FEET = as.integer(GROSS.SQUARE.FEET),
         SALE.PRICE = as.integer(SALE.PRICE)) %>% 
  select_if(is.integer) %>% 
  filter(complete.cases(.))

head(ppt)

# data pre-process ---
ppt_z <- scale(ppt)

# find eigen ---
eigen_ppt <- eigen(cov(ppt_z))

# eigen value
eigen_ppt$values

# eigen vector
eigen_ppt$vectors

# principal component analysis ---
ppt.pca <- prcomp(ppt, scale = TRUE)

summary(ppt.pca)

# visualize pca ---
ppt.small <- ppt[1:300,]

biplot(prcomp(ppt.small, scale = T), cex = 0.5)

# subset ppt.small
ppt.small <- ppt[1:300,]
ppt.small_pca <- prcomp(ppt.small, scale = T)
ppt.small_pca

# K-Means Clustering --- 
# load data
whiskies <- read.csv("data_input/whiskies.txt") %>% 
  select(-c(RowID, Postcode, Latitude, Longitude)) 

head(whiskies)
# rownames(whiskies) <- whiskies[,1]
# whiskies <- whiskies %>%
#   select(-Distillery)

str(whiskies)

# scale data
whiskies.z <- whiskies %>% 
  mutate_if(is.integer, scale)

# elbow plot
set.seed(100)
wss <- function(data, maxCluster = 9) {
  # Initialize within sum of squares
  SSw <- (nrow(data) - 1) * sum(apply(data, 2, var))
  SSw <- vector()
  for (i in 2:maxCluster) {
    SSw[i] <- sum(kmeans(data, centers = i)$withinss)
  }
  plot(1:maxCluster, SSw, type = "o", xlab = "Number of Clusters", ylab = "Within groups sum of squares", pch=19)
}

wss(whiskies.z)

# kmeans
set.seed(100)
whis.km <- kmeans(whiskies.z, k = 5)

# k-means and pca
whi.pca <- PCA(whiskies.z, graph = F)

# get cluster 
whiskies$cluster <- as.factor(whis.km$cluster)

# visualize 
plot(whi.pca, choix=c("ind"), label="none", col.ind=whiskies$cluster)
legend("topright", levels(whiskies$cluster), pch=19, col=1:5)
