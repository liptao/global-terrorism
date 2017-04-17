sumUp <- function(data, clusters, depth = 3, horizontal = FALSE){
  # Summarize cluster variables by most frequent 
  #
  # Args:
  #       data: input data
  #       clusters: vector of cluster labels
  #       depth: top 3 most frequent variables
  #       horizontal: control format of results. FALSE means one cluster per row.
  #
  # Returns:
  #       A data frame of k-number of centroids
  #
  
  #Calculate means, rotate such that features = rows
  overview <- aggregate(data, list(clusters), mean)
  overview <- as.data.frame(cbind(colnames(overview)[2:ncol(overview)], 
                                  t(overview[,2:ncol(overview)])))
  row.names(overview) <- 1:nrow(overview)
  overview[,1] <- gsub("count.","",as.character(overview[,1]))
  
  #Clean up values as numerics
  for(i in 2:ncol(overview)){
    overview[,i] <- round(as.numeric(as.character(overview[,i])),2)
  }
  
  #Get top X features
  depth.temp <- data.frame()
  for(i in 2:ncol(overview)){
    temp <- overview[order(-overview[,i]), ]
    temp <- paste("(",temp[,i], "): ", temp[,1], sep = "")
    temp <- as.data.frame(matrix(temp[1:depth], 
                                 nrow = 1, 
                                 ncol = depth))
    colnames(temp) <- paste0("Rank.", 1:depth)
    depth.temp <- rbind(depth.temp, temp)
  }
  depth.temp <- cbind(data.frame(table(clusters)), depth.temp)
  
  #Rotate?
  if(horizontal == TRUE){
    depth.temp <- t(depth.temp)
  }
  
  return(depth.temp)
}






project <- read.csv("data_sci.csv")
library(maps)
map("world",interior=FALSE)
map("world",exterior=FALSE,col="gray",add=TRUE)


map("world",interior=FALSE,xlim=c(25,91),ylim=c(1,38))
map("world",boundary=FALSE,col="gray",add=TRUE,xlim=c(25,91),ylim=c(1,38))
points(data_sci$longitude,data_sci$latitude,cex=0.2,col="blue")


new <- c("eventid","latitude","longitude","attacktype1")
newdata <- data_sci[new]

library(tidyr)
data_wide <- spread(newdata, attacktype1, attacktype1)

data_wide[,4:12][!is.na(data_wide[,4:12])] <- 1 
data_wide[,4:12][is.na(data_wide[,4:12])] <- 0 

attacklist <- c("assassination","armed_assault","bombing","hijack","barricade","kidnapping","facility","unarmed_assault","unknown")
names(data_wide)[4:12] <- attacklist


data_round <- data.frame(data_wide)

data_round$latitude <- round(data_round$latitude,digits=2)
data_round$longitude <- round(data_round$longitude,digits=2)


data_agg <- aggregate(data_round[,-1],by=list(data_round$latitude,data_round$longitude),FUN=mean,na.rm=TRUE)

data_agg2 <- scale(data_agg[,c(5:ncol(data_agg))], scale = TRUE, center = TRUE)


library(stats)
#Find elbow
master <- data.frame()

for(i in seq(3,30,1)){
  print(i)
  new <- kmeans(data_agg2, i)
  master <- rbind(master,
                  data.frame(k = i,
                             ss = new$betweenss))
}
plot(master) 

#Run K for "optimal"
set.seed(20)
cluster <- kmeans(data_agg2, 10)$cluster

#Graph results
#Set color palette
palette(colorRampPalette(c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a'))(10))

#Graph lat-lons with color coding by cluster

op <- par(mar = rep(0, 4)) 
plot(data_agg$longitude, data_agg$latitude, col = factor(cluster), pch = 15, cex = 0.5, 
     frame.plot=FALSE, yaxt='n', ann=FALSE, xaxt='n')
legend(x="topleft", bty = "n", legend=levels(factor(cluster)), 
       cex = 1,  x.intersp=0, xjust=0, yjust=0, text.col=seq_along(levels(factor(cluster))))
par(op)

#Results
recap <- sumUp(data_agg[,5:ncol(data_agg)], cluster, 5)


