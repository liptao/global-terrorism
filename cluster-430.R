setwd("/Users/yuanxiang/Documents/Project/global-terrorism/data")

# project_data <- read.csv("data_sci.csv", stringsAsFactors = FALSE)
load("/Users/yuanxiang/Documents/Project/global-terrorism/data/globalterror12_15.RData")
project_data <- globalterror

new <- c("eventid","country_txt", "latitude","longitude","attacktype1", "targtype1", "targsubtype1", 
         "region", "weaptype1","weapsubtype1","imonth",
          "suicide","gname","claimed","guncertain1" )

tocluster.data <- project_data[new]

# clean data
tocluster.data <- tocluster.data[tocluster.data$gname != "Unknown", ]
tocluster.data[, 5:ncol(tocluster.data)] <- lapply(tocluster.data[, 5:ncol(tocluster.data)], as.factor)

# take random sample first to simulate the result
sample_data <- tocluster.data[sample(1:nrow(tocluster.data), 5000 , replace = TRUE),]


# # Hierarchical Cluster using gower.distance
# library(cluster)
# gower.data <- sample_data
# gowerd <- daisy(gower.data[,5:ncol(sample_data)-1], metric = "gower")
# gmat <- as.matrix(gowerd)
# hc <- hclust(gowerd, method="ward.D")
# plot(hc)
# 
# gower.data[
#   which(gmat == max(gmat[gmat != min(gmat)]),
#         arr.ind = TRUE)[1, ], ]
# rect.hclust(hc, k=5, border="red")
# groups <- cutree(hc, k=5)
# rect.hclust(hc, k=5, border="red")
# library(maps)
# map("world",interior=FALSE)
# map("world",boundary=FALSE,col="gray",add=TRUE)
# 
# x_cord <- xlim = c(25,91)
# y_cord <- ylim = c(1,38)
# 
# points(gower.data$longitude,gower.data$latitude,cex=0.1,col=factor(groups))
# legend(x="topleft", bty = "n", legend=levels(factor(groups)), 
#        cex = 1,  x.intersp=0, xjust=0, yjust=0, text.col=seq_along(levels(factor(groups))))
# 
# gower.data$groups <- groups
# recap <- sumUp(gower.data[,5:ncol(gower.data)], groups, 5)
# 
# # customized function
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
#Custom Function to summarize
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
  overview <- aggregate(data, list(clusters), Mode)
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



# hclust using binary method
new.binary <- c("eventid","country_txt", "latitude","longitude","attacktype1", "targtype1", "targsubtype1", 
         "region", "weaptype1","weapsubtype1","imonth",
         "suicide","gname","claimed","guncertain1" )

tocluster.data.binary <- project_data[project_data$guncertain1 == 0 & project_data$doubtterr == 0,new.binary]

# clean data
tocluster.data.binary <- tocluster.data.binary[tocluster.data.binary$country_txt == "Iraq", ]
  
# tocluster.data.binary <- tocluster.data.binary[tocluster.data$gname != "Unknown", ]
tocluster.data.binary[, 5:ncol(tocluster.data.binary)] <- lapply(tocluster.data.binary[, 5:ncol(tocluster.data.binary)], as.factor)

# take random sample first to simulate the result
set.seed(12)
# sample_data <- tocluster.data.binary[sample(1:nrow(tocluster.data.binary), 5000 , replace = TRUE),]
sample_data <- tocluster.data.binary
  
library(tidyr)

colnames(sample_data)[c(5,6,9,10)] <- c("attacktype", "targtype", "weaptype", "weaponsubtype")
colnames(sample_data)[c(5,6,9,10)] <- c("attacktype", "targtype", "weaptype", "weaponsubtype")

data_wide <- cbind(model.matrix(~ 0 + attacktype, sample_data),model.matrix(~ 0 + targtype, sample_data),
                   model.matrix(~ 0 + weaptype, sample_data), model.matrix(~ 0 + imonth, sample_data), sample_data$suicide)

data_wide[, ncol(data_wide)] <- ifelse(data_wide[, ncol(data_wide)]== 1, 0, 1)
colnames(data_wide)[ncol(data_wide)] <- c("suicide")

# create distance matrix
dist.mat <- dist(data_wide, method = "binary")

hclust.res <- hclust(dist.mat, method="ward.D")
plot(cut(as.dendrogram(hclust.res), h = 0)$upper, leaflab = "none")


cl <- cutree(hclust.res, k = 3)
cl_1 <- subset(cl, cl==1)
cl_2 <- subset(cl, cl==2)
cl_3 <- subset(cl, cl==3)


coords <- state.data[, c("x", "y")]
assignColor <- function(cl.idx){
  col.codes <- c("#FF8000", "#0080FF", "#FFBF00", "#FF4000","#FF4234")
  return(col.codes[cl.idx])
}

southasia_x <- c(60, 120)
southasia_y <- c(5,38)
  
map("world", regions = "Iraq", interior = FALSE)
map("world", regions = "Iraq", boundary = FALSE, col = "gray", add = TRUE)
points(sample_data$longitude, sample_data$latitude, cex=0.2, pch=20, col=unique(sample_data$attacktype))


