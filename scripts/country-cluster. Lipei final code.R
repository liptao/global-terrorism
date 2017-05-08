setwd("/Users/Claire/Documents/Data Science project")

# project_data <- read.csv("data_sci.csv", stringsAsFactors = FALSE)
project_data <- read.csv("data_sci.csv", stringsAsFactors = FALSE)

new <- c("eventid","country_txt", "latitude","longitude","attacktype1", "targtype1", "weaptype1","imonth", "suicide","gname","guncertain1","doubtterr" )

tocluster.data <- project_data[new]

# clean data Lipei Version
tocluster.data <- tocluster.data[tocluster.data$guncertain1 == 0 | tocluster.data$doubtterr == 0,] 
tocluster.data <- tocluster.data[tocluster.data$gname != "Unknown",]
tocluster.data <- tocluster.data[!tocluster.data$weaptype1 %in% c(12, 13),]
tocluster.data <- tocluster.data[tocluster.data$attacktype1 != 9, ]
tocluster.data <- tocluster.data[!tocluster.data$targtype1 %in% c(13,20), ]

tocluster.data[, 5:ncol(tocluster.data)] <- lapply(tocluster.data[, 5:ncol(tocluster.data)], as.factor)

#Functions
##HC Object function
hc.object <- function(country, featurelist){
  library(cluster)
  country.gower <- daisy(country[featurelist], metric = "gower")
  country.gmat <- as.matrix(country.gower)
  hc <- hclust(country.gower, method="ward.D")
  return(hc)
}

##Group function
set.groups <- function(hc.object, klusters){
  library(cluster)
  groups <- cutree(hc.object, k = klusters)
  return(groups)
}

##Maps function
draw.map <- function(country, groups){
  library(maps)
  country.map <- map("world", region = country, interior=FALSE)
  country.map <- map("world",boundary=FALSE,col="black",add=TRUE)
  country.map <- points(eval(parse(text=country))$longitude, eval(parse(text=country))$latitude, pch = 19, cex=0.3,col=factor(groups))
  legend(x="topleft", bty = "n", legend=levels(factor(groups)), 
         cex = 1,  x.intersp=0, xjust=0, yjust=0, text.col=seq_along(levels(factor(groups))))
  return(country.map)
}

# Create country data frames
Iraq <- tocluster.data[tocluster.data$country_txt=="Iraq",]
Pakistan <- tocluster.data[tocluster.data$country_txt=="Pakistan",]
Afghanistan <- tocluster.data[tocluster.data$country_txt=="Afghanistan",]
India <- tocluster.data[tocluster.data$country_txt=="India",]
feature.list <- c("attacktype1", "targtype1", "weaptype1","imonth", "suicide")

#Loop
country.list <- c("Iraq", "Pakistan", "Afghanistan", "India")
k.list <- c(3:6)

for(i in country.list){
  df.hc <- hc.object(eval(parse(text=i)), feature.list)
  print(paste(i))
  assign(paste0(i,".hc"), df.hc)
  df.plot <- plot(eval(parse(text = paste0(i,".hc"))))
  dev.print(pdf, paste0(i,".hc",".pdf"))
  assign(paste0(i,".plot"), df.plot)
  carto.data <- cbind(eval(parse(text=i))$longitude, eval(parse(text=i))$latitude)
  for(j in k.list){
    df.k <- set.groups(eval(parse(text = paste0(i,".hc"))), j)
    assign(paste0(i,".k", j), df.k)
    draw.map(i, eval(parse(text = paste0(i,".k", j))))
    dev.print(pdf, paste0(i,".map", ".k",j,".pdf"))
    carto.data <- cbind(carto.data, eval(parse(text = paste0(i,".k", j))))
  }
  assign(paste0(i, ".carto"), carto.data)
}

# Misc
# Iraq.hc <- hc.object(Iraq, feature.list)
# plot(Iraq.hc)
# Iraq.k5 <- set.groups(Iraq.hc, 5)
# # 
# # Iraq.map <- map("world", region = "Iraq", interior=FALSE)
# # Iraq.map <- map("world",boundary=FALSE,col="black",add=TRUE)
# # Iraq.map <- points(Iraq$longitude, Iraq$latitude, pch = 19, cex=0.5,col=factor(Iraq.k5))
# 
Iraq.map.k5 <- draw.map("Iraq", Iraq.k5)




