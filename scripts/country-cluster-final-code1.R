setwd("~/Documents/Project/global-terrorism/data")

# project_data <- read.csv("data_sci.csv", stringsAsFactors = FALSE)
# project_data <- read.csv("data_sci.csv", stringsAsFactors = FALSE)
# load data
load("/Users/yuanxiang/Documents/Project/global-terrorism/data/globalterror.RData")
project_data <- globalterror

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

# Convert output to dataframe and rename the output
Afghanistan.carto <- as.data.frame(Afghanistan.carto)
India.carto <- as.data.frame(India.carto)
Pakistan.carto <- as.data.frame(Pakistan.carto)
Iraq.carto <- as.data.frame(Iraq.carto)
carto.data <- as.data.frame(carto.data)

colnames(Afghanistan.carto) <- c("longitude","latitude","k3","k4","k5","k6")
colnames(India.carto) <- c("longitude","latitude","k3","k4","k5","k6")
colnames(Iraq.carto) <- c("longitude","latitude","k3","k4","k5","k6")
colnames(Pakistan.carto) <- c("longitude","latitude","k3","k4","k5","k6")
colnames(carto.data) <- c("longitude","latitude","k3","k4","k5","k6")

# append groups to the country dataframe(k = 3)
Afghanistan.group <- cbind(Afghanistan[5:9], Afghanistan.carto$k3)
India.group <- cbind(India[5:9], India.carto$k3)
Pakistan.group <- cbind(Pakistan[5:9], Pakistan.carto$k3)
Iraq.group <- cbind(Iraq[5:9], Iraq.carto$k3)

# rename cluster column
colnames(Afghanistan.group)[ncol(Afghanistan.group)] <- c("group")
colnames(India.group)[ncol(India.group)] <- c("group")
colnames(Pakistan.group)[ncol(Pakistan.group)] <- c("group")
colnames(Iraq.group)[ncol(Iraq.group)] <- c("group")

# Summary of clusters
# Create mode function
Mode <- function(x){
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Get mode features for each clusters
overview.Iraq <- aggregate(Iraq.group, list(Iraq.group$group), Mode)
overview.India <- aggregate(India.group, list(India.group$group), Mode)
overview.Pakistan <- aggregate(Pakistan.group, list(Pakistan.group$group), Mode)
overview.Afghanistan <- aggregate(Afghanistan.group, list(Afghanistan.group$group), Mode)


# write cluster results with locations as csv
write.csv(Afghanistan.carto,"Afghanistan.csv")
write.csv(India.carto,"India.csv")
write.csv(Pakistan.carto,"Pakistan.csv")
write.csv(Iraq.carto,"Iraq.csv")

# use pvclust
# library(pvclust)
# result.iraq <- pvclust(Iraq[, 5:9], method.dist="cor", method.hclust="average", nboot=1000)
# plot(result.iraq)
# pvrect(result.iraq, alpha = 0.90)
# 
# result.pak <- pvclust(Pakistan[, 5:9], method.dist="cor", method.hclust="average", nboot=1000)
# plot(result.pak)
# pvrect(result.pak, alpha = 0.90)
# 
# result.afg <- pvclust(Afghanistan[, 5:9], method.dist="cor", method.hclust="average", nboot=1000)
# plot(result.afg)
# pvrect(result.afg, alpha = 0.90)
# 
# result.india <- pvclust(India[, 5:9], method.dist="cor", method.hclust="average", nboot=1000)
# plot(result.india)
# pvrect(result.india, alpha = 0.90)


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






