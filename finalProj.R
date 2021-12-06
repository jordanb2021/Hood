require(data.table)
library(dplyr)
library(tidyverse)
library(maps)
library(mapdata)
library(factoextra)
library(randomForest)
library(datasets)
library(caret)
data <- fread("Datasets/HeartDisease/merged_hd_income_geo_smoking_obesity_age_data.csv")

tail(data)
print(nrow(data))

map("worldHires",'usa', xlim=c(-125, -66.6), ylim=c(25.1, 49.1), col='gray90', fill=TRUE)

# Plot color coded points for counties
for (row in 1:nrow(data)) {
  range <- data[row, "hd_range"]
  lon <- data[row, "lon"]
  lat <- data[row, "lat"]
  if (grepl("647", range, fixed=TRUE)) {
    points(lon, lat, pch=20, col="green", cex=0.5)
  }
  else if (grepl("641", range, fixed=TRUE)) {
    points(lon, lat, pch=20, col="yellow", cex=0.5)
  }
  else if (grepl("645", range, fixed=TRUE)) {
    points(lon, lat, pch=20, col="orange", cex=0.5)
  }
  else if (grepl("643", range, fixed=TRUE)) {
    points(lon, lat, pch=20, col="red", cex=0.5)
  }
  else if (grepl("644", range, fixed=TRUE)) {
    points(lon, lat, pch=20, col="purple", cex=0.5)
  }
}
# Plot Clusters for the uncommented kvars subset
# Top 2 categories of heart disease death rates
#kvars <- subset(data, (hd_range == "361.0 - 416.0 (643)" | hd_range == "416.1 - 810.5 (644)"), select = c("lat","lon"))
# Bottom income category
#kvars2 <- subset(data, (grepl("(637)", income_range, fixed=TRUE)), select = c("lat","lon"))
# Top smoking rate category
#kvars <- subset(data, (grepl("(618)", smoking_range, fixed=TRUE)), select = c("lat","lon"))
# Top obesity category
#kvars <- subset(data, (grepl("(619)", obesity_range, fixed=TRUE)), select = c("lat","lon"))
# Top over 65 category
kvars <- subset(data, (grepl("(636)", sixtyfiveover_range, fixed=TRUE)), select = c("lat","lon"))

print(kvars)
#is.na(kvars)

set.seed(1234)
kmean <- kmeans(na.omit(kvars), 5, nstart = 15)

paste("Counties in Each Cluster: ", toString(kmean[7]))
print(kmean[2])

print(kmean$centers[1:5])

lon <- as.double(kmean$centers[6:10])
lat <- as.double(kmean$centers[1:5])
results <- data.frame(lat, lon)
map("worldHires",'usa', xlim=c(-125, -66.6), ylim=c(25.1, 49.1), col='gray90', fill=TRUE)
points(results$lon, results$lat, pch=21, col="red", cex=5)

# Add column for boolean highDeathRate
data$highDeathRate <- "false"
for (row in 1:nrow(data)) {
  range <- data[row, "hd_range"]
#  if (grepl("643", range, fixed=TRUE)) {
#    data[row, "highDeathRate"] <- "true"
#  }
   if (grepl("644", range, fixed=TRUE)) {
    data[row, "highDeathRate"] <- "true"
  }
}

# Add column for boolean highObesRate
data$highObesRate <- "false"
for (row in 1:nrow(data)) {
  range <- data[row, "obesity_range"]
  if (grepl("619", range, fixed=TRUE)) {
    data[row, "highObesRate"] <- "true"
  }
}

# Add column for boolean highSmokeRate
data$highSmokeRate <- "false"
for (row in 1:nrow(data)) {
  range <- data[row, "smoking_range"]
  if (grepl("618", range, fixed=TRUE)) {
    data[row, "highSmokeRate"] <- "true"
  }
}

# Add column for boolean highOldRate
data$highOldRate <- "false"
for (row in 1:nrow(data)) {
  range <- data[row, "sixtyfiveover_range"]
  if (grepl("636", range, fixed=TRUE)) {
    data[row, "highOldRate"] <- "true"
  }
}

# Add column for boolean lowIncomeRate
data$lowIncomeRate <- "false"
for (row in 1:nrow(data)) {
  range <- data[row, "income_range"]
  if (grepl("637", range, fixed=TRUE)) {
    data[row, "lowIncomeRate"] <- "true"
  }
}

# Data subsets for each factor to be tested
obes_data <- subset(data, select = c("highObesRate","highDeathRate"))
obes_data$highDeathRate <- as.factor(obes_data$highDeathRate)

income_data <- subset(data, select = c("lowIncomeRate","highDeathRate"))
income_data$highDeathRate <- as.factor(income_data$highDeathRate)

smoke_data <- subset(data, select = c("highSmokeRate","highDeathRate"))
smoke_data$highDeathRate <- as.factor(smoke_data$highDeathRate)

age_data <- subset(data, select = c("highOldRate","highDeathRate"))
age_data$highDeathRate <- as.factor(age_data$highDeathRate)

income_smoke_data <- subset(data, select = c("highSmokeRate","lowIncomeRate","highDeathRate"))
income_smoke_data$highDeathRate <- as.factor(income_smoke_data$highDeathRate)

# Create train and test data 
set.seed(222)
ind <- sample(2, nrow(income_smoke_data), replace = TRUE, prob = c(0.7, 0.3))
train_data <- income_smoke_data[ind==1,]
test_data <- income_smoke_data[ind==2,]

# Random Forest
rf <- randomForest(highDeathRate~., data=train_data, proximity=TRUE) 
print(rf)

# Predict test data classification 
pred1 <- predict(rf, test_data)
confusionMatrix(pred1, test_data$ highDeathRate)

#head(data)
#tail(data)
#test <- data[(data$lowIncomeRate == "true"),]
#print(test)