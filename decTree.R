require(data.table)
library(dplyr)
library(tidyverse)
library(rpart)
library(rpart.plot)

data <- fread("Traffic_Violations.csv", select = c("Driver State", "Violation Type", "Make", "SubAgency", "Model", "Race", "Gender", "Color"))
names(data)[names(data) == 'Violation Type'] <- 'ViolationType'
names(data)[names(data) == 'Driver State'] <- 'DriverState'
names(data)[names(data) == 'Work Zone'] <- 'WorkZone'
#names(data)[names(data) == 'Time Of Stop'] <- 'TimeOfStop'

data <- data[!(data$ViolationType == "SERO" | data$ViolationType == "ESERO"),]

set.seed(678)

shuffle_index <- sample(1:nrow(data))
data <- data[shuffle_index, ]
head(data)

create_train_data <- function(data, size = 0.8, train = TRUE) {
    n_row = nrow(data)
    total_row = size * n_row
    train_sample <- 1: total_row
    if (train == TRUE) {
        return (data[train_sample, ])
    } else {
        return (data[-train_sample, ])
    }
}

data_train <- create_train_data(data, 0.8, train = TRUE)
data_test <- create_train_data(data, 0.8, train = FALSE)
dim(data_train)

data_train <- na.omit(data_train)
data_test <- na.omit(data_test)

data_train <- data_train %>%
  add_column(Ticket = if_else(.$ViolationType == "Citation", TRUE, FALSE))

data_test <- data_test %>%
  add_column(Ticket = if_else(.$ViolationType == "Citation", TRUE, FALSE))

data_test <- data_test[!(data_test$DriverState == "GU" | data_test$DriverState == ""),]

head(data_train)

prop.table(table(data_test$ViolationType))

data_train <- mutate_if(data_train, is.character, as.factor)
data_train <- mutate_if(data_train, is.factor, fct_lump, n = 10, other_level = "OTHER", ties.method = "first")
data_train$Ticket <- factor(data_train$Ticket)
data_test <- mutate_if(data_test, is.character, as.factor)
data_test <- mutate_if(data_test, is.factor, fct_lump, n = 10, other_level = "OTHER", ties.method = "first")
data_test$Ticket <- factor(data_test$Ticket)
str(data_train)
#method = 'class'

print(data_train)
fit <- rpart(Ticket ~ Make + Model + Color, data = data_train, method = 'class', minsplit=1, minbucket=1, cp=0.001)
rpart.plot(fit,type = 1,extra = 106)

#type = 'class'
predict_result <- predict(fit, data_test, type = 'class')
table_tick <- table(data_test$Ticket, predict_result)
table_tick

accuracy_Test <- sum(diag(table_tick)) / sum(table_tick)
print(paste('Accuracy of predict: ', accuracy_Test))
