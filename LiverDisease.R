if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# import needed libraries
library(tidyverse)
library(dplyr)
library(tidyr)
library(caret)
library(readxl)
library(ggplot2)

#Import file
liver_patient <- read_excel("liver_patient.xls")
#Check the structure of the file
head(liver_patient)
str(liver_patient)

#Change names of columns for shorter ones and more usable
names(liver_patient) = c("age", "gender", "total_bil","direct_bil","aap","salam","sasam","total_prot","alb","ag_ratio", "result")
head(liver_patient)

#Check if there some NA
sum(is.na(liver_patient))

# Drop rows with NA values
liver_data <- drop_na(liver_patient)

# Check if in the data no NA values left
sum(is.na(liver_data))

#Check the structure of the data without NA values
str(liver_data)

#Plot numbers of patients with disease and without
liver_data %>% ggplot(aes(result)) + geom_bar() + ggtitle(" Number of patients with disease and without")

# Plot age distribution in both groups
liver_data  %>% ggplot(aes(result, age, group = result)) + geom_point()+
  geom_boxplot() + ggtitle("Age distribution")

# Calculate numbers of males and females in both groups (with disease and without)  
gender_data <- liver_data  %>% group_by(gender) %>% summarise(liv_pat = sum(result =="1"), non_liv_pat = sum(result =="2"))
gender_data

# Make data tidy for visualization 
plot_gender <- gender_data %>% gather(key = result,value = number, 2:3)
plot_gender

# Plot numbers of males and females in both groups
plot_gender %>% ggplot(aes(x = result, y = number)) + geom_col(aes(fill = gender))+
  ggtitle("Numbers of males and females in positive and negative groups")

# Plot 8 combined plots(boxplots) to visualize blood results
facet_liver <- gather(liver_data, key = "measure", value = "value", 
                      c("total_bil", "direct_bil", "aap", "salam", "sasam",
                        "total_prot", "alb", "ag_ratio"))
ggplot(facet_liver, aes( x = result, y = value, group = result)) + geom_boxplot()+
  facet_wrap(~ measure, scales = "free_y") + ggtitle("Blood test results in two groups")

# Splitting data for final test set for testing the final model(10% of liver_data set) 
# and practice set for training different algorithms
set.seed(4, sample.kind="Rounding") # if using R 3.6 or later

test_index <- createDataPartition(y = liver_data$result, times = 1, p = 0.1, list = FALSE)
practice_set <- liver_data[-test_index,]
final_test <- liver_data[test_index,]

# Create train and test set from practice set
set.seed(6, sample.kind="Rounding") # if using R 3.6 or later
test_index <- createDataPartition(y = practice_set$result, times = 1,
                                  p = 0.2, list = FALSE)
train_set <- practice_set[-test_index,]
test_set <- practice_set[test_index,]


# Classification tree model
train_rpart <- train(as.factor(result)~.,method = "rpart", data = train_set)
plot(train_rpart)

rpart_model <- confusionMatrix(predict(train_rpart, test_set),
                               as.factor(test_set$result))

#Make tibble with model results
model_result <- tibble(Model = "Classification tree",
                       Accuracy = rpart_model$overall["Accuracy"],
                       Sensitivity = rpart_model$byClass["Sensitivity"],
                       Specificity = rpart_model$byClass["Specificity"])
model_result


# Classification tree model with tuning
train_rpart_t <- train(as.factor(result)~.,method = "rpart",
                       tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 20)),
                       data = train_set)
plot(train_rpart_t)

tuned_rpart_model <- confusionMatrix(predict(train_rpart_t, test_set), as.factor(test_set$result))

#Adding to tibble tuned classification tree model results

model_result <- bind_rows(model_result,tibble(Model = "Classification tree with tuning",
                       Accuracy = tuned_rpart_model$overall["Accuracy"],
                       Sensitivity = tuned_rpart_model$byClass["Sensitivity"],
                       Specificity = tuned_rpart_model$byClass["Specificity"]))
model_result

# Generalized linear model
train_glm <- train(as.factor(result)~., method = "glm", data = train_set)
glm_model <- confusionMatrix(predict(train_glm, test_set), as.factor(test_set$result))

#Adding to tibble Generalized linear model results

model_result <- bind_rows(model_result,tibble(Model = "Generalized linear model",
                                              Accuracy = glm_model$overall["Accuracy"],
                                              Sensitivity = glm_model$byClass["Sensitivity"],
                                              Specificity = glm_model$byClass["Specificity"]))
model_result

#k-nearest neighbors model
train_knn <- train(as.factor(result)~.,method = "knn", data = train_set)
knn_model <- confusionMatrix(predict(train_knn, test_set), as.factor(test_set$result))

#Adding to tibble k-nearest neighbors model results

model_result <- bind_rows(model_result,tibble(Model = "k-nearest neighbors model",
                                              Accuracy = knn_model$overall["Accuracy"],
                                              Sensitivity = knn_model$byClass["Sensitivity"],
                                              Specificity = knn_model$byClass["Specificity"]))
model_result

#k-nearest neighbors model with trainControl
control <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn_cv <- train(as.factor(result)~., method = "knn", data = train_set,
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)
knn_cv_model <- confusionMatrix(predict(train_knn_cv, test_set), as.factor(test_set$result))

#Adding to tibble k-nearest neighbors model with trainControl results

model_result <- bind_rows(model_result,tibble(Model = "k-nearest neighbors model with trainControl",
                                              Accuracy = knn_cv_model$overall["Accuracy"],
                                              Sensitivity = knn_cv_model$byClass["Sensitivity"],
                                              Specificity = knn_cv_model$byClass["Specificity"]))
model_result


#Random forest model
set.seed(6, sample.kind = "Rounding")
train_rf <- train(as.factor(result)~., method = "rf", data = train_set)
rf_model <- confusionMatrix(predict(train_rf, test_set), as.factor(test_set$result))

#Adding to tibble random forest model results

model_result <- bind_rows(model_result,tibble(Model = "Random forest",
                                              Accuracy = rf_model$overall["Accuracy"],
                                              Sensitivity = rf_model$byClass["Sensitivity"],
                                              Specificity = rf_model$byClass["Specificity"]))
model_result

#Trying random forest model on the final test set
final_results <- confusionMatrix(predict(train_rf, final_test), as.factor(final_test$result))

#Adding to tibble final test results

model_result <- bind_rows(model_result,tibble(Model = "Final test",
                                              Accuracy = final_results$overall["Accuracy"],
                                              Sensitivity = final_results$byClass["Sensitivity"],
                                              Specificity = final_results$byClass["Specificity"]))
model_result
