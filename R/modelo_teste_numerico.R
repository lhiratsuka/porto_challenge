library(tidyverse)
library(caret)
library(mice)

train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
meta <- read.csv("data/metadata.csv")
set.seed(231192)

train$y <- factor(train$y)


# CV ----
myFolds <- createFolds(train$y, k = 5)
trControl <- trainControl(
  method = "cv",
  number = 5,
  index = myFolds
)

# Ranger baseline ----
model_ranger <- train(y ~.,
                      data = train %>% select(-id),
                      method = "ranger",
                      metric = "Accuracy",
                      importance = 'permutation',
                      trControl = trControl,
                      tuneLength = 3)


mean(model_ranger$resample$Accuracy)

# Bossting baseline
model_xgb <- train(y ~.,
                      data = train %>% select(-id),
                      method = "xgbTree",
                      metric = "Accuracy",,
                      trControl = trControl,
                      tuneLength = 3)


mean(model_xgb$resample$Accuracy)


# Submit
submit <- test %>% 
  mutate(predicted = predict(model_xgb,test)) %>% 
  select(id,predicted)

head(submit)
write.csv(submit, "output/xgb_numerico.csv", row.names = FALSE)


# Imputando missing -----
train <- read.csv("data/train.csv")
test <- read.csv("data/test.csv")
train <- train %>% mutate_all(~ifelse(.x == -999, NA_real_, .x))
test <- test %>% mutate_all(~ifelse(.x == -999, NA_real_, .x))

imputed_train <- mice(train, m=5, maxit = 50, method = 'pmm', seed = 500)
imputed_test <- mice(train, m=5, maxit = 50, method = 'pmm', seed = 500)
summary(imputed_train)

train_complete <- complete(imputed_train,2)
test_complete <- complete(imputed_test,2)

# Ranger + Mice ----
model_ranger <- train(factor(y) ~.,
                      data = train_complete %>% select(-id),
                      method = "ranger",
                      metric = "Accuracy",
                      importance = 'permutation',
                      trControl = trControl,
                      tuneLength = 3)


mean(model_ranger$resample$Accuracy)

# Submit
submit <- test_complete %>% 
  mutate(predicted = predict(model_ranger,test_complete)) %>% 
  select(id,predicted)

head(submit)
write.csv(submit, "output/ranger_numerico_imputed.csv", row.names = FALSE)


# Ranger com factor de baixo level ----

prep_data <- function(x, test=F){
  #x <- x %>% mutate_all(~ifelse(.x == -999, NA_real_, .x))
  
  for(j in 1:nrow(meta)){
    
    if(str_detect(meta[j, 2], "^Qualitativo")){
      
      if(str_detect(meta[j, 2], "Qualitativo ordinal")){
        x[,j] <- factor(x %>% pull(j), ordered=T)
      }else if(str_detect(meta[j, 2], "Qualitativo nominal")){
        x[,j] <- as.factor(x %>% pull(j))
      }   
    }
  }
  
  
  return(x)
}

train <- prep_data(train)
count_level <- train %>% 
  summarise(across(where(is.factor), nlevels))

write.csv(count_level,"output/count_level.csv", row.names = FALSE, na = "")
menor10 <- function(x) {x <= 10}

levels_final <-  count_level %>% select_if(menor10) %>% select(-y)
levels_final <- colnames(levels_final)

train <- read.csv("data/train.csv")
test_factor <- test %>% 
  mutate_at(levels_final, as.factor)

train_factor <- train %>% 
  mutate_at(levels_final, as.factor)

model_ranger <- train(factor(y) ~.,
                      data = train_factor %>% select(-id),
                      method = "ranger",
                      metric = "Accuracy",
                      importance = 'permutation',
                      trControl = trControl,
                      tuneLength = 3)

# Submit
submit <- test_factor %>% 
  mutate(predicted = predict(model_ranger,test_factor)) %>% 
  select(id,predicted)

head(submit)
write.csv(submit, "output/ranger_factor_baixo_level.csv", row.names = FALSE)
