library(Metrics)
library(dplyr)
library(caret)
library(glmnet)
library(readr)
library(data.table)

#Read data:
wine = read.csv(file = "wine.csv")

# Split the data into training and test set
set.seed(123)
training.samples <- wine.quality$quality %>%
  createDataPartition(p = 0.5, list = FALSE)
train.data  <- wine.quality[training.samples, ]
test.data <- wine.quality[-training.samples,]

# Predictor variables
x <- model.matrix(quality~., train.data)[,-1]
# Outcome variable
y <- train.data$quality

linear = lm(quality ~ ., data = train.data)
summary(linear)

# Make predictions on the test data
predictions <- predict(linear, test.data, type = "response")

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
    Rsquare = R2(predictions, test.data$quality)
)

test.data.no.penalty = test.data
test.data.no.penalty$no.penalty.score= predictions
test.data.no.penalty <- test.data.no.penalty  %>% mutate(no.penalty.score.rounded = round(no.penalty.score))
test.data.no.penalty <- test.data.no.penalty %>% mutate(accurate = ifelse(no.penalty.score.rounded == quality, 1,0))
sum(test.data.no.penalty$accurate)/nrow(test.data.no.penalty)

#Computing lasso regression:

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = 1, nfolds = 5)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
fit.lasso <- glmnet(x, y, alpha = 1, lambda = cv$lambda.min)
# Dsiplay regression coefficients
coef(fit.lasso)

# Make predictions on the test data
x.test <- model.matrix(quality ~., test.data)[,-1]
predictions <- fit.lasso %>% predict(x.test) %>% as.vector()

# Model performance metrics
data.frame(
  RMSE = RMSE(predictions, test.data$quality),
  Rsquare = R2(predictions, test.data$quality)
)

#Calculate accuracy:
test.data.lasso = test.data
test.data.lasso$lasso.score= predictions
test.data.lasso <- test.data.lasso  %>% mutate(lasso.score.rounded = round(lasso.score))
test.data.lasso <- test.data.lasso %>% mutate(accurate = ifelse(lasso.score.rounded == quality, 1,0))
sum(test.data.lasso$accurate)/nrow(test.data.lasso)

#Plot actual quality scores and predicted data  score:
hist(test.data.lasso$quality)
hist(test.data.lasso$lasso.score)

