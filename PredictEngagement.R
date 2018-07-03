# Clear workspace and console
rm(list = ls())
cat("\014")

# Set options
options(digits = 3)
options(dplyr.width = Inf)

# Load packages
library(plyr) # Package to manipulate data
library(dplyr) # Upload dplyr to process data
library(tidyr) # Package to tidy data
library(ggplot2) # Package for visualization
library(ggthemes) # Package for applying themes of visualization
library(forcats) # Package for recoding categorical variable
library(lubridate) # Package to deal with datetime variable
library(zoo) # Convert numeric date to string date
library(RSQLite) # Read data from SQLite database
library(SuperLearner) # Pckage to implement the Super Learner predictive method
library(RhpcBLASctl) # Package to use multicore
library(doParallel) # Parallel computing
library(foreach) #  Parallel computing
library(unbalanced) # Package that deals with unbalanced dataset
library(DMwR) # Package for oversampling imbalanced dataset (SMOTE)
library(xgboost) # Package to implement XGBoost algorithm
library(caret) # Package to split sample into train and test sets
library(pROC) # Calculate ROC
library(PRROC) # Calculate ROC under the PR curve

#############################################################
# Exploratory Data Analysis
#############################################################
# -----------------------------------------------------------
# Mixpanel Events dataset
# -----------------------------------------------------------
# Establish connection to SQLite database.
con <- dbConnect(SQLite(),
                 dbname = "D:/Data Science/Insight/Projects/Shleep/Data/database.db")

# Show list of tables in the database
as.data.frame(dbListTables(con))
# Get table
mixpanel <- dbReadTable(con, 'all-mixpanel-events-anon')
# Disconnect after fetching the data.
dbDisconnect(con)

# Checking proportions of missing value for each column
sort(sapply(mixpanel, function(x) sum(is.na(x))/nrow(mixpanel)))

# Select relevant variables that don't have extremely high proportion of missing value
mixpanel_subset <- mixpanel %>%
  dplyr::select(event, distinct_id, mp_country_code, X.browser,
                X.initial_referrer, X.os, X.device, X.screen_height, X.screen_width) %>%
  dplyr::rename(country_code = mp_country_code, browser = X.browser,
                initial_referrer = X.initial_referrer, os = X.os, device = X.device,
                screen_height = X.screen_height, screen_width = X.screen_width)

# Checking proportions of missing value for each column
sapply(mixpanel_subset, function(x) sum(is.na(x))/nrow(mixpanel_subset))







#---------------------------------------------------------
# Copy into a separate dataset and feature engineering
#---------------------------------------------------------
data <- mixpanel_subset

# Create function rescaling outcome into 0-1
normalize <- function(x) {
  x = (x - min(x))/(max(x) - min(x))
}

# Create target variable of playing the video.
data$video_played <- ifelse(data$event == "video play", 1, 0)

# One-hot encoding country variable and feature engineering
data <- data %>% dplyr::select(distinct_id, country_code, browser,
                               initial_referrer, os, device,
                               screen_height, screen_width,
                               video_played) %>%
  mutate(country_BE = ifelse(country_code == "BE", 1, 0),
         country_NL = ifelse(country_code == "NL", 1, 0),
         country_ES = ifelse(country_code == "ES", 1, 0),
         country_US = ifelse(country_code == "US", 1, 0),
         browser = ifelse(browser == "Mobile Safari", 1, 0),
         os = ifelse(os == "Android", 1, 0),
         device = ifelse(device == "iPhone", 1, 0),
         initial_referrer = ifelse(initial_referrer == "$direct", 1, 0)) %>%
  dplyr::select(-c(country_code)) %>% na.omit() %>%
  mutate(distinct_id = factor(distinct_id),
         screen_height = normalize(screen_height),
         screen_width = normalize(screen_width))

# Check imbalance of the target variable.
prop.table(table(data$video_played))
# [0: 0.9638, 1: 0.0362]

#------------------------------------------------------------------------
# Split the original sample into training set (75%) and testing set (25%)
#------------------------------------------------------------------------
set.seed(234)
splitIndex <- createDataPartition(data$video_played, times = 1, p = .75, list = FALSE)
train_set <- data[ splitIndex,]
test_set <- data[-splitIndex,]

# Check imbalance of the target variable in the train set and test set.
prop.table(table(train_set$video_played))
# [0: 0.9638, 1: 0.0362]
prop.table(table(test_set$video_played))
# [0: 0.9638, 1: 0.0362]

# Split into training set and test set
X_train <- train_set[, c(2:7, 9:12)]
Y_train <- train_set[, 8]
X_test <- test_set[, c(2:7, 9:12)]
Y_test <- test_set[, 8]

# Check imbalance of the oversampled sample
prop.table(table(Y_train))
# [0: 0.806, 1: 0.194]
prop.table(table(Y_test))

# Create xgb.DMatrix training set
dtrain <- xgb.DMatrix(data = data.matrix(X_train), label = Y_train)
dtest <- xgb.DMatrix(data = data.matrix(X_test), label = Y_test)





#---------------------------------------------------------
# Build and test hyperparameter-tuned XGBoost model
#---------------------------------------------------------
# Set our hyperparameters
param <- list(objective   = "binary:logistic",
              eval.metric = "logloss",
              eval_metric = "auc",
              eval_metric = "error",
              max_depth   = 3,
              eta         = 0.1,
              early_stopping_rounds = 200,
              scale_pos_weight = prop.table(table(Y_train))[1]/prop.table(table(Y_train))[2],
              gammma      = 1,
              colsample_bytree = 0.8,
              min_child_weight = 5)

# Train hyperparameter-tuned XGBoost model
xgb <- xgboost(params  = param,
               data    = dtrain,
               label   = Y_train, 
               nrounds = 2000,
               nthread = 2,
               print_every_n = 100,
               verbose = 1)

# Create our prediction probabilities and calculate AUC
Y_pred <- predict(xgb, dtest)
auc_data <- data.frame(cbind(Y_pred, Y_test))
pROC::roc(Y_test, Y_pred)$auc

# Get the ROC curve
ggplot(auc_data, aes(m = Y_pred, d = Y_test)) +
  geom_roc(labels = FALSE, n.cuts = 10, size = 2, color = 'black') +
  style_roc(theme = theme_wsj) +
  xlab("False positive fraction") +
  ylab("True positive fraction") +
  ggtitle("AUC Curve (AUC = 0.63)") +
  theme(axis.title = element_text(size = 35))
ggsave("AUC_XGB.jpeg", width = 14, height = 8.5, units = "in")


#-------------------------------------------------------------
# XGBoost feature importance using the entire original dataset
#-------------------------------------------------------------
dataxgb <- xgb.DMatrix(data = data.matrix(data[, c(2:7, 9:12)]),
                       label = data[, 8])

# Set our hyperparameters
param_full <- list(objective   = "binary:logistic",
              eval.metric = "logloss",
              eval.metric = "auc",
              eval.metric = "error",
              max_depth   = 3,
              eta         = 0.1,
              early_stopping_rounds = 200,
              scale_pos_weight = table(data[, 8])[1]/table(data[, 8])[2],
              gammma      = 1,
              colsample_bytree = 0.8,  
              min_child_weight = 5)

# Train hyperparameter-tuned XGBoost model
xgb_full <- xgboost(params  = param_full,
                    data    = dataxgb,
                    label   = data[, 8],
                    nrounds = 2000,
                    print_every_n = 100,
                    verbose = 1)

# Feature importance
model <- xgb.dump(xgb_full, with_stats = TRUE)

# Get the feature real names
names <- dimnames(data)[[2]][c(2:7, 9:12)]

# Compute feature importance matrix
# View top 10 most important features
importance_matrix <- xgb.importance(names, model = xgb_full)[0:10]

# Plot
xgb.plot.importance(importance_matrix, rel_to_first = TRUE,
                    xlab = "Relative importance")
xgb.ggplot.importance(importance_matrix = importance_matrix,
                      top_n = 10,
                      measure = "Gain",
                      rel_to_first = FALSE, n_clusters = c(0: 1),
                      xlab("Relative importance (information gain)")) +
  geom_bar(stat = "identity", color = "darkblue", size = 2.5) +
  theme_fivethirtyeight() +
  theme(axis.text = element_text(size = 30, face = 'bold'))
ggsave("feature_importance.jpeg", width = 14, height = 8.5, units = "in")

# -----------------------------------------------------------
# Save results and data in an RData file
save.image(file = "PredictEngagement.RData")
# -----------------------------------------------------------