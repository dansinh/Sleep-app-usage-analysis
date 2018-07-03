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
library(plotROC) # Visualize ROC using ggplot2

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

# Transform variables
data <- data %>%
  mutate(video_played = factor(video_played),
         browser = factor(browser),
         initial_referrer = factor(initial_referrer),
         os = factor(os),
         device = factor(device),
         country_BE = factor(country_BE),
         country_NL = factor(country_NL),
         country_ES = factor(country_ES),
         country_US = factor(country_US))

# Split the sample to create a test set comprising only 10% of observations
set.seed(123)
splitIndex <- createDataPartition(data$video_played, times = 1, p = .1, list = FALSE)
train_set <- data[ splitIndex,]
test_set <- data[-splitIndex,]

# Check imbalance of the target variable in the train set and test set.
prop.table(table(train_set$video_played))
# [0: 0.9638, 1: 0.0362]
prop.table(table(test_set$video_played))
# [0: 0.9638, 1: 0.0362]

# Oversampling the training set with 5 nearest neighbors
set.seed(1234)
over_sample <- ubSMOTE(X = train_set[, c("distinct_id", "browser", "initial_referrer",
                                         "os", "device",
                                         "screen_height", "screen_width",
                                         "country_BE", "country_NL",
                                         "country_ES", "country_US")],
                       Y = train_set[, "video_played"],
                       perc.over = 200, k = 5, perc.under = 800, verbose = TRUE)
X <- over_sample[[1]]
Y <- ifelse(over_sample[[2]] == 1, 1, 0)
id <- over_sample[[1]]$distinct_id
balanced_sample <- data.frame(cbind(X, Y))

# Check imbalance of the oversampled sample
prop.table(table(Y))
# [0: 0.842, 1: 0.158]







# ----------------------------------------------------------------------
# Employ the Super Learning framework to tune hyperparameters of XGBoost
# ----------------------------------------------------------------------
# Setup parallel computation - use all cores on computer
num_cores = RhpcBLASctl::get_num_cores()
# Use all of those cores for parallel SuperLearner.
options(mc.cores = num_cores)
# Check how many parallel workers we are using: 
getOption("mc.cores")
# We need to set a different type of seed that works across cores
set.seed(1, "L'Ecuyer-CMRG")

# Set XGBoost 3*4*3 = 36 different configurations fro grid search
tune = list(ntrees = c(500, 1000, 2000),
            max_depth = c(2:5),
            shrinkage = c(0.01, 0.05, 0.1))

# Set detailed names = T to see each configuration
learners = create.Learner("SL.xgboost", tune = tune,
                          detailed_names = T, name_prefix = "XGB")

# Small sample < 1K observations
small_sample <- dplyr::sample_n(data.frame(balanced_sample),
                                size = nrow(balanced_sample)/10,
                                replace = FALSE)
small_id <- small_sample$distinct_id
small_X <- small_sample %>% dplyr::select(-c(distinct_id, Y))
small_Y <- small_sample$Y

# Create Super Learning library of XGBoost configurations
SL.library_tuning <- c(learners$names)

# Tune XGBoost using cross-validated Super Learner
set.seed(213)
XGB_tuning = CV.SuperLearner(Y = small_Y, X = small_X,
                             family = binomial(), SL.library = SL.library_tuning,
                             method = "method.NNloglik", verbose = TRUE,
                             id = small_sample$distinct_id,
                             V = 5,
                             parallel = "multicore")

# Extract and plot the cross-validated tuning results
plot.CV.SuperLearner(XGB_tuning)
tuning_result <- summary.CV.SuperLearner(XGB_tuning)$Table
tuning_result[order(tuning_result$Ave), ]
tuning_result$Algorithm <- factor(tuning_result$Algorithm,
                                  levels = tuning_result$Algorithm[order(tuning_result$Ave,
                                                                         decreasing = TRUE)])
tuning <- ggplot(data = tuning_result,
                 aes(x = Algorithm, y = Ave, ymin = Min, ymax = Max)) +
  geom_pointrange(size = 1.2) + coord_flip() +
  ggtitle("XGBoost Configurations Cross-validated Risk") +
  theme_wsj() +
  xlab("Cross-validated risk (non-negative logloss)") +
  theme(axis.text = element_text(size = 12))
tuning
ggsave("XGB_tuning.jpeg", width = 14, height = 8.5, units = "in")
# -----------------------------------------------------------







# -----------------------------------------------------------------------
# Employ the Super Learning framework to conduct algorithmic horse-racing
# -----------------------------------------------------------------------
# Set XGBoost configuration using cross-validated tuned hyperparameters
XGB = create.Learner("SL.xgboost",
                     tune = list(ntrees = 2000, max_depth = 3, shrinkage = 0.1),
                     detailed_names = T, name_prefix = "XGB")

# Create library of Super Learners with five algorithms
SL.library <- c("SL.glm", "SL.gam", "SL.mean", "SL.xgboost", XGB$names)

# Conduct cross-validated algorithmic horse-racing
X <- X %>% dplyr::select(-c(distinct_id))
SL_horseracing = CV.SuperLearner(Y = Y, X = X,
                                 family = binomial, SL.library = SL.library,
                                 method = "method.NNloglik", verbose = TRUE,
                                 id = id,
                                 V = 5,
                                 parallel = "multicore")
results <- summary.CV.SuperLearner(SL_horseracing)$Table

# Plot algorithmic performance
results <- results %>%
  mutate(algorithms = c("Super Learner", "Discrete Super Learner",
                        "Logistic regression model", "Generalized additive model",
                        "Weighted mean outcome", "XGBoost (default)",
                        "XGBoost (tuned)"))
results$algorithms <- factor(results$algorithms,
                             levels = results$algorithms[order(results$Ave,
                                                               decreasing = TRUE)])
ggplot(data = results, aes(x = algorithms, y = Ave, ymin = Min, ymax = Max)) +
  geom_pointrange(size = 1.2) + 
  coord_flip() +
  ggtitle("5-fold Cross-validated Risk") +
  theme_wsj() + theme(axis.text = element_text(size = 32))
ggsave("horse_racing.jpeg", width = 14, height = 8.5, units = "in")
# -----------------------------------------------------------



# -----------------------------------------------------------
# Save results and data in an RData file
save.image(file = "ML_tuning.RData")
# -----------------------------------------------------------