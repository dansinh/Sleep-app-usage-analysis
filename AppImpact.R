# Clear console and workspace
rm(list = ls())
cat('\014')

# Set options
options(digits = 3)
options(dplyr.width = Inf)

# Upload R packages
library(plyr) # Package to manipulate data
library(tidyr) # tinyr package to tidy data
library(dplyr) # dplyr package to manipulate data
library(lubridate) # package to deal with datetime variable
library(ggplot2) # package for data visualization
library(ggthemes) # package to apply visualization theme
library(forcats) # package for recoding levels of a categorical variable
library(zoo) # Package to turn numeric into date format
library(lme4) # Package for fitting linear mixed effects models
library(broom) # Get multiple fitted models
library(plyr) # Get summary statistics
# -----------------------------------------------------------





# -----------------------------------------------------------
# User Sleep dataset
# -----------------------------------------------------------
# Read the User Sleep dataset into R
user_sleep <- read.csv("user_sleeps.csv", header = TRUE, stringsAsFactors = FALSE)

# Arrange the rows by user_id and created_at (renamed as reported_date).
# Typeset variables and remove observations with missing values.
# Select only observations with sleep hours > 0 and < 24
user_sleep <- user_sleep %>%
  mutate(reported_date = ymd(as.Date(created_at))) %>%
  dplyr::select(c(user_id, reported_date, hours, rating)) %>%
  arrange(user_id, reported_date) %>%
  mutate(hours = as.numeric(hours), rating = factor(rating),
         rating = fct_recode(rating, "Great" = "great", "Not Good" = "not-good",
                             "OK" = "ok", "No Rating" = "NULL")) %>%
  filter(hours > 0 & hours < 24) %>%
  na.omit

# Count the number of unique users in this dataset (18741)
length(unique(user_sleep$user_id))

# -----------------------------------------------------------
# Sleep Health dataset
# -----------------------------------------------------------
# Read the User Sleep Health dataset into R
sleep_health <- read.csv("user_health_data.csv", header = TRUE, stringsAsFactors = FALSE)

# Calculate numbers of hours in bed and asleep based on start time and end time
# Remove all observations with sleep hours <= 0 or >= 24.
# Only consider asleep observations for now.
# Sum up sleep hours for users who reportedly slept multiple times a day.
duration <- function(start, end){
  start = ymd_hms(start)
  end = ymd_hms(end)
  return(as.duration(start %--% end)/dhours(1))
}

sleep_health <- sleep_health %>%
  mutate(sleep_hours = duration(starts_at, ends_at),
         ends_at = ymd(as.Date(ends_at))) %>%
  filter(type == "sleepdata.asleep") %>%
  dplyr::select(user_id, sleep_hours, ends_at) %>%
  arrange(user_id, ends_at) %>%
  group_by(user_id, ends_at) %>% summarize(total_sleep_hours = sum(sleep_hours)) %>%
  filter(total_sleep_hours > 0, total_sleep_hours < 24) %>%
  data.frame()

# Count the number of unique users in this dataset (971)
length(unique(sleep_health$user_id))


# ----------------------------------------------------
# Inner join User Sleep and User Sleep Health (971 unique users)
# Use first date of reporting sleep hours as usage start date, i.e., applying treatment.
# Sleep outcome measures include before and after treatment (app usage) 
sleep_data <- left_join(sleep_health, user_sleep, by = 'user_id') %>%
  group_by(user_id) %>%
  mutate(usage_date = min(ends_at)) %>%
  mutate(measure_date = ifelse(reported_date < usage_date, reported_date, ends_at)) %>%
  mutate(measure_date = as.Date(measure_date)) %>%
  mutate(sleep_outcome = ifelse(measure_date < usage_date, hours, total_sleep_hours)) %>%
  mutate(days = as.numeric(measure_date - usage_date)) %>%
  dplyr::select(user_id, usage_date, measure_date, days, sleep_outcome) %>%
  distinct(user_id, usage_date, measure_date, days, sleep_outcome)  %>%
  mutate(app_treatment = ifelse(measure_date < usage_date, 0, 1)) %>%
  data.frame() %>% na.omit()

# Count the number of unique users in this combined dataset (944)
length(unique(sleep_data$user_id))

# Manually identify users in Sleep Health group that are not in Sleep Data group
sum(unique(sleep_data$user_id) %in% unique(sleep_health$user_id))
setdiff(unique(sleep_health$user_id), unique(sleep_data$user_id))

# Create function rescaling outcome into 0-1
normalize <- function(x) {
  x = (x - min(x))/(max(x) - min(x))
}

#----------------------------------------------------
# Prepare dataset for fitting a mixed effect model
#----------------------------------------------------
data <- sleep_data
data <- data %>% mutate(user_id = factor(user_id),
                        app_treatment = factor(app_treatment),
                        days_scaled = normalize(days)) %>%
  dplyr::select(-c(measure_date, usage_date, days)) %>%
  group_by(user_id) %>%
  mutate(lag_outcome = dplyr::lag(sleep_outcome)) %>% na.omit() %>% data.frame()

#----------------------------------------------------
# Mixed effects model with bootstrap inference
#----------------------------------------------------
set.seed(21)
boot_lmer <- data %>% bootstrap(1000) %>%
  do(tidy(lmer(sleep_outcome ~ app_treatment + lag_outcome + days_scaled +
                 (1 + app_treatment | user_id), .)))
boot_lmer

# Summarize the results of the bootstraping
alpha = .05
boot_lmer %>% group_by(term) %>%
  summarize(low = quantile(estimate, alpha / 2),
            high = quantile(estimate, 1 - alpha / 2))

boot_lmer_df <- data.frame(boot_lmer)
boot_lmer_df <- boot_lmer_df %>% dplyr::filter(term == "app_treatment1") %>%
  dplyr::select(estimate)

# Compute the quantiles of bootstrap estimates
boot_quantiles <- lapply(boot_lmer_df, quantile, probs = c(0.05, 0.5, 0.95), name = FALSE)
vl = boot_quantiles[[1]][1]
vm = boot_quantiles[[1]][2]
vu = boot_quantiles[[1]][3]

# Plot boostrap estimates of app usage impact
ggplot(data = boot_lmer_df) +
  geom_histogram(aes(x = estimate), fill = 'darkblue', color = 'white', bins = 100) +
  theme_wsj() +
  xlab('Estimated effect of app usage (hours) using mixed effect model') +
  geom_vline(aes(xintercept = vl), color = 'salmon', size = 2.5) +
  geom_vline(aes(xintercept = vm), color = 'salmon', size = 2.5) +
  geom_vline(aes(xintercept = vu), color = 'salmon', size = 2.5) +
  scale_x_continuous(breaks = sort(c(vl, 0, vm, 0.1, vu))) +
  ggtitle('Bootstrap Estimates of App Usage Effect') +
  theme(axis.title = element_text(size = 25))
ggsave("app_impact_LMER.jpeg", width = 14, height = 8.5, units = "in")


#----------------------------------------------------
# Super Learner model with bootstrap inference
#----------------------------------------------------
# Set multicore compatible seed.
set.seed(12, "L'Ecuyer-CMRG")
# Setup parallel computation - use all cores on our computer.
num_cores = RhpcBLASctl::get_num_cores()
# Use all of those cores for parallel SuperLearner.
options(mc.cores = num_cores)
# Check how many parallel workers we are using: 
getOption("mc.cores")

# Create Super Learner library
SL.library <- c("SL.glm", "SL.gam", "SL.xgboost")

# Parallelization
cluster = parallel::makeCluster(2)
# Load the SuperLearner package on all workers so they can find
# SuperLearner::All(), the default screening function which keeps all variables.
parallel::clusterEvalQ(cluster, library(SuperLearner))
# We need to explictly export our custom learner functions to the workers.
parallel::clusterExport(cluster, SL.library)
parallel::clusterSetRNGStream(cluster, 2)

# Create cluster for parallel computation
cl <- makeCluster(2)
registerDoParallel(cl)
B <- 1000
psi_boot <- matrix(NA, nrow = B, ncol = 1)

# Parallel computation of app usage effect estimates
foreach(b = 1:B, .packages = c("dplyr", "xgboost", "SuperLearner"),
        .verbose = TRUE) %do% {
          
          # Create bootstrap indices
          bootIndices <- sample(1:nrow(data), replace = TRUE)
          bootData <- data[bootIndices, ]
          ndata <- nrow(bootData)
          
          # Causal effect of app treatment using resample dataset
          psi <- matrix(NA, nrow = 1, ncol = 1)
          id = bootData$user_id
          Y <- bootData$sleep_outcome
          X <- data.frame(bootData[, c("app_treatment", "days_scaled", "lag_outcome")])
          X1 <- X0 <- X
          X1[, c("app_treatment")] <- 1
          X0[, c("app_treatment")] <- 0
          newdata <- rbind(X, X1, X0)
          SLmodel <- snowSuperLearner(Y = Y, X = X, SL.library = SL.library,
                                       newX = newdata, family = "gaussian",
                                       method = "method.NNLS",
                                       id = id,
                                       verbose = TRUE, cluster = cluster,
                                       cvControl = list(V = 5L, shuffle = TRUE))
          predX1 <- SLmodel$SL.predict[(ndata + 1):(2*ndata)]
          predX0 <- SLmodel$SL.predict[(2*ndata + 1):(3*ndata)]
          psi[1, 1] <- mean(predX1 - predX0)
          print(b)
 
          # Combine bootstrap estimates
          psi_boot[b, 1] <- psi
        }

# Calculate quantiles of bootstrap estimates
psi_boot <- data.frame(psi_boot)
psi_quantiles <- lapply(psi_boot, quantile, probs = c(0.05, 0.5, 0.95), name = FALSE)
psi_low = psi_quantiles[[1]][1]
psi_mean = psi_quantiles[[1]][2]
psi_high = psi_quantiles[[1]][3]

# Plot bootstrap estimates and 0.05 and 0.95 quantiles
ggplot(data = psi_boot, aes(x = psi_boot)) +
  geom_histogram(fill = 'darkblue', color = 'white', bins = 100) +
  theme_wsj() +
  xlab('Super Learner-based bootstrap estimates of app usage effect (hours)') +
  geom_vline(aes(xintercept = psi_low), color = 'salmon', size = 2.5) +
  geom_vline(aes(xintercept = psi_mean), color = 'salmon', size = 2.5) +
  geom_vline(aes(xintercept = psi_high), color = 'salmon', size = 2.5) +
  scale_x_continuous(breaks = sort(c(0, psi_low, psi_mean, psi_high))) +
  ggtitle('Bootstrap Estimates of App Usage Impact') +
  theme(axis.title = element_text(size = 22))
ggsave("app_impact_SL.jpeg", width = 14, height = 8.5, units = "in")

# -----------------------------------------------------------
# Save results and data in an RData file
save.image(file = "App_Impact.RData")
# -----------------------------------------------------------