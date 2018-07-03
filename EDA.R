# Clear console and workspace if you do not use Porject Options in RSrudio
rm(list = ls())
cat('\014')

# Set digit display options
options(digits = 1)
options(dplyr.width = Inf)

# Load R packages
library(plyr) # Package to manipulate data frames. Make sure lead this before loading dplyr
library(tidyr) # Package to manipulate data frames
library(dplyr) # Package to manipulate data frames
library(lubridate) # Package to deal with datetime variables
library(ggplot2) # Package for data visualization
library(ggthemes) # Package to apply visualization themes
library(forcats) # Package for recoding levels of categorical variables
library(zoo) # Package to turn numeric into date format
library(knitr) # Package to work with R markdown documents
library(lme4) # Package for fitting linear mixed effects models
library(broom) # Get multiple fitted models
# -----------------------------------------------------------







#############################################################
# Exploratory Data Analysis
#############################################################
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

# Histogram of initial reported sleep hours
v_hours = median(user_sleep$hours)
ggplot(user_sleep, aes(x = hours)) +
  geom_histogram(fill = 'darkblue', color = 'white', binwidth = 1) +
  geom_vline(aes(xintercept = v_hours), color = "salmon", size = 2.5) +
  scale_x_continuous(breaks = sort(c(seq(0, 15, length.out = 4), v_hours))) +
  theme_wsj() + labs(x = 'Self-reported numbers of sleep hours (and median)',
                     y = 'Number of user-day observations') +
  theme(axis.title=element_text(size = 25))
ggsave("initial_sleep_hours.jpeg", width = 14, height = 8.5, units = "in")

# Bar plot of initial sleep quality
ggplot(data = user_sleep, aes(x = rating)) +
  geom_bar(fill = 'darkblue', color = 'white', width = 0.4) +
  theme_wsj() + xlab('Self-reported sleep quality') +
  ylab('Number of user-day observations') +
  theme(axis.title = element_text(size = 25))
ggsave("initial_sleep_ratings.jpeg", width = 14, height = 8.5, units = "in")
# -----------------------------------------------------------







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

# Histograms of sleep duration
v_sleep = median(sleep_health$total_sleep_hours)
ggplot(sleep_health) +
  geom_histogram(aes(x = total_sleep_hours), fill = 'darkblue',
                 color = 'white', binwidth = 0.5,
                 position = "identity", alpha = 0.9) +
  theme_wsj() + theme(axis.title = element_text(size = 25)) +
  scale_x_continuous(name = "Reported hours of sleep by app users",
                     breaks = seq(0, 24, 1),
                     limits = c(0, 24), v_sleep) +
  geom_vline(aes(xintercept = v_sleep), color = "salmon", size = 2.5) +
  scale_y_continuous(name = "Number of user-day observations")
ggsave("reported_sleep_hours.jpeg", width = 14, height = 8.5, units = "in")
# ----------------------------------------------------







# -----------------------------------------------------------
# Notification datasets
# -----------------------------------------------------------
# Read the notification types dataset into R
notif <- read.csv("notifications.csv") %>% dplyr::select(id, type, slug) %>%
  dplyr::rename(notification_id = id)

# Read the notification sent dataset and count the number of notifications by user_id
notif_sent <- read.csv("notification_sent.csv") %>%
  dplyr::select(user_id, notification_id, sent_at) %>%
  mutate(sent_at = ymd(as.Date(sent_at)))

# Combine the notification types and notification_sent datasets by notification_id
notif_data <- notif_sent %>%
  left_join(notif, by = "notification_id") %>%
  mutate(user_id = factor(user_id),
         notification_id = factor(notification_id),
         type = fct_recode(type, "Time tips" = "tip-trick-time",
                           "General tips" = "tip-trick-day",
                           "Sleep tips" = "sleep-habit",
                           "Main tips" = "tip-trick-main")) %>%
  select(user_id, notification_id, type, sent_at)

# Plot distribution of notification types
ggplot(data = notif_data, aes(x = type, y = ..count..)) +
  geom_bar(fill = 'darkblue', color = 'white', width = 0.3) +
  scale_y_log10(breaks = c(0, 10, 1000, 10000)) +
  geom_text(stat = "count", aes(label = format(..count..)), vjust = -1, cex = 5) +
  theme_wsj() +
  xlab('Type of notifications') + ylab('Number of notifications sent') +
  theme(axis.title = element_text(size = 20))
ggsave("notif_type.jpeg", width = 14, height = 8.5, units = "in")

# Plot distribution of notification frequency
notif_id_summary <- data.frame(notif_data %>% select(user_id, notification_id, type) %>%
                                 group_by(notification_id, type) %>% tally())

ggplot(data = notif_id_summary, aes(x = reorder(notification_id, -n), y = n, fill = type)) +
  geom_bar(stat = "identity", color = 'white') +
  coord_flip() +
  theme_wsj() + xlab('Notification IDs') +
  ylab('Number of sent notifications (2018/05/07-2018/06/08)') +
  theme(axis.title = element_text(size = 20)) +
  scale_fill_manual(values = c("darkblue", "darkgreen", "salmon")) +
  theme(legend.text = element_text(size = 25))
ggsave("notif_sent.jpeg", width = 14, height = 8.5, units = "in")

# Plot density of notifications by users
user_id_summary <- data.frame(notif_data %>% select(user_id, notification_id, type) %>%
                                group_by(user_id) %>% tally()) %>% arrange(desc(n))

ggplot(data = user_id_summary, aes(n)) +
  geom_density(alpha = 0.75, fill = 'darkblue', color = 'white') +
  scale_x_continuous(limits = c(0, 150), breaks = round(seq(0, 150, 10))) +
  theme_wsj() +
  xlab('Number of notifications received') +
  ylab('Proportion of app users') +
  theme(axis.title = element_text(size = 20)) 
ggsave("notif_density.jpeg", width = 14, height = 8.5, units = "in")

# Read the notification schedules dataset into R
notif_schedule <- read.csv("notification_schedules.csv")

# Select relevant variables
notif_schedule <- notif_schedule %>% select(user_id, notification_id,
                                            day_of_week, hour) %>%
  dplyr::rename(day = day_of_week) %>% mutate(user_id = factor(user_id),
                                              notification_id = factor(notification_id),
                                              day = factor(day), hour = factor(hour))

# Group and plot notification schedule by day and time
notif_schedule_day <- data.frame(notif_schedule %>%
                                   select(user_id, notification_id, day) %>%
                                   group_by(day) %>% tally())
ggplot(notif_schedule_day, aes(x = day, y = n, group = 1)) +
  geom_point(size = 4, color = 'darkblue') +
  geom_path(color = 'darkblue', size = 2) +
  theme_wsj() + xlab("Day of the week") +
  ylab("Number of notifications sent") +
  theme(axis.title = element_text(size = 25))
ggsave("notif_day.jpeg", width = 14, height = 8.5, units = "in")

notif_schedule_hour <- data.frame(notif_schedule %>%
                                    select(user_id, notification_id, hour) %>%
                                    group_by(hour) %>% tally())
ggplot(notif_schedule_hour, aes(x = hour, y = n, group = 1)) +
  geom_point(size = 4, color = 'darkblue') +
  geom_path(color = 'darkblue', size = 2) +
  theme_wsj() + xlab("Hour of the day") +
  ylab("Number of notifications sent") +
  theme(axis.title = element_text(size = 25))
ggsave("notif_hour.jpeg", width = 14, height = 8.5, units = "in")
# ----------------------------------------------------

# Re-read the user sleep data into R.
# 19072 unique users, 95179 observations.
sleep_data <- read.csv("user_sleeps.csv", stringsAsFactors = FALSE, header = TRUE)

# Select and typecast relevant columns.
sleep_data <- sleep_data %>%
  dplyr::select(c(user_id, hours, rating, created_at)) %>%
  mutate(rating = factor(rating),
         hours = as.numeric(hours),
         user_id = factor(user_id),
         created_at = as.Date(created_at))

## Re-read the sleep health dataset into R.
# 2430 unique users, 82305 observations.
sleep_health <- read.csv("user_health_data.csv", stringsAsFactors = FALSE, header = TRUE)

# Select and typecast relevant variables.
sleep_health <- sleep_health %>%
  dplyr::select(user_id, type, starts_at, ends_at, created_at) %>%
  dplyr::mutate(user_id = factor(user_id),
                type = factor(type),
                created_at = as.POSIXct(created_at)) %>%
  mutate(type = fct_recode(type,
                           "In Bed" = "sleepdata.in_bed",
                           "Asleep" = "sleepdata.asleep"))

# Compute sleep duration and in_bed duration.
# Functions to calculate time duration in hours.
# Filter only users whose sleep hours are between 0 and 24 hours a day.
duration <- function(s, e){
  start = ymd_hms(s)
  end = ymd_hms(e)
  return(as.duration(start %--% end)/dhours(1))
}

sleep_health <- sleep_health %>%
  mutate(duration_time = duration(starts_at, ends_at)) %>%
  filter(duration_time < 24, duration_time > 0)

# Select relevant columns.
# Convert string dates into numeric; combine sleep duration by date.
# Turn numeric dates back to string dates.
# Filter only those who sleep, not just stay in bed.
# Combine all hours for those who reported multiple times.
sleep_health <- data.frame(sleep_health %>%
                             filter(type == "Asleep") %>%
                             dplyr::select(user_id, duration_time, user_id, ends_at) %>%
                             mutate(ends_at = unclass(as.Date(as.POSIXlt(ends_at)))) %>%
                             group_by(user_id, ends_at) %>%
                             summarize(duration_time = sum(duration_time)) %>%
                             mutate(ends_at = as.Date(ends_at)))

# Combine sleep_health, sleep_data, notification datasets.
sleep_notif <- sleep_health %>%
  inner_join(notif_data, by = "user_id") %>%
  mutate(user_id = factor(user_id)) %>%
  na.omit()

# Calculate the effect of using the app on sleep hours.
# Functions to calculate gap time in hours.
gap_duration <- function(s, e){
  start = ymd(s)
  end = ymd(e)
  return(as.duration(start %--% end)/ddays(1))
}

# Select only users that reported sleep time right after receiving notifications.
sleep_notif_effect <- sleep_notif %>%
  mutate(notif_gap = gap_duration(sleep_notif$ends_at,
                                  as.Date(sleep_notif$sent_at))) %>%
  select(user_id, duration_time, ends_at, sent_at, notif_gap) %>%
  mutate(notif_gap = as.numeric(notif_gap)) %>%
  filter(notif_gap <= 0, notif_gap >= -1, duration_time < 24) %>%
  distinct(user_id, hours, ends_at, sent_at, duration_time)

# Calculate the effect of same-day notification
v_notif = median(sleep_notif_effect$duration_time)
ggplot(data = sleep_notif_effect) +
  geom_histogram(aes(x = duration_time, y = (..count..)/sum(..count..)),
                 fill = 'darkblue', alpha = 0.8, binwidth = 0.5) +
  geom_vline(aes(xintercept = v_notif), color = 'salmon', size = 2) +
  scale_x_continuous(limits = c(0, 20), breaks = round(seq(0, 20, 1))) +
  theme_wsj() + xlab('Sleep hours and median value with same-day notifications') +
  ylab('Proportion of app users') +
  theme(axis.title = element_text(size = 20))
ggsave("sleep_sameday_notif.jpeg", width = 14, height = 8.5, units = "in")
# -----------------------------------------------------------




# -----------------------------------------------------------
# Save results and data in an RData file
save.image(file = "EDA.RData")
# -----------------------------------------------------------