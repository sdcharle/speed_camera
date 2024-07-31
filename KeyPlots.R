# explore 
library(DBI)
library(odbc)
library(ggplot2)
library(lubridate)
library(dplyr)
library(mclust)
library(cluster)
library(jpeg)
library(grid)
library(glue)
library(readr)
library(tidyr)
# Jul 2 is when speed got dialed in
db_path = "data/speed_cam.db"

db <- RSQLite::dbConnect(RSQLite::SQLite(), 
                         dbname = db_path)
speed <- odbc::dbGetQuery(db, 'select * from speed ')

speed <- speed %>% mutate(speed_date = ymd_hms(log_timestamp))

begin_date = '2024-07-02'

speed <- speed %>% 
  filter(speed_date >= ymd(begin_date))



# things for shiny

ggplot(speed, aes(x = hour(speed_date), group = hour(speed_date), y = ave_speed)) + 
  geom_boxplot() +
  ggtitle("Speed Spread by hour of day") +
  labs(x = "Hour", y = "Speed") +
  geom_hline(yintercept = 30, color = "green", linetype = "dashed") +
  geom_hline(yintercept = 40, color = "red", linetype = "dashed") +
  theme_minimal()

ggplot(speed, aes(x = wday(speed_date, label = TRUE), y = ave_speed)) + 
  geom_boxplot() +
  ggtitle("Speed Spread by Day of the Week") +
  labs(x = "", y = "Speed") + 
  geom_hline(yintercept = 30, color = "green", linetype = "dashed") +
  geom_hline(yintercept = 40, color = "red", linetype = "dashed") +
  theme_minimal()

# Calculate counts per hour by day of the week
speed_count <- speed %>%
  mutate(day_of_week = wday(speed_date, label = TRUE),
         hour_of_day = hour(speed_date)) %>%
  group_by(day_of_week, hour_of_day) %>%
  summarise(count = n()) %>%
  ungroup()

# Calculate the number of occurrences of each day of the week in the dataset
day_occurrences <- speed %>%
  mutate(day_of_week = wday(speed_date, label = TRUE)) %>%
  group_by(day_of_week) %>%
  summarise(days_count = n_distinct(floor_date(speed_date, "day")))

# Join the occurrences data with the speed_count data
normalized_speed_count <- speed_count %>%
  left_join(day_occurrences, by = "day_of_week") %>%
  mutate(normalized_count = count / days_count)

# Plot the normalized data with a distinguishable color palette
ggplot(normalized_speed_count, aes(x = hour_of_day, y = normalized_count, group = day_of_week, color = day_of_week)) + 
  geom_line() +
  ggtitle("Vehicles per Hour by Day of the Week") +
  theme_minimal() +
  labs(x = "Hour of the Day", y = "Vehicles") +
  scale_color_brewer(palette = "Set1")


hour_count <-speed %>%
  mutate(speed_hour = floor_date(speed_date, "hour")) %>% 
  group_by(speed_hour) %>% 
  summarize(count = n())


# plot of vehicle counts start_date to end date.
ggplot(hour_count, aes(x = speed_hour, y = count)) + 
  geom_line() +
  ggtitle("Vehicles per Hour") +
  labs(x = "Hour", y = "Vehicles") +
  scale_color_brewer(palette = "Set1")

ggplot(speed %>% 
         mutate(speed_hour = floor_date(speed_date, "hour")),
       aes(x = speed_hour, y = ave_speed, group = speed_hour)) + 
        geom_boxplot() +
  ggtitle("Speed Over Time") +
  labs(x = "Hour", y = "Speed (MPH)") 


