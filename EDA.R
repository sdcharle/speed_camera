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
db_path = "speed_app/speed_cam.db"

db <- RSQLite::dbConnect(RSQLite::SQLite(), 
                         dbname = db_path)
speed <- odbc::dbGetQuery(db, 'select * from speed ')

speed <- speed %>% mutate(speed_date = ymd_hms(log_timestamp))

begin_date = '2024-07-02'
min_speed = 12

speed <- speed %>% 
  filter(speed_date >= ymd(begin_date) & 
           ave_speed >= min_speed & 
           image_path != 'media/images/speed-20240711-0720/speed-98-20240711-1430377.jpg') 

ggplot(speed, aes(x = ave_speed)) + 
  #geom_histogram(bins = 1000) +  
  geom_density() +
  geom_vline(xintercept = 30, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 40, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density Plot of Average Speed", x = "Average Speed", y = "Density") +
  theme_minimal()

ggplot(speed, aes(x = ave_speed, group = direction, color = direction)) + 
  geom_boxplot(notch = TRUE)  + 
  geom_vline(xintercept = 30, color = "green", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 40, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Density Plot of Average Speed", x = "Average Speed", y = "Density") +
  theme_minimal()

# oops L2R is slower (consistent with my findings)

ggplot(speed, aes(x = m_area)) + geom_histogram()
# area - 4000????
# bimodal widths. Big trucks > 50????
ggplot(speed, aes(x = mw)) + geom_histogram()
ggplot(speed, aes(x = mh)) + geom_histogram()
# some trucks showin' up as 16, boooo.....
# 40 seems good for height but .... shadows

# even 90 x 40 cuttin it close?

# problems
# shadows
# nighttimes
# multiple vehicles

# prob DOOO need compu viz

ggplot(speed %>% dplyr::sample_frac(prop = 0.01), aes(x = speed_date, y = ave_speed, alpha = 0.01)) + geom_point()

# mixtools not as robust, so..

set.seed(42)
data = speed
# around 40K shit is slow
#sample_indices <- sample(1:nrow(data), size = 20000) # Adjust size as needed
data_sample <- data #[sample_indices, ]

# Fit the mclust model on the sampled data
mclust_model <- Mclust(data_sample %>% select(ave_speed), G=2)
labels <- mclust_model$classification
# Calculate Silhouette Score
sil_score <- silhouette(labels, dist(data_sample %>% select(ave_speed)))
mean_silhouette <- mean(sil_score[, 3])
print(paste("Mean Silhouette Score:", mean_silhouette))

summary(mclust_model)

mclust_model$parameters$mean
mclust_model$parameters$variance
#plot(mclust_model, what = "uncertainty")
plot(mclust_model, what = "classification")
plot(mclust_model, what = "density")

data_sample$label = labels

ggplot(data_sample, aes(x = ave_speed, group = label,color = label)) + 
  geom_density() + 
  theme_minimal()

# Fit the mclust model on the sampled data
mclust_model <- Mclust(data_sample %>% select(ave_speed), G=1)
labels <- mclust_model$classification

summary(mclust_model)
plot(mclust_model, what = "density")
mclust_model$parameters$mean
# 33.4
mclust_model$parameters$variance
plot(mclust_model, what = "classification")

# 2 BIC -279182.7
# 1 BIC -280423.9

# ok OK, 1 is best but by the slightest margin. Silhouette score is not bad!

"

Initialization:

Mclust: Uses a combination of hierarchical clustering and the Expectation-Maximization (EM) algorithm for initialization.
normalmixEM: Uses a different initialization method for the EM algorithm, which can lead to different starting points.
Optimization Algorithms:

Both packages use the EM algorithm, but the specific implementation details and convergence criteria can differ, leading to slightly different parameter estimates.
Model Assumptions:

Mclust: Automatically selects the best model based on the Bayesian Information Criterion (BIC) and can include different covariance structures.
normalmixEM: Typically fits a model with equal covariance matrices unless specified otherwise.
To see the differences, let's run a comparison with both packages:

"

# detected things

things <- read_csv("data/detections0.7.csv")
things <- things %>%
  separate(`Detected Classes`, into = c("object", "width", "height"), sep = ",", convert = TRUE)

ggplot(things, aes(x = width)) + geom_density()
ggplot(things, aes(x=width, color = object, group = object)) + geom_boxplot()
ggplot(things, aes(x = height)) + geom_density()
ggplot(things, aes(x=height, color = object, group = object)) + geom_boxplot()

# height gt...100????

# width....200 seems...most cars? all under 300.

# 250 nope
# 300 ok ish. This is not quite doin' it

for (x in (things %>% arrange(desc(width)) %>% filter(width > 300))$Filename ) {
  print(x)
  img <- readJPEG(glue("/Users/sdcharle/Desktop/speedy/imgfiles/speed-20240704-1013/{x}"))
  grid.newpage()
  # Display the image
  grid::grid.raster(img)
  # Pause to allow viewing the image before the next one is displayed
  Sys.sleep(2)  # Pause for 2 seconds, adjust as needed
}
# have 'verified' cars are always(?) not trucks
subthings <- things %>% 
  filter(object %in% c('truck','train', 'boat')) %>% 
  arrange((height)) %>% filter(height > 100)

print(nrow(subthings))

for (x in subthings$Filename ) {
  print(x)
  #x = "speed-33-20240704-1643396.jpg"
  img <- readJPEG(glue("/Users/sdcharle/Desktop/speedy/imgfiles/speed-20240704-1013/{x}"))
  grid.newpage()
  # Display the image
  grid::grid.raster(img)
  # Pause to allow viewing the image before the next one is displayed
  Sys.sleep(2)  # Pause for 2 seconds, adjust as needed
}

# height is better but shadows be foolin it!
# 75 for sure not high enuff
# 100 is nice but still shadow false positives



# ave speeds

# things for shiny

summary(speed)
hour(speed[1]$speed_date)

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


top10 <- speed %>% 
  arrange(desc(ave_speed)) %>% 
  head(10) 

top10 %>% select(log_timestamp, ave_speed, image_path, mw,mh)

# woah wait 97.8 is a tractor?!?!?!?!?
