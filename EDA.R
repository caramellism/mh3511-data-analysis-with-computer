# load data
getwd()
setwd("C:\\Users\\N7N0L\\OneDrive\\Desktop\\MH3511")
df <- read.csv('customer_booking.csv')

# check for missing values
str(df)
any(is.na(df)) #FALSE -> no missing values

# convert binary categorical variables into factor 
df$wants_extra_baggage <- factor(df$wants_extra_baggage, levels = c(0, 1))
df$wants_preferred_seat <- factor(df$wants_preferred_seat, levels = c(0, 1))
df$wants_in_flight_meals <- factor(df$wants_in_flight_meals, levels = c(0, 1))

# load libraries
library(dplyr)
library(ggplot2)

# 2.3 Visualization of distributions & summary statistics

# 2.3.1 Summary statistics for the main variable of num_passengers 
# Histogram
ggplot(df, aes(x = num_passengers)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Histogram of num_passengers") # original data is heavily right skewed
# Boxplot
ggplot(df, aes(y = num_passengers)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of num_passengers")
# Summary
summary(df$num_passengers)

# 2.3.2 Summary statistics for the main variable of purchase_lead 
# Histogram
ggplot(df, aes(x = purchase_lead)) +
  geom_histogram(binwidth = 45, fill = "skyblue", color = "black") +
  ggtitle("Histogram of purchase_lead")
# Boxplot
ggplot(df, aes(y = purchase_lead)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of purchase_lead")
# Summary
summary(df$purchase_lead)

# 2.3.3 Summary statistics for the main variable of length_of_stay
# Histogram
ggplot(df, aes(x = length_of_stay)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Histogram of length_of_stay")
# Boxplot
ggplot(df, aes(y = length_of_stay)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of length_of_stay")
# Summary
summary(df$length_of_stay)

# 2.3.4 Summary statistics for the main variable of flight_duration
# Histogram
ggplot(df, aes(x = flight_duration)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  ggtitle("Histogram of flight_duration")
# Boxplot
ggplot(df, aes(y = flight_duration)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of flight_duration")
# Summary
summary(df$flight_duration)

# 2.3.5 Summary statistics for the main variable of sales_channel and trip_type
# Bar plot of sales_channel
ggplot(df, aes(x = sales_channel)) +
  geom_bar(fill = "skyblue") + 
  labs(title = "Sales Channel", x = "sales channel", y = "count") 
# Summary of sales_channel
table(df$sales_channel) 

# Bar plot of trip_type
ggplot(df, aes(x = trip_type)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Trip Type", x = "trip type", y = "count") 
# Summary of trip_type
table(df$trip_type) 

# 2.3.6 Summary statistics for the main variable of booking_origin
top10 <- df %>%
  count(booking_origin, sort = TRUE) %>%
  slice_head(n = 10) %>%
  pull(booking_origin) # top 10 out of 104 countries with most bookings

df1 <- df %>% mutate(origin_grouped = ifelse(booking_origin %in% top10, booking_origin,
                                             "Other")) 
origin_summary <- df1 %>%
  count(origin_grouped) %>%
  arrange(desc(n))

n_other_countries <- df1 %>%
  filter(!(booking_origin %in% top10)) %>%
  summarise(n = n_distinct(booking_origin)) %>%
  pull(n)

ggplot(origin_summary, aes(x = reorder(origin_grouped, n), y = n,
                           fill = ifelse(origin_grouped == "Other", "Other", "Top 10"))) +
  geom_bar(stat = "identity") +
  geom_text(data = origin_summary %>% filter(origin_grouped == "Other"),
            aes(label = paste0("Includes total of ", n_other_countries, " countries")),
            hjust = -0.05, size = 4, fontface = "bold", color = "tomato") +
  scale_fill_manual(values = c("Top 10" = "skyblue", "Other" = "tomato")) +
  coord_flip(clip = "off") +
  labs(
    title = "Top 10 Booking Origins + Other",
    x = "Booking Origin",
    y = "Number of Bookings",
    fill = "Group"
  ) + theme_minimal() +
  theme(plot.margin = margin(5.5, 50, 5.5, 5.5)) # extra right margin for label

# 2.3.7 Summary statistics for the main variable of Preferences
# Bar plot of wants_extra_baggage
barplot(table(df$wants_extra_baggage), main= "Extra Baggage Preference", col =
          "skyblue")
# Summary of wants_extra_baggage
table(df$wants_extra_baggage) 

# Bar plot of wants_in_flight_meal
barplot(table(df$wants_in_flight_meals), main= "In Flight Meal Preference", col =
          "skyblue") 
# Summary of wants_in_flight_meal
table(df$wants_in_flight_meals) 

# Bar plot of wants_preferred_seat
barplot(table(df$wants_preferred_seat), main= "In Flight Meal Preference", col =
          "skyblue") 
# Summary of wants_preferred_seat
table(df$wants_preferred_seat) 

# 2.3.8 Summary statistics for the main variable of booking_complete
# Bar plot of booing_complete
barplot(table(df$booking_complete), main= "Booking Complete", col = "skyblue")
# Summary of booking_complete
table(df$booking_complete) 

# 2.4 Data Cleaning
# Histogram of length_of_stay with zoomed-in view (Before/Uncleaned)
ggplot(df, aes(x = length_of_stay)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  scale_x_continuous(limits = c(0, 40), breaks = seq(0, 40, by = 2)) + # Adjust limits for zoom
labs(title = "Histogram of Length of Stay (Zoomed In)",
     x = "Length of Stay (days)",
     y = "Frequency") +
  theme_minimal() +
  geom_vline(xintercept = c(7, 16), color = "red", linetype = "dashed", linewidth = 1)

# Histogram of length_of_stay (After/Cleaned)
df1 <- df %>%
  filter(length_of_stay >= 0 & length_of_stay <= 6) # short term stay only

ggplot(df1, aes(x = length_of_stay)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 6, by = 1)) + # Set breaks
  labs(title = "Histogram of Length of Stay (0 to 6 days)",
       x = "Length of Stay (days)",
       y = "Frequency") +
  theme_minimal()

total_count <- nrow(df1)
print(total_count)
# 24673 records remained after deletion

# 2.4.2 Outlier Removal: purchase_lead
# Box plot of purchase_lead (Before/Uncleaned)
ggplot(df, aes(y = purchase_lead)) +
  geom_boxplot(fill = "pink", color = "black", outlier.color = "red") +
  labs(title = "Boxplot of Purchase Lead Time",
       y = "Purchase Lead (days)") +
  theme_minimal()

# Box plot of purchase_lead (After/Cleaned)
df1 <- df %>%
  filter(length_of_stay >= 0 & length_of_stay <= 6) %>% # short stays only
  filter(purchase_lead <= 365) #filter out outliers

ggplot(df1, aes(y = purchase_lead)) +
  geom_boxplot(fill = "pink", color = "black", outlier.color = "red") +
  labs(title = "Boxplot of Purchase Lead Time",
       y = "Purchase Lead (days)") +
  theme_minimal()

# 2.4.3 Filtering of Countries
top14 <- df1 %>%
  count(booking_origin, sort = TRUE) %>%
  slice_head(n = 14)

ggplot(top14, aes(x = n, y = reorder(booking_origin, n))) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top 14 Booking Origins",
       x = "Count",
       y = "Booking Origin") +
  theme_minimal()

# Finalized Cleaning
df <- df %>%
  filter(length_of_stay >= 0 & length_of_stay <= 6) %>% # short stays (0-6days)only
  filter(purchase_lead <= 365) %>% # remove extreme purchase lead
  select(-route, -flight_hour) # drop unused columns
# get top 14 countries
top14 <- df %>% count(booking_origin, sort = TRUE) %>% slice_head(n = 14)
df <- df %>% filter(booking_origin %in% top14$booking_origin)

# 2.5 Data Normalization
#2.5 Num_passengers
# qqplot
qqnorm(df$num_passengers, main = "num_passengers qqplot")
qqline(df$num_passengers, col = "red")
# log transformation
df$num_passengers_log <- log(df$num_passengers)
# log transformed histogram
ggplot(df, aes(x = num_passengers_log)) +
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") +
  ggtitle("Histogram of num_passengers_log")
# log transformed boxplot
ggplot(df, aes(y = num_passengers_log)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of num_passengers_log")
# log transformed qqplot
qqnorm(df$num_passengers_log, main = "num_passengers_log qqplot")
qqline(df$num_passengers_log, col = "red")
#still not normal after log transformation

# 2.5 purchase_lead
#qqplot
qqnorm(df$purchase_lead, main = "purchase_lead qqplot")
qqline(df$purchase_lead, col = "red")
# log transformation
df$purchase_lead_log <- log(df$purchase_lead+1)
#log transformed histogram
ggplot(df, aes(x = purchase_lead_log)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  ggtitle("Histogram of purchase_lead_log")
# log transformed boxplot
ggplot(df, aes(y = purchase_lead_log)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of purchase_lead_log")
#log transformed qqplot
qqnorm(df$purchase_lead_log, main = "purchase_lead_log qqplot")
qqline(df$purchase_lead_log, col = "red") 
#still not normal after log transformation

# 2.5 length_of_stay
# qqplot
qqnorm(df$length_of_stay, main = "length_of_stay qqplot") 
qqline(df$length_of_stay, col = "red")
df$exp_length_of_stay <- exp(df$length_of_stay) 
# exp transformed histogram
ggplot(df, aes(x = exp_length_of_stay)) +
  geom_histogram(binwidth = 0.2, fill = "skyblue", color = "black") +
  ggtitle("Histogram of exp_length_of_stay")
# exponential transformation
df$exp_length_of_stay <- exp(df$length_of_stay)
# exp transformed boxplot
ggplot(df, aes(y = exp_length_of_stay)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of exp_length_of_stay")
# exp transformed qqplot
qqnorm(df$exp_length_of_stay, main = "exp_length_of_stay qqplot")
qqline(df$exp_length_of_stay, col = "red")
#still not normal after exponential transformation

# 2.5 flight duration
#qqplot
qqnorm(df$flight_duration,main = "flight_duration qqplot" )
qqline(df$flight_duration, col = "red")
df$log_flight_duration <- log(df$flight_duration)
# log transformation
df$log_flight_duration <- log(df$flight_duration)
# log transformed histogram
ggplot(df, aes(x = log_flight_duration)) +
  geom_histogram(binwidth = 0.15, fill = "skyblue", color = "black") +
  ggtitle("Histogram of log_flight_duration")
# log transformed boxplot
ggplot(df, aes(y = log_flight_duration)) +
  geom_boxplot(fill = "pink") +
  ggtitle("Boxplot of log_flight_duration")
# log transformed qqplot
qqnorm(df$log_flight_duration,main = "log_flight_duration qqplot" )
qqline(df$log_flight_duration, col = "red") 
#still not normal after log transformation


