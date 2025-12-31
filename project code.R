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

# A
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

# B
# 3.1.2.1  Selecting an Extra Baggage and Booking Completion 
table_baggage <- table(df$wants_extra_baggage, df$booking_complete) 
dimnames(table_baggage) <- list( "Extra Baggage" = c("No", "Yes"),"Booking Completed" 
                                 = c("No", "Yes")) 
print(table_baggage) 
chisq.test(table_baggage) 
# X-squared = 423.32, df = 1, p-value < 2.2e-16  
# p < 0.05: Reject H₀ → there is a significant association between wants_extra_baggage and booking completion.

prop.table(table_baggage, 1) 
# Customers who select extra baggage are more likely to complete their bookings
# compared to those who did not select extra baggage. 

model <- glm(booking_complete ~ wants_extra_baggage, data = df, family = "binomial") 
summary(model)
# Customers who select extra baggage (compared to those who don’t) have significantly higher odds of completing the booking. 
# Specifically, selecting extra baggage increases the log odds of booking completion by 0.70960, 
# which translates to an odds ratio of e^0.7096 = 2.033. meaning customers who select baggage are about 2 times more likely to complete their booking. 


# Repeating Tests for Preferred Seating 
table_seat <- table(df$wants_preferred_seat, df$booking_complete) 
dimnames(table_seat) <- list("Preferred Seat" = c("No", "Yes"),"Booking Completed" = 
                               c("No", "Yes")) 
print(table_seat) 
chisq.test(table_seat) 
# X-squared = 162.96, df = 1, p-value < 2.2e-16 
# p < 0.05: Reject H₀ → there is a significant association between the  wants_preferred_seat and booking completion
prop.table(table_seat, 2) 
# Customers who select preferred seating are more likely to complete their bookings 
# compared to those who did not select preferred seating. 

model_seat <- glm(booking_complete ~ wants_preferred_seat, data = df, family = 
                    "binomial") 
summary(model_seat) 
# Customers who select preferred seating (compared to those who don’t) have significantly higher odds of completing the booking. 
# Specifically, selecting preferred seating increases the log odds of booking completion by 0.44684, 
# which translates to an odds ratio of e^0.44684 = 1.563. meaning customers who select preferred seating are about 1.56 times more likely to complete their booking.

# Repeating Tests for In-Fight Meals  
table_meal <- table(df$wants_in_flight_meals, df$booking_complete)


dimnames(table_meal) <- list(  "In-flight Meals" = c("No", "Yes"),  "Booking Completed" = 
                                 c("No", "Yes")) 
print(table_meal) 
chisq.test(table_meal) 
# X-squared = 132.53, df = 1, p-value < 2.2e-16 
# p < 0.05: Reject H₀ → there is a significant association between wants_in_flight_meals and booking completion.

prop.table(table_meal, 1)
# Customers who select flight meals are more likely to complete their bookings (17.0%) 
# compared to those who did not select extra baggage (23.1%). 

model_meal <- glm(booking_complete ~ wants_in_flight_meals, data = df, family = 
                    "binomial") 
summary(model_meal) 
# Customers who select wants_in_flight_meals (compared to those who don’t) have 
# significantly higher odds of completing the booking. 
# Specifically, selecting flight meals increases the log odds of booking completion by 0.38759, 
# which translates to an odds ratio of e^0.38759 = 1.47. meaning customers who select 
# flight meals are about 1.47 times more likely to complete their booking.

model_prefernce <- glm(booking_complete ~ wants_extra_baggage 
                       +wants_in_flight_meals+wants_preferred_seat, data = df, family = "binomial") 
summary(model_prefernce) 

# 3.1.2.2: Sales Channel, Booking Origin and Booking Completion 
table_channel <- table(df$sales_channel, df$booking_complete) 
dimnames(table_channel) <- list("Sales Channel" = unique(df$sales_channel),"Booking 
Completed" = c("No", "Yes"))
print(table_channel) 
chisq.test(table_channel) 
# X-squared = 80.764, df = 1, p-value < 2.2e-16 
# p < 0.05: Reject H₀ → there is a significant association between the sales_channel and booking completion.
prop.table(table_channel, 1) 
# Customers who book via Internet (web/desktop) are more likely to complete their 
# bookings compared to those using Mobile. 

model_channel <- glm(booking_complete ~ sales_channel, data = df, family = "binomial") 
summary(model_channel) 

# Customers who book through internet (compared to those who book through mobile) 
# have significantly higher odds of completing the booking. 
# Specifically, booking through internet increases the log odds of booking completion by 0.47625, 
# which translates to an odds ratio of e^ -0.47625 = 0.621. meaning customers who book through internet are about 1/0.621 = 1.61 times more likely to complete their booking.

# booking_origin 
table_origin <- table(df$booking_origin, df$booking_complete) 
chisq.test(table_origin) 
# X-squared = 1882.3, df = 13, p-value < 2.2e-16 
# p < 0.05: Reject H₀ → there is a significant association between the booking_origin and booking completion. 

prop.table(table_origin, 1) 
model_origin <- glm(booking_complete ~ booking_origin, data = df, family = "binomial") 
summary(model_origin) 

# 3.1.2.2: Purchase lead time  
ggplot(df, aes(x = purchase_lead)) + 
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") + 
  facet_wrap(~ booking_complete, ncol = 1) + 
  ggtitle("Distribution of Purchase Lead by Booking Completion") 

qqnorm(df$purchase_lead[df$booking_complete == 1], main = "QQ Plot - Completed") 
qqline(df$purchase_lead[df$booking_complete == 1], col = "red") 

qqnorm(df$purchase_lead[df$booking_complete == 0], main = "QQ Plot - Not 
Completed") 
qqline(df$purchase_lead[df$booking_complete == 0], col = "red") 

shapiro.test(df$purchase_lead[df$booking_complete == 1]) 
shapiro.test(df$purchase_lead[df$booking_complete == 0]) 

# try log Add small constant to avoid log(0) issues 
df$purchase_lead_log <- log(df$purchase_lead + 1)

# Shapiro-Wilk test 
shapiro.test(df$purchase_lead_log[df$booking_complete == 1]) 

qqnorm(df$purchase_lead_log[df$booking_complete == 0]) 
qqline(df$purchase_lead_log[df$booking_complete == 0], col = "red") 

# still not normal, Use the non-parametric test:  Wilcoxon rank-sum test in R 
wilcox.test(purchase_lead ~ booking_complete, data = df)  
# H₀: distribution of purchase_lead is the same for customers who completed their booking and those who didn’t. 
# H₀: distribution of purchase_lead is different between the two groups. 
# W = 42602098, p-value = 0.4746 
# not sufficient evidence to reject H₀, meaning we found no evidence that purchase lead time differs between those who complete their booking and those who don’t. 

model_purchase_lead <- glm(booking_complete ~ purchase_lead, data = df, family = 
                             "binomial") 
summary(model_purchase_lead) 

# Repeating Tests for Flight Duration 
wilcox.test(flight_duration ~ booking_complete, data = df)  
# H₀: distribution of flight duration is the same for customers who completed their booking and those who didn’t. 
# H₀: distribution of flight duration is different between the two groups. 
# W = 45453984, p-value = 6.81e-15 
# p-value < 0.05 , we reject H₀, Significant difference in the distribution of flight duration between completers and non-completers 

model_flight_duration <- glm(booking_complete ~ flight_duration, data = df, family = 
                               "binomial") 
summary(model_flight_duration)

# Customers who booked flights with higher flight duration (compared to those who have lower flight duration) have significantly lower odds of completing the booking. 
# Specifically, those customers decreases the log odds of booking completion by 0.10825 , 
# which translates to an odds ratio of e^ -0.10825 = 0.8974. meaning customers who has higher flight hour are about 10.2% times less likely to complete their booking.

# Repeating Tests for Length of Stay  
wilcox.test(length_of_stay ~ booking_complete, data = df)  
# H₀: distribution of length of stay is the same for customers who completed their booking and those who didn’t. 
# H₀: distribution of length of stay is different between the two groups. 
# W = 40674354, p-value = 2.857e-05 
# p-value < 0.05, we reject H₀, Significant difference in length_of_stay between completers and non-completers 

model_length_of_stay <- glm(booking_complete ~ length_of_stay, data = df, family = 
                              "binomial") 
summary(model_length_of_stay) 
# Customers who booked flights with to have longer length of stay (compared to those who has lower length of stay) have significantly higher odds of completing the booking. 
# Specifically, those customers decreases the log odds of booking completion by 0.05417, 
# which translates to an odds ratio of e^ 0.05417  = 1.055. meaning customers who has higher flight hour are about 1.055 times less likely to complete their booking

# C
# 3.2.2.1 Preference vs Sales Channel 

# 3.2.2.1a Extra Baggage vs Sales Channel 
sales_baggage <- table(df$sales_channel, df$wants_extra_baggage) 
chisq.test(sales_baggage) 
# H₀: The row and column variables are independent. 
# H₀: They are associated (not independent). 
# X-squared = 19.316, df = 1, p-value = 1.107e-05 
# p < 0.05: Reject H₀ → there is a significant association between the wants_extra_baggage and sales_channel. 

prop.table(sales_baggage, 1) 

# Buyers who purchase their tickets via the internet are slightly more likely to purchase 
# extra baggage compared to those who use the mobile channel. Although this relationship 
# is statistically significant, the practical difference is relatively small—approximately 4.09%. 

model <- glm(wants_extra_baggage ~ sales_channel, data = df, family = binomial) 
summary(model) 

# Customers who have book through mobile (compared to those who book via internet) 
# have significantly lower odds of selecting extra baggage. 
# Specifically, those customers book via internet decreases the log odd of selecting extra 
# baggage by 0.16432, 
# which translates to an odds ratio of e^-0.16432 = 0.8484 . meaning customers who purchases 
# through mobile are about 0.1515 times less likely to selecting extra baggage. 

# 3.2.2.1b Preferred Seat vs Sales Channel 
sales_seat <- table(df$sales_channel, df$wants_preferred_seat) 
chisq.test(sales_seat) 
# H₀: The row and column variables are independent. 
# H₀: They are associated (not independent). 
# X-squared = 23.523, df = 1, p-value = 1.234e-06 
# p < 0.05: Reject H₀ → there is a significant association between the wants_preferred_seat and sales_channel. 

prop.table(sales_seat, 1) 
model <- glm(wants_preferred_seat ~ sales_channel, data = df, family = binomial) 
summary(model) 
# Customers who have book through mobile (compared to those who book via internet) 
# have significantly lower odds of selecting extra baggage. 
# Specifically, those customers book via internet increases the log odd of selecting extra baggage by 0.19499, 
# which translates to an odds ratio of e^0.19499 = 1.2152 . meaning customers who purchase 
# the bookings through mobile are about 1.2152 times more likely to selecting extra baggage.

# 3.2.2.1c In Flight Meal vs Sales Channel 
sales_meal <- table(df$sales_channel, df$wants_in_flight_meals) 
chisq.test(sales_meal) 
# H₀: The row and column variables are independent. 
# H₀: They are associated (not independent). 
# X-squared = 5.5587, df = 1, p-value = 0.01839 
# p < 0.05: Reject H₀ → there is a significant association between the wants_extra_baggage and sales_channel. 

prop.table(sales_meal, 1) 
 

model <- glm(wants_in_flight_meals ~ sales_channel, data = df, family = binomial) 
summary(model)
# Customers who have book through mobile (compared to those who book via internet) 
# have significantly lower odds of selecting extra baggage. 
# Specifically, those customers book via internet decreases the log odd of selecting extra baggage by 0.09266, 
# which translates to an odds ratio of e^-0.09266 = 0.3958 . meaning customers who has purchases bookings through mobile are about 0.604 times less likely to select extra baggage.

# 3.2.2.2 Preference vs Length of Stay 

# 3.2.2.2a Extra Baggage vs Length of Stay 
wilcox.test(length_of_stay ~ wants_extra_baggage, data = df) 
# H₀: distribution of length of stay is the same for customers who select wants_extra_baggage and those who didn’t. 
# H₀: distribution of length of stay is different between the two groups. 
# W = 56599314, p-value < 2.2e-16 
# p-value < 0.05, we reject H₀, Significant difference in length_of_stay between those who select wants extra baggage and those who don't. 
 
model <- glm(wants_extra_baggage ~ length_of_stay, data = df, family = binomial) 
summary(model) 
# Customers who have longer length of stay (compared to those who has lower length of stay) have significantly higher odds of selecting extra baggage. 
# Specifically, those customers increase the log odd of selecting extra baggage by 0.25330, 
# which translates to an odds ratio of e^ 0.25330 = 1.2882. meaning with every one unit increase
# in length of stay, customer are about 1.2882 times more likely to buy extra baggage. 

# 3.2.2.2b Flight meals vs Length of Stay 
wilcox.test(length_of_stay ~ wants_in_flight_meals, data = df) 
# H₀: distribution of length of stay is the same for customers who select 
# wants_in_flight_meals and those who didn’t. 
# H₀: distribution of length of stay is different between the two groups. 
# W = 61285443, p-value = 3.367e-05 
# p-value < 0.05, we reject H₀, Significant difference in length_of_stay between those 
# who select wants flight meals and those who don't.

model <- glm(wants_in_flight_meals ~ length_of_stay, data = df, family = binomial)
summary(model) 
# Customers who have longer length of stay (compared to those who has lower length of 
# stay) have significantly higher odds of selecting flight meals. 
# Specifically, those customers increase the log odds of booking completion by 0.04333, 
# which translates to an odds ratio of e^ 0.04333 = 1.0442. meaning customers who has 
# higher length of stay are about 1.0442 times more likely to selecting flight meals.

# 3.2.2.2c Preferred seats vs Length of Stay 
wilcox.test(length_of_stay ~ wants_preferred_seat, data = df) 
# H₀: distribution of length of stay is the same for customers who select 
# wants_preferred_seat and those who didn’t. 
# H₀: distribution of length of stay is different between the two groups. 
# W = 55596609, p-value = 0.8217 
# p-value > 0.05, we do not sufficient evidence to reject H₀,there is no significant 
# difference in length_of_stay between those who select wants preferred seat and those who don't. 

#3.2.2.3 Preference vs Purchase Lead 

# 3.2.2.3a Extra Baggage vs Purchase lead 
wilcox.test(purchase_lead ~ wants_extra_baggage, data = df) 
# H₀: distribution of purchase lead is the same for customers who select 
# wants_extra_baggagee and those who didn’t. 
# H₀: distribution of purchase lead is different between the two groups. 
# W = 62598857, p-value < 2.2e-16 
# p-value < 0.05, we reject H₀, Significant difference in purchase_lead between those 
# who select wants extra baggage and those who don't. 
 
model <- glm(wants_extra_baggage ~ purchase_lead, data = df, family = binomial) 
summary(model) 
# Customers who have longer purchase lead (compared to those who has lower purchase 
# lead) have significantly higher odds of selecting extra baggage. 
# Specifically, those customers increase the log odds of selecting extra baggage by 0.0007397, 
# which translates to an odds ratio of e^ 0.0007397 = 1.0007. meaning for every one unit increase in purchase lead
# customers are about 1.0007 times more likely to selecting extra baggage.

# 3.2.2.3b Flight meals vs Purchase lead 
wilcox.test(purchase_lead ~ wants_in_flight_meals, data = df) 
# H₀: distribution of purchase lead is the same for customers who select wants_flight_meals and those who didn’t. 
# H₀: distribution of purchase lead is different between the two groups. 
# W = 63571656, p-value = 0.5432 
# p-value > 0.05, we do not sufficient evidence to reject H₀,there is no significant 
# difference in purchase lead between those who select wants flight meals and those who don't. 

# 3.2.2.3c Preferred seat vs Purchase lead 
wilcox.test(purchase_lead ~ wants_preferred_seat, data = df) 
# H₀: distribution of purchase lead is the same for customers who select wants_preferred_seat and those who didn’t. 
# H₀: distribution of purchase lead is different between the two groups. 
# W = 56645707, p-value = 0.04174 
# p-value < 0.05, we reject H₀,there is significant difference in purchase lead between 
# those who select wants preferred seat and those who don't. 

model <- glm(wants_preferred_seat ~ purchase_lead, data = df, family = binomial) 
summary(model) 
# Customers who have longer purchase lead (compared to those who has lower purchase 
# lead) have significantly lower odds of selecting preferred seats. 
# Specifically, those customers decrease the log odds of selecting preferred seats by 0.0006103, 
# which translates to an odds ratio of e^-0.0006103 = 0.9994. meaning customers who 
# has higher purchase lead are about 0.0006 times less likely to selecting extra baggage. 

# 3.2.2.4 Preference vs Flight Duration 

# 3.2.2.4a Extra Baggage vs Flight Duration 
wilcox.test(flight_duration ~ wants_extra_baggage, data = df) 
# H₀: distribution of flight duration is the same for customers who select wants_extra_baggage and those who didn’t. 
# H₀: distribution of flight duration is different between the two groups. 
# W = 68027369, p-value = 0.3206 
# p-value > 0.05, we do not sufficient evidence to reject H₀,there is no significant 
# difference in flight duration between those who select wants_extra-baggage and those who don't. 

# 3.2.2.4b Preferred seat vs Flight Duration 
wilcox.test(flight_duration ~ wants_preferred_seat, data = df) 
# H₀: distribution of flight duration is the same for customers who select wants_preferred_seat and those who didn’t. 
# H₀: distribution of flight duration is different between the two groups. 
# W = 50000686, p-value < 2.2e-16 
# p-value < 0.05, we reject H₀,there is significant difference in flight duration between 
# those who select wants preferred seat and those who don't. 
 
model <- glm(wants_preferred_seat ~ flight_duration, data = df, family = binomial) 
summary(model) 
# Customers who have longer flight duration (compared to those who has lower flight 
# duration) have significantly higher odds of selecting preferred seats. 
# Specifically, those customers increase the log odds of selecting preferred seats by 0.12578, 
# which translates to an odds ratio of e^0.12578 = 1.134. meaning for every increase in flight duration, 
# customer's are about 1.134 times more likely to purchase preferred seats. 

# 3.2.2.4c Flight meals vs Flight Duration 
wilcox.test(flight_duration ~ wants_in_flight_meals, data = df) 
# H₀: distribution of flight duration is the same for customers who select wants_in_flight_meals and those who didn’t. 
# H₀: distribution of flight duration is different between the two groups. 
# W = 55327694, p-value < 2.2e-16 
# p-value < 0.05, we reject H₀,there is significant difference in flight duration between 
# those who select wants flight meals and those who don't. 
 
model <- glm(wants_in_flight_meals ~ flight_duration, data = df, family = binomial) 
summary(model)
# Customers who have longer flight duration (compared to those who has lower flight 
# duration) have significantly more odds of selecting flight meals. 
# Specifically, those customers increase the log odds of selecting flight meals by 0.16822, 
# which translates to an odds ratio of e^0.16822 = 1.183. meaning for every unit increase in 
# customer's flight duration, they are about 1.183 times more likely to purchase flight meal

