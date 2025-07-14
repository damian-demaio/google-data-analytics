# Bar chart mean_ride_length_time vs. day_of_week, grouped by usertype
ggplot(mean_ride_length_by_day, aes(fill=usertype, y=mean_ride_length_time, x=day_of_week)) + 
  geom_bar(position="dodge", stat="identity")

# Bar chart number of trips per day vs. day_of_week, grouped by usertype
ggplot(weekday_count_per_day, aes(fill=usertype, y=n, x=day_of_week)) + 
  geom_bar(position="dodge", stat="identity") + scale_y_continuous(
    name = "Number of trips per day")

#Combine data frames mean_ride_length_by_day and weekday_count_per_day
correlation_RN <- merge(mean_ride_length_by_day, weekday_count_per_day)

#Correlations

correlation_customer <- subset(correlation_RN, usertype == "Customer")

correlation_subscriber<- subset(correlation_RN, usertype == "Subscriber")

correlation1 <- cor(correlation_customer$mean_ride_length, correlation_customer$n)

correlation2 <- cor(correlation_subscriber$mean_ride_length, correlation_subscriber$n)

ggplot(correlation_customer, aes(x = correlation_customer$n, y = correlation_customer$mean_ride_length)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Trend line
  annotate("text", x = min(correlation_customer$n), y = max(correlation_customer$mean_ride_length), 
           label = paste("Correlation:", round(correlation1, 2)), 
           hjust = 0, size = 5, color = "darkred") +  # Add correlation text
  labs(
    title = "Correlation between ride length mean and daily number of trips for Customers",
    x = "number_of_trips_per_day",
    y = "mean_ride_length (seconds)"
  ) +
  theme_minimal()

ggplot(correlation_customer, aes(x = correlation_subscriber$n, y = correlation_subscriber$mean_ride_length)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +  # Scatter plot
  geom_smooth(method = "lm", color = "red", linetype = "dashed") +  # Trend line
  annotate("text", x = min(correlation_subscriber$n), y = max(correlation_subscriber$mean_ride_length), 
           label = paste("Correlation:", round(correlation2, 2)), 
           hjust = 0, size = 5, color = "darkred") +  # Add correlation text
  labs(
    title = "Correlation between ride length mean and daily number of trips for Subscribers",
    x = "number_of_trips_per_day",
    y = "mean_ride_length (seconds)"
  ) +
  theme_minimal()
