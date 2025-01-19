library("isotree")
library('ggplot2')

seed = 120397

data <- read.csv('C:/Users/utente/OneDrive/Documenti/Dati/realKnownCause/realKnownCause/rogue_agent_key_hold.csv')
mean(data$value)

#plotting

data$timestamp <- as.POSIXct(data$timestamp, format = "%Y-%m-%d %H:%M:%S")

ggplot(data, aes(x = timestamp, y = value)) +
  geom_line(color = "blue") +
  ggtitle("Time Series Plot") +
  xlab("Timestamp") +
  ylab("Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#features engeneering
#day of week
data$day_of_week <- as.numeric(format(data$timestamp, "%u"))
data$day_of_week_label <- weekdays(data$timestamp)

#night or day
data$hour <- as.numeric(format(data$timestamp, "%H"))
data$day_or_night <- ifelse(data$hour >= 7 & data$hour < 18, "Day", "Night")

#working day
data$is_working_day <- ifelse(data$day_of_week >= 1 & data$day_of_week <= 5, "Yes", "No")

iso <- isolation.forest(data[, c('value', 'day_of_week_label', 'is_working_day', 'day_or_night')],
                        sample_size = 250, 
                        ntrees = 100)

outlier_scores <- predict(iso, data[, c('value', 'day_of_week_label', 'is_working_day', 'day_or_night')], type = "score")
summary(outlier_scores)
data$outlier_score <- outlier_scores
threshold <- quantile(outlier_scores, 0.95)

data$is_outlier <- ifelse(data$outlier_score >= threshold, "Yes", "No")

ggplot(data, aes(x = 1:nrow(data), y = outlier_score, color = is_outlier)) +
  geom_point() +
  labs(title = "Outlier Scores", x = "Record Index", y = "Outlier Score") +
  scale_color_manual(values = c("Yes" = "red", "No" = "blue"))

table(data$is_outlier)


