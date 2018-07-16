library(ggplot2)

# Import data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Input current CAPE ratio for plotting
current_cape <- 32.57

# Recalculate the index without dividends
full_data$index[2] <- full_data$P[1] * full_data$diff[2]
for (i in 1:I(nrow(full_data) - 2)) {
  full_data$index[i + 2] <- full_data$index[i + 1] * full_data$diff[i + 2]
}

# Calculate one year returns
full_data$oneyear <- NA
for (i in 1:I(nrow(full_data) - 1)) {
  full_data$oneyear[i + 1] <- (full_data$index[i + 13] / full_data$index[i + 1])
}

# Remove first ten years where CAPE calculation is missing
data <- full_data[first(which(!is.na(full_data$CAPE))):nrow(full_data), ]
# Make a new column indicating if there was a correction or a crash during the next year
data <- data %>%
  select(dates, CAPE, oneyear) %>%
  mutate(correction = as.numeric(oneyear < 0.9),
         crash = as.numeric(oneyear < 0.8)) %>% 
  na.omit()

correction_logreg <- glm(correction ~ oneyear, data, family = binomial(link = "logit"))
crash_logreg <- glm(crash ~ oneyear, data, family = binomial(link = "logit"))

# Plot the logistic regression for corrections
data %>%
  ggplot(aes(CAPE, correction)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = current_cape) +
  annotate("text", x = current_cape + 3.2, y = 0.6, label = "Current CAPE") +
  xlab("CAPE") +
  ylab("Probability of correction in the next year")

# Plot the logistic regression for crashes
data %>%
  ggplot(aes(CAPE, crash)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = current_cape) +
  annotate("text", x = current_cape + 3.2, y = 0.6, label = "Current CAPE") +
  xlab("CAPE") +
  ylab("Probability of crash in the next year")
