library(ggplot2)
library(MASS)

# Import data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Input current PB ratio for plotting
current_pb <- 4.25

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

full_data$PB <- 1 / as.numeric(full_data$bm)

# Remove first ten years where PB calculation is missing
data <- full_data[first(which(!is.na(full_data$PB))):last(which(!is.na(full_data$PB))), ]
# Make a new column indicating if there was a correction or a crash during the next year
data <- data %>%
  select(dates, PB, oneyear) %>%
  mutate(correction = as.numeric(oneyear < 0.9),
         crash = as.numeric(oneyear < 0.8)) %>% 
  na.omit()

correction_logreg <- glm(correction ~ PB, data, family = binomial(link = "logit"))
crash_logreg <- glm(crash ~ PB, data, family = binomial(link = "logit"))

# Plot the logistic regression for corrections
data %>%
  ggplot(aes(PB, correction)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = current_pb) +
  annotate("text", x = current_pb + 1, y = 0.6, label = "Current P/B") +
  xlab("P/B") +
  ylab("Probability of correction in the next year")

# Plot the logistic regression for crashes
data %>%
  ggplot(aes(PB, crash)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = current_pb) +
  annotate("text", x = current_pb + 1, y = 0.6, label = "Current P/B") +
  xlab("P/B") +
  ylab("Probability of crash in the next year")

# Calculat odds ratios
exp(cbind(coef(correction_logreg), confint(correction_logreg)))
exp(cbind(coef(crash_logreg), confint(crash_logreg)))
