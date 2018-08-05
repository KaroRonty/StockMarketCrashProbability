library(ggplot2)
library(MASS)

# Import data from Shiller & Goyal
source("https://raw.githubusercontent.com/KaroRonty/ShillerGoyalDataRetriever/master/ShillerGoyalDataRetriever.r")

# Input current PE ratio for plotting
current_pe <- 23.15

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

full_data$PE <- as.numeric(full_data$P / full_data$E)

# Remove first ten years where PE calculation is missing
data <- full_data[first(which(!is.na(full_data$PE))):last(which(!is.na(full_data$PE))), ]
# Make a new column indicating if there was a correction or a crash during the next year
data <- data %>%
  select(dates, PE, oneyear) %>%
  mutate(correction = as.numeric(oneyear < 0.9),
         crash = as.numeric(oneyear < 0.8)) %>% 
  na.omit()

correction_logreg <- glm(correction ~ PE, data, family = binomial(link = "logit"))
crash_logreg <- glm(crash ~ PE, data, family = binomial(link = "logit"))

# Plot the logistic regression for corrections
data %>%
  ggplot(aes(PE, correction)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = current_pe) +
  annotate("text", x = current_pe + 15, y = 0.6, label = "Current P/E") +
  xlab("P/E") +
  ylab("Probability of correction in the next year")

# Plot the logistic regression for crashes
data %>%
  ggplot(aes(PE, crash)) +
  geom_point(alpha = .1) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  geom_vline(xintercept = current_pe) +
  annotate("text", x = current_pe + 15, y = 0.6, label = "Current P/E") +
  xlab("P/E") +
  ylab("Probability of crash in the next year")

# Calculat odds ratios
exp(cbind(coef(correction_logreg), confint(correction_logreg)))
exp(cbind(coef(crash_logreg), confint(crash_logreg)))
