library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(DataExplorer)
library(GGally)
library(zoo)

# Exploratory Data Analysis (EDA)

# Load Data 

df_train <- read.csv("df_train.csv", stringsAsFactors = FALSE)
df_test  <- read.csv("df_test.csv", stringsAsFactors = FALSE)

# Combine for global EDA 

df <- bind_rows(df_train, df_test)

# Convert date and factor variables

df$date <- mdy(df$date)
df$day_in_week <- factor(df$day_in_week)

# Preview

head(df)
str(df)
summary(df)

# Missing Values

missing_values <- sapply(df, function(x) sum(is.na(x)))
missing_values

plot_missing(df)

# Distribution of Power Consumption

ggplot(df, aes(x = power_consumption)) +
  geom_histogram(bins = 40, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Daily Power Consumption",
    x = "Power Consumption (kW)",
    y = "Frequency"
  ) +
  theme_minimal()

# Time Series Plot

ggplot(df, aes(x = date, y = power_consumption)) +
  geom_line(color = "firebrick") +
  labs(
    title = "Daily Power Consumption Over Time",
    x = "Date",
    y = "Power Consumption (kW)"
  ) +
  theme_minimal()

# Rolling Average (7-day)

df <- df %>% arrange(date)
df$rolling_7d <- rollmean(df$power_consumption, k = 7, fill = NA, align = "right")

ggplot(df, aes(date)) +
  geom_line(aes(y = power_consumption, color = "Daily")) +
  geom_line(aes(y = rolling_7d, color = "7-Day Rolling Avg"), linewidth = 1.2) +
  labs(
    title = "Daily Power Consumption with 7-Day Rolling Mean",
    x = "Date",
    y = "Power Consumption (kW)",
    color = ""
  ) +
  theme_minimal()

# Weekly Seasonality

ggplot(df, aes(x = day_in_week, y = power_consumption)) +
  geom_boxplot(fill = "gold") +
  labs(
    title = "Power Consumption by Day of Week",
    x = "Day of Week",
    y = "Power Consumption (kW)"
  ) +
  theme_minimal()

# Numeric Correlation Matrix

numeric_df <- df %>% select(where(is.numeric))

cor_matrix <- cor(numeric_df, use = "pairwise.complete.obs")
cor_matrix

ggcorr(cor_matrix, label = TRUE)

# Pairwise Relationships

ggpairs(numeric_df, progress = FALSE)

# Outlier Detection

q_low  <- quantile(df$power_consumption, 0.01)
q_high <- quantile(df$power_consumption, 0.99)

outliers <- df %>%
  filter(power_consumption < q_low | power_consumption > q_high)

n_outliers <- nrow(outliers)
n_outliers
head(outliers)

ggplot(df, aes(x = date, y = power_consumption)) +
  geom_point(alpha = 0.4, color = "grey70") +
  geom_point(
    data = outliers,
    aes(x = date, y = power_consumption),
    color = "red",
    size = 2
  ) +
  labs(
    title = "Outlier Detection in Daily Power Consumption",
    x = "Date",
    y = "Power Consumption (kW)"
  ) +
  theme_minimal()

# Relationship with Numeric Predictors

num_cols <- setdiff(names(numeric_df), "power_consumption")

for (col in num_cols) {
  print(
    ggplot(df, aes_string(x = col, y = "power_consumption")) +
      geom_point(alpha = 0.4, color = "dodgerblue") +
      geom_smooth(method = "loess", se = FALSE, color = "darkorange") +
      labs(
        title = paste("Power Consumption vs 7 days rolling mean"),
        x = col,
        y = "Power Consumption (kW)"
      ) +
      theme_minimal()
  )
}

# Density plots per weekday

ggplot(df, aes(x = power_consumption, fill = day_in_week)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Density Plot of Power Consumption by Day of Week",
    x = "Power Consumption (kW)",
    y = "Density"
  ) +
  theme_minimal()
