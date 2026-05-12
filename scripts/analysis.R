library(tidyverse)
# load the data frame
df <- read.csv("./data/airbnb.csv")

# check for missing values
sum(is.na(df)) # 0

# Measures of Central Tendency
mean_price <- mean(df$price, na.rm = TRUE)
median_price <- median(df$price, na.rm = TRUE)
get_mode <- function(x) {
  unique_values <- unique(x) 
  unique_values[which.max(tabulate(match(x, unique_values)))]
}

mode_price <- get_mode(df$price)

# Measures of Variation
range_price <- diff(range(df$price))
var_price <- var(df$price)
sd_price <- sd(df$price)
cvar_price <- (sd_price / mean_price)*100


cat(
  "MEASURES OF CENTRAL TENDENCY\n",
  "mean  : ", mean_price, "\n",
  "median: ", median_price, "\n",
  "mode  :", mode_price, "\n"
)

cat(
  "MEASURES OF VARIATION\n",
  "range                   : ", range_price, "\n",
  "variation               : ", var_price, "\n",
  "standard deviation      : ", sd_price, "\n",
  "coefficient of variation: ", cvar_price, "\n"
)

# Histogram of price
hist(df$price,
     col = "lightyellow",
     main = "Histogram of Airbnb Prices in Istanbul",
     xlab = "Prices of Airbnb in Istanbul")

# Box plot of price
boxplot(df$price,
        col = "lightyellow",
        main = "Boxplot of Airbnb Prices in Istanbul",
        xlab = "Prices of Airbnb in Istanbul",
        outline = FALSE,
        horizontal = TRUE)

# class boundaries
width <- range_price / 20
print(width) # 385.35, round up to 400
width <- 400

breaks <- seq(0,8000,400)
class_boundaries <- cut(df$price,
                        breaks = breaks,
                        right = FALSE,
                        dig.lab = 5
                        )
frequencies <- hist(df$price,
                    plot = FALSE,
                    breaks = breaks)$counts

class_price <- data.frame(
  "Frequency" = frequencies,
  "Classes" = levels(class_boundaries)
)

class_price







