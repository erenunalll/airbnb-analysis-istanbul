# load the data frame
df <- read.csv("./data/airbnb.csv")

# Central tendencies
mean_price <- mean(df$price, na.rm = TRUE)
median_price <- median(df$price, na.rm = TRUE)
get_mode <- function(x) {
  unique_values <- unique(x)
  unique_values[which.max(tabulate(match(x, unique_values)))]
}
mode_price <- get_mode(df$price)
