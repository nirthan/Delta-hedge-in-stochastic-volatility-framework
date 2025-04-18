# Load necessary library
library(xts)
library(broom)

option_data6 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-06.csv")
option_data7 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-07.csv")
option_data8 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-08.csv")
option_data9 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-09.csv")
option_data12 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-12.csv")
option_data13 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-13.csv")
option_data14 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-14.csv")
option_data15 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-15.csv")
option_data16 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-16.csv")
option_data19 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-19.csv")
option_data20 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-20.csv")
option_data21 <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/2022-12-21.csv")

stock_prices

stock_prices <- c(146.63 ,option_data6[1,2], option_data7[1,2], option_data8[1,2], option_data9[1,2], option_data12[1,2], option_data13[1,2], option_data14[1,2], option_data15[1,2], option_data16[1,2], option_data19[1,2], option_data20[1,2], option_data21[1,2], 132.23)
#delta_values <- c(0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648, 0.648) # constant B&S
#delta_values <- c(0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905, 0.6905) # constant Heston
#delta_values <- c(0.6905, 0.6385, 0.6068, 0.6351, 0.6299, 0.6806, 0.6690, 0.6364, 0.5207, 0.4838, 0.4208, 0.3964, 0.4705) # Heston
delta_values <- c(0.648, 0.5894, 0.5576, 0.5862, 0.5772, 0.6208, 0.6295, 0.5947, 0.4783, 0.4381, 0.3893, 0.3784, 0.4425) # B&S

#option_prices <- c(option_data6[8,5], option_data7[8,5], option_data8[8,5], option_data9[8,5], option_data12[8,5], option_data13[8,5], option_data14[8,5], option_data15[8,5], option_data16[8,5], option_data19[8,5], option_data20[8,5], option_data21[8,5])
option_prices <- c(14.918, 12.06, 10.10, 11.02, 11.70, 11.0, 12.62, 11.0, 7.66, 6.31, 4.96, 4.44, 5.80)



# Posision wanted to hedge
initial_stock_price <- stock_prices[1]
stock_position <- 1  
delta_stock <- 1     


# Funktion til at updatere position ud fra greeks
adjust_options <- function(delta_stock, delta_option, stock_position) {
  required_option_position <- -stock_position * delta_stock / delta_option
  return(required_option_position)
}


n_days <- length(delta_values)

pnl_hedged <- numeric(n_days)
pnl_unhedged <- numeric(n_days)


for (day in 2:n_days) {
  current_stock_price <- stock_prices[day]
  previous_stock_price <- stock_prices[day - 1]
  
  current_option_price <- option_prices[day]
  previous_option_price <- option_prices[day - 1]
  
  current_delta <- delta_values[day]
  previous_delta <- delta_values[day - 1]

  option_position <- adjust_options(delta_stock, previous_delta, stock_position)
  
  pnl_unhedged[day] <- (current_stock_price - previous_stock_price) * stock_position
  
  option_pnl <- option_position * (current_option_price - previous_option_price) # skal ændres til at være ændringen i options pris i stedet
  pnl_hedged[day] <- pnl_unhedged[day] + option_pnl
  
  print(option_position)
}


pnl_unhedged <- pnl_unhedged[-1]
pnl_hedged <- pnl_hedged[-1]



pnl_data <- data.frame(
  pnl_unhedged = pnl_unhedged,
  pnl_hedged = pnl_hedged
)

length(stock_prices)

regression_model <- lm(pnl_hedged ~ pnl_unhedged, data = pnl_data)
regression_model1 <- lm(pnl_hedged ~ stock_prices, data = pnl_data)
#plot(regression_model)

r_squared <- summary(regression_model)$r.squared

r_squared

var(pnl_hedged)
var(pnl_unhedged)

pnl_hedged


plot(pnl_unhedged)
plot(pnl_hedged)
# R^2 for heston constant R^2 of the hedged portfolio: 0.1738059 
# R^2 for B&S constant R^2 of the hedged portfolio: 0.1615159
# R^2 for heston non constant R^2 of the hedged portfolio: 0.1553856



