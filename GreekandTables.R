# Function to calculate Greeks for a call option in the Black-Scholes model
calculate_greeks <- function(S, K, r, sigma, T) {
  d1 <- (log(S/K) + (r + 0.5 * sigma^2) * T) / (sigma * sqrt(T))
  d2 <- d1 - sigma * sqrt(T)
  
  delta <- pnorm(d1)
  gamma <- dnorm(d1) / (S * sigma * sqrt(T))
  theta <- -((S * dnorm(d1) * sigma) / (2 * sqrt(T))) - r * K * exp(-r * T) * pnorm(d2)
  vega <- S * dnorm(d1) * sqrt(T)
  rho <- K * T * exp(-r * T) * pnorm(d2)
  
  return(c(delta, gamma, theta, vega, rho))
}


# Example strike prices (K) and time to expiration (T) values
strike_prices <- c(140,146.63,150)
time_to_expiration <- c("32 days" , "74 days" , "102 days" , "137 days" , "165 days", "193 days" , "228 days" , "284 days")
stock_prices <- c(146.63,146.63,146.63)  # Example: replace with actual stock prices for each week

# Create lists to store dataframes for each Greek
greek_dfs <- list()

greek_matrix1 <- matrix(NA, nrow = 8, ncol = 3)
greek_matrix2 <- matrix(NA, nrow = 8, ncol = 3)
greek_matrix3 <- matrix(NA, nrow = 8, ncol = 3)
greek_matrix4 <- matrix(NA, nrow = 8, ncol = 3)
greek_matrix5 <- matrix(NA, nrow = 8, ncol = 3)

# Calculate Greek values for each combination of K and T

for (i in 1:length(time_to_expiration)) {
  for (j in 1:length(strike_prices)) {
    T <- as.numeric(sub(" days", "", time_to_expiration[i])) / 365  # Convert days to years
    S <- stock_prices[j]  # Get stock price for the corresponding week
    greeks <- calculate_greeks(S, strike_prices[j], r, IV, T)
    # Assign greek values to the corresponding columns in greek_matrix
    greek_matrix1[i,j] <- matrix(greeks[1], nrow = 1)
    greek_matrix2[i,j] <- matrix(greeks[2], nrow = 1)
    greek_matrix3[i,j] <- matrix(greeks[3], nrow = 1)
    greek_matrix4[i,j] <- matrix(greeks[4], nrow = 1)
    greek_matrix5[i,j] <- matrix(greeks[5], nrow = 1)
  }
  }



greek_matrix1 #delta


#greeks <- calculate_greeks(146.63, 140, r, IV, 228/365)


colnames(greek_matrix1) <- strike_prices
rownames(greek_matrix1) <- time_to_expiration
colnames(greek_matrix2) <- strike_prices
rownames(greek_matrix2) <- time_to_expiration
colnames(greek_matrix3) <- strike_prices
rownames(greek_matrix3) <- time_to_expiration
colnames(greek_matrix4) <- strike_prices
rownames(greek_matrix4) <- time_to_expiration
colnames(greek_matrix5) <- strike_prices
rownames(greek_matrix5) <- time_to_expiration


write.csv(greek_matrix1, file = paste0("Delta", 1, ".csv"), row.names = TRUE, quote = FALSE)
write.csv(greek_matrix2, file = paste0("Gamma", 2, ".csv"), row.names = TRUE, quote = FALSE)
write.csv(greek_matrix3, file = paste0("Theta", 3, ".csv"), row.names = TRUE, quote = FALSE)
write.csv(greek_matrix4, file = paste0("Vega", 4, ".csv"), row.names = TRUE, quote = FALSE)
write.csv(greek_matrix5, file = paste0("Rho", 5, ".csv"), row.names = TRUE, quote = FALSE)


# Read the CSV file into a dataframe (replace "i" with the correct index)

