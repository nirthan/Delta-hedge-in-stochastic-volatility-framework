# Load required packages
library(RQuantLib)
library(ggplot2)
library(plotly)
library(RND)

r = 0.01862546

type = "call"

#IMPORT DATA TABLE "AAPL-data" MANUALLY INSTEAD TO GET RID OF THE EXTRA COLUMN

#load data
#callPR <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/AAPL-data.csv", header = TRUE, row.names = 1)

#Laust
callPR <- read.csv("C:\\Users\\Bruger\\Desktop\\F2024\\AAPL-data.csv", header = TRUE, row.names = 1)

#9dan
#callPR <-read.csv("C:\\Users\\Nirthan\\OneDrive - Aalborg Universitet\\AAU\\8.semester\\8.semester code\\GitHub\\F2024\\AAPL-data.csv", header = TRUE, row.names = 1)

# Function to calculate Black-Scholes price
bs_price <- function(sigma, S, K, T, r, option_type) {
  price <- EuropeanOption(type = option_type, underlying = S, strike = K, 
                          dividendYield = 0, riskFreeRate = r, maturity = T, 
                          volatility = sigma)
  return(price)
}

# Function to calculate sum of squared errors
error_function <- function(sigma, observed_price, S, K, T, r, option_type) {
  model_price <- bs_price(sigma, S, K, T, r, option_type)
  error <- (observed_price - model_price$value)^2
  return(error)
}

PD_function <- function(sigma, observed_price, S, K, T, r, option_type) {
  model_price <- bs_price(sigma, S, K, T, r, option_type)
  error <- (observed_price - model_price$value)
  return(error)
}

errorvector <- vector(mode = "numeric", length = 0)
PDvector <- vector(mode = "numeric", length = 0)

#Ttid <- (1667595600000-callPR[1,3])/(1000 * 60 * 60 * 24 * 365)
#Ttid

T <- 0

S <- 146.63

dfPD <- data.frame(
  PD = c(0),
  K = c(0),
  T = c(0)
)




optimfunc <- function(sigma) {
  r <- 0.01862546 #from treasuries
  for (i in 1:nrow(AAPL.data)) {
    for (j in 4:(ncol(AAPL.data)-1)){
      if (!is.na(AAPL.data[i,j])) {
        S <- AAPL.data[i,1]
        K <- AAPL.data[i,2]#as.numeric(gsub("\\D", "", colnames(callPR[j]) ))
        T <- AAPL.data[i,3]#(1667595600000-callPR[i,3])/(1000 * 60 * 60 * 24 * 365)
        obs_price <- AAPL.data[i,4]#callPR[i,j]
        vol <- sigma 
        errorvector <- c(errorvector, error_function(sigma = vol, observed_price = obs_price , S = S, K = K, T = T, r = r ,option_type = "call"))
        PDvector <- c(PDvector, PD_function(sigma = vol, observed_price = obs_price , S = S, K = K, T = T, r = r ,option_type = "call"))
        newPD <- data.frame(PD = PDvector[length(PDvector)], K = K, T=T, stringsAsFactors = FALSE)
        dfPD <- rbind(dfPD, newPD)
        if (i == nrow(AAPL.data)) {
          PDvector <<- PDvector
          dfPD <<- dfPD
        }
      }
    }
  }
  return(sum(errorvector))
}
#Insert in for loop for later use

#PDvector <- c(PDvector, PD_function(sigma = vol, observed_price = obs_price , S = S, K = K, T = T, r = r ,option_type = "call"))
#newPD <- data.frame(PD = PDvector[length(PDvector)], K = K, T=T, stringsAsFactors = FALSE)
#dfPD <- rbind(dfPD, newPD)
#if (i == nrow(callPR)) {
#  PDvector <<- PDvector
#  dfPD <<- dfPD
#}

IPV <- optimize(optimfunc,interval = c(0.3, 0.5), tol = 1e-5)

IPV #min = sigma , objec = SSE 

IV <- IPV$minimum
IV

#MSE price
MSE <- IPV$objective/1317 #1317 er antallet af observationer
MSE

#MSE IV
ivtest <- vector(mode = "numeric", length = 0)
for(i in 1:nrow(AAPL.data)){
  ivtest[i]<-compute.implied.volatility(r=0.01862546, te=AAPL.data[i,3], 146.63, k=AAPL.data[i,2], y=0, call.price=AAPL.data[i,4],lower=0.1, upper=0.6)
}
IV_MSE <- sum((ivtest - IV)^2)/1317
IV_MSE

mean(dfPD$PD)

plot(dfPD$PD)

optimfunc(IV)



dfPD2 = dfPD[-1,]

PLOT_KvsPD <- ggplot(dfPD2, aes(x = K, y = PD)) +
  geom_point() +
  labs(x = "K", y = "PD", title = "K vs PD")

ggsave("/Users/christiantaulbjerg/Documents/P8/KvsPD.png", plot = PLOT_KvsPD)
#ggsave("C:\\Users\\Bruger\\Desktop\\F2024\\KvsPD.png", plot = PLOT_KvsPD)

#ggsave("C:\\Users\\Nirthan\\OneDrive - Aalborg Universitet\\AAU\\8.semester\\8.semester code\\GitHub\\F2024\\KvsPD.png", plot = PLOT_KvsPD)

PLOT_TvsPD <- ggplot(dfPD2, aes(x = T, y = PD)) +
  geom_point() +
  labs(x = "T", y = "PD", title = "T vs PD")

ggsave("/Users/christiantaulbjerg/Documents/P8/TvsPD.png", plot = PLOT_TvsPD)
#ggsave("C:\\Users\\Bruger\\Desktop\\F2024\\TvsPD.png", plot = PLOT_TvsPD)
#ggsave("C:\\Users\\Nirthan\\OneDrive - Aalborg Universitet\\AAU\\8.semester\\8.semester code\\GitHub\\F2024\\TvsPD.png", plot = PLOT_TvsPD)

PLOT_KvsT <- ggplot(dfPD2, aes(x = K, y = T)) +
  geom_point() +
  labs(x = "K", y = "T", title = "K vs T")

ggsave("/Users/christiantaulbjerg/Documents/P8/KvsT.png", plot = PLOT_KvsT)
#ggsave("C:\\Users\\Bruger\\Desktop\\F2024\\KvsT.png", plot = PLOT_KvsT)
#ggsave("C:\\Users\\Nirthan\\OneDrive - Aalborg Universitet\\AAU\\8.semester\\8.semester code\\GitHub\\F2024\\KvsT.png", plot = PLOT_KvsT)

#scatter plot her

p <- plot_ly(data = dfPD, x = ~K, y = ~T, z = ~PD,type = "scatter3d", mode = "markers")

p

#test til heat map
library(plotly)

pheat1 <- plot_ly(data = dfPD, x = ~K, y = ~T, z = ~PD,type = "heat map")
pheat1

pheat <- plot_ly(data = dfPD, x = ~K, y = ~T, z = ~PD) %>%
  add_surface()
pheat




#Stock price overlook (134.84, 145.585, 157.455) OBSOLETE, de er alle 146.63 nu
#max(callPR$S)
#min(callPR$S)
#mean(callPR$S)

#first day 4 oktober 2022

#last day 3 november 2022

# T = 4 novemeber

# obs_price - bs_model(med  mean imp vol) -> pricing difference
#  (obs_price - bs_model(med  mean imp vol))^2

