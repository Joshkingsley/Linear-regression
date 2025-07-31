library(randomForest)
library(dplyr)
library(xts)


target_profit <- 3 #Target profit in USD
lot_size_per_pip <- 0.01 #For JPY pairs in a cents account
target_profit_pips <- target_profit / lot_size_per_pip
RR <- 1/3
target_stop_loss <- target_profit * RR
target_stoploss_pips <- target_stop_loss / lot_size_per_pip

n_rows <- 1000

timestamps <- seq(from = as.POSIXct("2025-01-01 09:00:00", tz="UTC"), by="1 sec", length.out = n_rows)


bid_price <- 1.1000 + cumsum(rnorm(n_rows, sd = 0.00005))
ask_price <- bid_price + runif(n_rows, 0.0001, 0.0003)
bid_volume <- round(runif(n_rows, 10, 100))
ask_volume <- round(runif(n_rows, 10, 100))
trade_volume <- round(runif(n_rows, 1, 50))
trade_price <- (bid_price + ask_price) / 2 + rnorm(n_rows, 0.0001)

raw_data_df <- data.frame(Timestamp = timestamps, BidPrice = bid_price, AskPrice = ask_price, BidVolume = bid_volume, AskVolume = ask_volume, TradePrice = trade_price, TradeVolume = trade_volume)

#Converting the dataframe into a time series

price_data <- xts(raw_data_df[, c("BidPrice", "AskPrice", "TradePrice")], order.by = raw_data_df$Timestamp)
volume_data <- xts(raw_data_df[, c("BidVolume", "AskVolume", "TradeVolume")], order.by = raw_data_df$Timestamp)

mid_price <- (price_data$BidPrice + price_data$AskPrice) / 2
colnames(mid_price) <- "MidPrice"
mid_price[1:5, ]

imbalance <- (volume_data$BidVolume - volume_data$AskVolume) / (volume_data$BidVolume + volume_data$AskVolume)
colnames(imbalance) <- "Imbalance"
imbalance[is.na(imbalance)] <- 0
imbalance[1:5, ]
spread <- price_data$AskPrice - price_data$BidPrice
colnames(spread) <- "Spreads"
spread[1:5, ]
depth_ratio <- volume_data$BidVolume / volume_data$AskVolume
colnames(depth_ratio) <- "DepthRatio"
depth_ratio[is.infinite(depth_ratio)] <- NA
depth_ratio[1:5, ]

n_future_seconds <- 50

price_movement_raw <- lead(mid_price, n = n_future_seconds) - mid_price
price_movement_pips <- price_movement_raw / lot_size_per_pip
colnames(price_movement_pips) <- "PriceMovementPips"
price_movement_pips

features <- na.omit(cbind(imbalance, spread, depth_ratio, price_movement_pips))

model_data <- as.data.frame(features)

train_index <- 1:floor(0.7 * nrow(model_data))
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

#Forming a linear model

trading_model_lm <- lm(PriceMovementPips ~ Imbalance + Spreads + DepthRatio, data = train_data)
summary(trading_model_lm)