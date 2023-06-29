library(astsa)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(extrafont)
library(LSTS)
library(keras)
library(tensorflow)
loadfonts()

setwd('D:/Users/Jurrivh Liao/Documents/All of statistics/Time Series and Spatial Statistics/project')
avocado <- read.csv('avocado-conventional.csv')
avocado$date <- as.Date(avocado$date, format = "%d/%m/%Y")
ggplot(avocado, aes(x=date, y=average_price)) +
  geom_line(color='#0057B7') +
  geom_smooth(color='#FFD700') +
  scale_y_continuous(breaks = seq(0, 3, 0.15)) +
  scale_x_date(date_breaks = "120 day") +
  labs(title = 'Average price of conventional avocados in total U.S.',
       x = 'Date',
       y = 'Average Price') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.text.x = element_text(angle = 35, hjust = 1.2, vjust = 1.2),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

price <- avocado$average_price
volume <- avocado$total_volume / 1e6
lag1.plot(price, 9, pch=20, col='#0057B7', lwc='#FFDD00', box.col='#FFFFFF',
          cex=0.8, lwl=1.5, gg=TRUE)
lag2.plot(price, volume, 8, pch=20, col='#0057B7', lwc='#FFDD00', box.col='#FFFFFF',
          cex=0.8, lwl=1.5, gg=TRUE)
lag2.plot(volume, price, 8, pch=20, col='#0057B7', lwc='#FFDD00', box.col='#FFFFFF',
          cex=0.8, lwl=1.5, gg=TRUE)

avocado.acf <- acf(avocado$average_price, plot=FALSE)
avocado.acf <- with(avocado.acf, data.frame(lag, acf))
ggplot(data = avocado.acf, mapping = aes(x = lag, y = acf)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), color = 'royalblue') +
  geom_hline(aes(yintercept = qnorm(0.975) / sqrt(306)), linetype = 2, color = 'gold') +
  geom_hline(aes(yintercept = 0), linetype = 1, color = 'dimgray') +
  geom_hline(aes(yintercept = -qnorm(0.975) / sqrt(306)), linetype = 2, color = 'gold') +
  labs(title = 'ACF of avocado price series',
       x = 'Lag (week)',
       y = 'ACF') +
  theme(text=element_text(size=12.5, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

avocado.pacf <- pacf(avocado$average_price, plot=FALSE)
avocado.pacf <- with(avocado.pacf, data.frame(lag, acf))
ggplot(data = avocado.pacf, mapping = aes(x = lag, y = acf)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), color = 'royalblue') +
  geom_hline(aes(yintercept = qnorm(0.975) / sqrt(306)), linetype = 2, color = 'gold') +
  geom_hline(aes(yintercept = 0), linetype = 1, color = 'dimgray') +
  geom_hline(aes(yintercept = -qnorm(0.975) / sqrt(306)), linetype = 2, color = 'gold') +
  ylim(-0.12, 1.0) +
  labs(title = 'PACF of avocado price series',
       x = 'Lag (week)',
       y = 'PACF') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )


# Define the training set and test set.
train <- data.frame(t = avocado$date, data=avocado$average_price)[1:261,]
test <- data.frame(t = avocado$date, data=avocado$average_price)[262:306,]
model <- ar.yw(train$data, aic = FALSE, order.max = 1)
# model <- ar.mle(train$data, aic = FALSE, order.max = 1)
# model <- arima(train$data, order = c(1, 1, 0))

# Diagnostics.
resid <- model$resid[-1]
resid.std <- resid / sqrt(model$var.pred)

ggplot(data=data.frame(x=avocado$date[2:261], y=resid.std), aes(x=x, y=y)) +
  geom_line(color='#0057B7') +
  geom_smooth(color='#FFD700') +
  scale_y_continuous(breaks = seq(-4.0, 4.0, 1.0)) +
  scale_x_date(date_breaks = "120 day") +
  labs(title = 'Standardized Residuals',
       x = 'Date', y = '') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.text.x = element_text(angle = 35, hjust = 1.2, vjust = 1.2),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 15, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

resid.acf <- acf(resid.std, plot=FALSE)
resid.acf <- with(resid.acf, data.frame(lag, acf))[-1,]
ggplot(data = resid.acf, mapping = aes(x = lag, y = acf)) +
  geom_segment(mapping = aes(xend = lag, yend = 0), color = 'royalblue') +
  geom_hline(aes(yintercept = qnorm(0.975) / sqrt(260)), linetype = 2, color = 'gold') +
  geom_hline(aes(yintercept = 0), linetype = 1, color = 'dimgray') +
  geom_hline(aes(yintercept = -qnorm(0.975) / sqrt(260)), linetype = 2, color = 'gold') +
  ylim(-0.20, 0.20) +
  scale_x_continuous(breaks = seq(0, 24, 4)) +
  labs(title = 'ACF of Residuals', x = 'Lag (week)', y = 'ACF') +
  theme(text=element_text(size=12.5, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

ggqqplot(data = resid.std, color = 'cornflowerblue', shape='o') +
  grids(linetype='solid') +
  labs(title = 'Normal Q-Q plot of Std Residuals',
       x = 'Theoretical Quantiles',
       y = 'Sample Quantiles') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

Box.Ljung.Test(resid, lag=20) +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

metric <- function(model, data, k, n=261){
  resid <- model$resid
  resid[is.na(resid)] <- 0
  sse <- sum(resid ^ 2)
  mse <- sse / (n - k)
  sst <- sum((data - mean(data)) ^ 2)
  rsquare <- 1 - sse / sst
  aic <- log(sse / n) + (n + 2 * k) / n
  aicc <- log(sse / n) + (n + k) / (n - k - 2)
  bic <- log(sse / n) + k * log(n) / n
  return(c(sse=sse, mse=mse, rsquare=rsquare, aic=aic, aicc=aicc, bic=bic))
}
ar1 <- arima(train$data, order=c(1, 0, 0), method='ML')
metric(ar1, train$data, 2)
ar2 <- arima(train$data, order=c(2, 0, 0), method='ML')
metric(ar2, train$data, 3)
arma11 <- arima(train$data, order=c(1, 0, 1), method='ML')
metric(arma11, train$data, 3)
arima110 <- arima(train$data, order=c(1, 1, 0), method='ML')
metric(arima110, train$data, 2)
ar12 <- arima(train$data, order=c(12, 0, 0))
metric(ar12, train$data, 13)

# One-step forecasting.
ar_forecast_step <- function(model, train_data, test_data, order.max=1){
  n.ahead <- length(test_data)
  preds <- numeric(n.ahead)
  ses <- numeric(n.ahead)
  prediction <- predict(model, train_data, n.ahead=1)
  preds[1] <- prediction$pred[1]
  ses[1] <- prediction$se[1]
  for(j in 1:(n.ahead - 1)){
    dataset <- c(train_data, test_data[1:j])
    prediction <- predict(model, dataset, n.ahead=1)
    preds[j + 1] <- prediction$pred[1]
    ses[j + 1] <- prediction$se[1]
  }
  return(data.frame(pred=preds, se=ses))
}
fore <- ar_forecast_step(model, train$data, test$data, order.max=1)
ggplot() +
  geom_line(aes(x=train$t, y=train$data, color='Historical')) +
  geom_line(aes(x=c(train$t[261], test$t), y=c(train$data[261], test$data), 
                color='Groundtruth')) +
  geom_line(aes(x=test$t, y=fore$pred, color='Forecasted')) +
  geom_point(aes(x=test$t, y=fore$pred), color='orange', shape=1) +
  geom_ribbon(aes(x=test$t, 
                  ymin=fore$pred + qnorm(0.025) * fore$se,
                  ymax=fore$pred + qnorm(0.975) * fore$se),
              fill='navajowhite', alpha=0.5) +
  scale_y_continuous(breaks = seq(0, 3, 0.15)) +
  scale_x_date(date_breaks = "120 day") +
  scale_color_manual(name='Legend', values=c(
    'Historical'='cornflowerblue',
    'Groundtruth'='mediumseagreen',
    'Forecasted'='orange'
  )) +
  labs(title = 'Forcasted average price of conventional avocados in total U.S. (AR, 1 step)',
       x = 'Date',
       y = 'Average Price') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.text.x = element_text(angle = 35, hjust = 1.2, vjust = 1.2),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

# m-step forecasting.
fore <- predict(model, n.ahead=45)
ggplot() +
  geom_line(aes(x=train$t, y=train$data, color='Historical')) +
  geom_line(aes(x=c(train$t[261], test$t), y=c(train$data[261], test$data), 
                color='Groundtruth')) +
  geom_line(aes(x=test$t, y=fore$pred, color='Forecasted')) +
  geom_point(aes(x=test$t, y=fore$pred), color='orange', shape=1) +
  geom_ribbon(aes(x=test$t, 
              ymin=fore$pred + qnorm(0.025) * fore$se,
              ymax=fore$pred + qnorm(0.975) * fore$se),
              fill='navajowhite', alpha=0.5) +
  scale_y_continuous(breaks = seq(0, 3, 0.15)) +
  scale_x_date(date_breaks = "120 day") +
  scale_color_manual(name='Legend', values=c(
    'Historical'='cornflowerblue',
    'Groundtruth'='mediumseagreen',
    'Forecasted'='orange'
  )) +
  labs(title = 'Forcasted average price of conventional avocados in total U.S. (AR, m steps)',
       x = 'Date',
       y = 'Average Price') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.text.x = element_text(angle = 35, hjust = 1.2, vjust = 1.2),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

# Rescale.
scaleDataset <- function(dataset, range=c(-1, 1)){
  std_dataset <- (dataset - min(dataset)) / (max(dataset) - min(dataset))
  scaled <- std_dataset * (range[2] - range[1]) + range[1]
  return(scaled)
}

# Inverse transform.
invTransform <- function(dataset, trange, range=c(-1, 1)){
  dataset <- (dataset - range[1]) / (range[2] - range[1])
  scaled <- dataset * (trange[2] - trange[1]) + trange[1]
  return(scaled)
}

# Create the dataset for training LSTM.
lagTransform <- function(data, lag=1){
  lagged =  c(rep(NA, lag), data[1:(length(data) - lag)])
  lagged[is.na(lagged)] <- 0
  return(lagged)
}

scaledSeries <- scaleDataset(train$data, range=c(-1, 1))
season <- 6
superviseDataset <- data.frame(y=scaledSeries)
for(i in 1:season)
  superviseDataset <- cbind(superviseDataset, 
                            lagged=lagTransform(scaledSeries, lag=season + 1 - i))
x_train <- superviseDataset[, 2:(season + 1)]
x_train <- array(
  data = unlist(x_train),
  dim = c(nrow(x_train), season, 1)
)
y_train <- superviseDataset[, 1]

# create the LSTM model.
set.seed(1289)
batch_size <- 1
units <- 6
LSTM <- keras_model_sequential() 
LSTM %>%
  layer_lstm(units, batch_input_shape = c(batch_size, season, 1), stateful=TRUE)%>%
  layer_dense(units = 1)

LSTM %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam(learning_rate=1e-2, decay=1e-6),  
  metrics = c('accuracy')
)

epochs = 50   
for(i in 1:epochs){
  LSTM %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, 
               verbose=1, shuffle=FALSE)
  LSTM %>% reset_states()
}

lstm_forecast <- function(model, series, n.ahead, batch_size=1){
  model %>% reset_states()
  predictions <- numeric(n.ahead + season)
  predictions[1:season] <- series[(length(series) - season + 1):length(series)]
  for(j in 1:n.ahead){
    X <- predictions[j:(j + season - 1)]
    dim(X) <- c(batch_size, season, 1)
    yhat <- model %>% predict(X, batch_size=batch_size)
    predictions[j + season] <- yhat
  }
  return(predictions[-1:-season])
}

predictions <- lstm_forecast(LSTM, scaledSeries, n.ahead=45)
output <- invTransform(predictions, trange=c(min(train$data), max(train$data)))

ggplot() +
  geom_line(aes(x=train$t, y=train$data, color='Historical')) +
  geom_line(aes(x=c(train$t[261], test$t), y=c(train$data[261], test$data), 
                color='Groundtruth')) +
  geom_line(aes(x=c(train$t[261], test$t), y=c(train$data[261], output),
                color='Forecasted')) +
  scale_color_manual(name='Legend', values = c('Historical'='royalblue',
                                'Groundtruth'='mediumseagreen', 
                                'Forecasted'='orange'),
                     guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 3, 0.15)) +
  scale_x_date(date_breaks = "120 day") +
  labs(title = 'Forcasted average price of conventional avocados in total U.S. (LSTM)',
       x = 'Date',
       y = 'Average Price') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.text.x = element_text(angle = 35, hjust = 1.2, vjust = 1.2),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

lstm_forecast_step <- function(model, series, input, batch_size=1){
  model %>% reset_states()
  n.ahead <- length(input)
  groundtruth <- numeric(n.ahead + season)
  groundtruth[1:season] <- series[(length(series) - season + 1):length(series)]
  groundtruth[season:(n.ahead + season - 1)] <- input
  predictions <- numeric(n.ahead)
  for(j in 1:n.ahead){
    X <- groundtruth[j:(j + season - 1)]
    dim(X) <- c(batch_size, season, 1)
    yhat <- model %>% predict(X, batch_size=batch_size)
    predictions[j] <- yhat
  }
  return(predictions)
}

input <- (test$data - min(train$data)) / (max(train$data) - min(train$data)) * 2 - 1
pred <- lstm_forecast_step(LSTM, scaledSeries, input)
output <- invTransform(pred, trange=c(min(train$data), max(train$data)))
ggplot() +
  geom_line(aes(x=train$t, y=train$data, color='Historical')) +
  geom_line(aes(x=c(train$t[261], test$t), y=c(train$data[261], test$data), 
                color='Groundtruth')) +
  geom_line(aes(x=c(train$t[261], test$t), y=c(train$data[261], output),
            color='Forecasted')) +
  scale_color_manual(name='Legend', values = c('Historical'='royalblue',
                                'Groundtruth'='mediumseagreen', 
                                'Forecasted'='orange'),
                     guide = guide_legend(reverse = TRUE)) +
  scale_y_continuous(breaks = seq(0, 3, 0.15)) +
  scale_x_date(date_breaks = "120 day") +
  labs(title = 'Forcasted average price of conventional avocados in total U.S. (LSTM)',
       x = 'Date',
       y = 'Average Price') +
  theme(text=element_text(size=12, family="Calibri"),
        axis.title.x = element_text(margin= margin(t = 7.5, unit = "pt")),
        axis.text.x = element_text(angle = 35, hjust = 1.2, vjust = 1.2),
        axis.title.y = element_text(margin= margin(r = 7.5, unit = "pt")),
        plot.title = element_text(size = 16, hjust = 0.5, 
                                  margin= margin(b = 10, unit = "pt")),
        plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt"),
  )

