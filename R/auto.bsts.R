#AddSeasonal()
seasonality.func = function(ts, season){
  time = seq(from = 1, by = 1, length.out = length(ts))
  seasonaility = as.factor(time %% season)
  df = cbind.data.frame(ts, time, seasonaility)
  lm.1 = lm(df[,1] ~ seasonaility, data = df)
  p.vals = summary(lm.1)$coefficients[,4]
  p.vals = p.vals[2:length(p.vals)]
  p.vals.lt.01 = as.numeric(sum(p.vals<.01)>0)
  return(p.vals.lt.01)
}
#AddLocalLinearTrend()
lineartrend.func = function(ts){
  time = seq(from = 1, by = 1, length.out = length(ts))
  df = cbind.data.frame(ts, time)
  lm.1 = lm(df[,1] ~ time, data = df)
  p.vals = summary(lm.1)$coefficients[,4]
  p.vals = p.vals[2:length(p.vals)]
  p.vals.lt.01 = as.numeric(sum(p.vals<.01)>0)
  return(p.vals.lt.01)
}


#AddAutoAR()
laggedvalues.func = function(ts){
  fit <- forecast::arimaorder(forecast::auto.arima(ts, stepwise = TRUE))
  lagged_value <- fit[1]
  if (is.null(fit[7])){
    stop("Please indicate your frequency of data")
  } else{
    freq <- fit[7]
  }
  values <- c(lagged_value, freq)
  return(values)
}


auto_bsts = function(ts, data = NULL, freq = NULL, niter = 500, seed = 1337){
  if (!is.numeric(ts)){
    stop("Data isnt in correct format, please change to time-series or numeric")
  }

  if (is.null(data)){
    print("No exogenous data being added")
  }
  ## extract data from series
  freq_lag <- laggedvalues.func(ts)

  ## frequency of data and the lag that auto arima found to be significant
  freq <- freq_lag[2]
  lag <- freq_lag[1]



  if (lineartrend.func(ts)==1){
    ss <- AddLocalLinearTrend(list(), ts)
    print("Added Local Linear Trend")
  } else {
    ss <- AddLocalLevel(list(), ts)
    print("Added Local Level Trend")
  }

  seas_test <- seasonality.func(ldeaths, season = freq)

  if (seas_test==1){
    ss <- AddSeasonal(ss, ts, nseasons = freq)
    print(paste("Added seasonaility at the", freq,"level.", sep = " "))
  } else {
    print("No Seasonaility Found")
  }

  if (lag>0){
    ss <- AddAutoAr(ss, ts, lags = lag)
    print(paste("Added AR trend at the", lag, "lagged value", sep = " "))
  } else {
    print("No Lagged AR value found")
  }

  ## the bsts modelling
  model <- bsts(ts, state.specification = ss, niter = niter, seed = seed)
  browser()

}

auto_bsts(co2)



