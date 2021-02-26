library(tidyverse)
library(rvest)
library(ggplot2)
library(gridExtra)
library(BatchGetSymbols)

url <- 'https://en.wikipedia.org/wiki/List_of_S%26P_500_companies'
webpage <- read_html(url)
companies <- webpage %>% html_nodes('#constituents td:nth-child(2)') %>% html_text()
tickers <- webpage %>% html_nodes('#constituents td:nth-child(1)') %>% html_text()
tickers <- sub('\n', '', tickers)
tickers <- sub('\\.', '-', tickers)
start.date <- '2020-01-01'
end.date <- '2020-12-31'
frequency.of.data <- 'daily'

stocks <- BatchGetSymbols(tickers = tickers, first.date = start.date,
                          last.date = end.date, freq.data = frequency.of.data)

head(stocks$df.control)
head(stocks$df.tickers)

index <- BatchGetSymbols(tickers = '^GSPC', first.date = start.date,
                         last.date = end.date, freq.data = frequency.of.data)

head(index$df.control)
head(index$df.tickers)

invalid.index <- which(stocks$df.control$threshold.decision == 'OUT')
valid.index <- which(stocks$df.control$threshold.decision == 'KEEP')
invalid.ticker <- stocks$df.control$ticker[invalid.index]
invalid.ticker

wrong.info.index <- stocks$df.tickers %>% filter(ref.date == '2020-10-02' & ret.adjusted.prices == 0)
right.info.index <- stocks$df.tickers %>% filter(ref.date == '2020-10-02' & !ret.adjusted.prices == 0)
stocks$df.tickers <- stocks$df.tickers %>% filter(!ref.date == '2020-10-02')
stocks$df.tickers <- bind_rows(stocks$df.tickers, right.info.index)
stocks$df.tickers <- stocks$df.tickers %>% mutate(ref.date = as.Date(ref.date, '%d-%m-%Y')) %>% arrange(ref.date)
index$df.tickers <- index$df.tickers[-192,]

stocks$df.control <- stocks$df.control %>%
  filter((threshold.decision != 'OUT'))

set.seed(1)
random.sampling.index <- sample(valid.index, 26, replace = FALSE)
random.sampling.index <- random.sampling.index[-2]
random.sample.stocks <- stocks$df.control$ticker[random.sampling.index]
random.sample.stocks

capital <- 100000
shares.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[1])
shares.number <- capital/shares.data$price.open[1]
print(c('number of shares is', shares.number))
shares.price <- shares.data$price.open[1]
print(c('price of shares was', shares.price))

index.data <- index$df.tickers
index.number <- capital/index.data$price.open[1]
print(c('number of index shares is', index.number))
index.price <- index.data$price.open[1]
print(c('price of index shares was', index.price))

shares.daily.return <- cumprod(1+(shares.data$ret.adjusted.prices[-1]))-1
index.daily.return <- cumprod(1+(index.data$ret.adjusted.prices[-1]))-1
return.data <- tibble(date = index.data$ref.date[-1])
return.data <- return.data %>% add_column(shares.return.perc = shares.daily.return)
return.data <- return.data %>% add_column(index.return.perc = index.daily.return)
ggplot() +
  geom_line(data = return.data, aes(date, shares.return.perc), color = 'blue', size = 1) +
  geom_line(data = return.data, aes(date, index.return.perc), color = 'black', size = 1) +
  labs(title = paste0('MS (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative return', x = 'date', y = 'return percentage')

shares.return.YTD <- return.data$shares.return.perc[dim(return.data)[1]]
index.return.YTD <- return.data$index.return.perc[dim(return.data)[1]]
print(c('MS YTD return is ', shares.return.YTD))
print(c('^GSPC YTD return is ', index.return.YTD))

shares.return.std.dev.YTD <- sqrt(sum((shares.data$ret.adjusted.prices[-1] - shares.return.YTD)^2))
index.return.std.dev.YTD <- sqrt(sum((index.data$ret.adjusted.prices[-1] - index.return.YTD)^2))
shares.return.std.dev.YTD/length(shares.daily.return)
index.return.std.dev.YTD/length(index.daily.return)

((shares.return.std.dev.YTD/length(shares.daily.return) + 1) * capital) - capital
((index.return.std.dev.YTD/length(index.daily.return) + 1) * capital) - capital

shares.return.mean.cumul <- c()
index.return.mean.cumul <- c()
for (i in 2:length(shares.data$ret.adjusted.prices)) {
  shares.return.mean.cumul <- c(shares.return.mean.cumul, mean(shares.data$ret.adjusted.prices[2:i]))
  index.return.mean.cumul <- c(index.return.mean.cumul, mean(index.data$ret.adjusted.prices[2:i]))
}
shares.list.cumul <- vector(mode = 'list', length = (length(shares.data$ret.adjusted.prices)-1))
index.list.cumul <- vector(mode = 'list', length = (length(index.data$ret.adjusted.prices)-1))
shares.return.std.dev.cumul <- c()
index.return.std.dev.cumul <- c()
for (j in 1:(length(shares.data$ret.adjusted.prices)-1)) {
  shares.list.cumul[[j]] <- c(shares.data$ret.adjusted.prices[2:(1+j)])
  shares.return.std.dev.cumul <- c(shares.return.std.dev.cumul, sqrt(sum((shares.list.cumul[[j]] - shares.return.mean.cumul[j])^2)))
  index.list.cumul[[j]] <- c(index.data$ret.adjusted.prices[2:(1+j)])
  index.return.std.dev.cumul <- c(index.return.std.dev.cumul, sqrt(sum((index.list.cumul[[j]] - index.return.mean.cumul[j])^2)))
}
return.data <- return.data %>% add_column(shares.return.std.dev = shares.return.std.dev.cumul)
return.data <- return.data %>% add_column(index.return.std.dev = index.return.std.dev.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, shares.return.std.dev), color = 'blue', size = 1) +
  geom_line(data = return.data, aes(date, index.return.std.dev), color = 'black', size = 1) +
  labs(title = paste0('MS (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Standard Deviation')

shares.return.semi.dev.cumul <- c()
index.return.semi.dev.cumul <- c()
for (j in 1:length(shares.list.cumul)) {
  shares.return.cumul <- shares.list.cumul[[j]]
  shares.return.semi.dev.values <- shares.return.cumul[shares.return.cumul <= mean(shares.return.cumul)]
  shares.return.semi.dev.cumul <- c(shares.return.semi.dev.cumul, sqrt((1/length(shares.return.semi.dev.values))*sum((shares.return.semi.dev.values - mean(shares.return.cumul))^2)))
  index.return.cumul <- index.list.cumul[[j]]
  index.return.semi.dev.values <- index.return.cumul[index.return.cumul <= mean(index.return.cumul)]
  index.return.semi.dev.cumul <- c(index.return.semi.dev.cumul, sqrt((1/length(index.return.semi.dev.values))*sum((index.return.semi.dev.values - mean(index.return.cumul))^2)))
}
return.data <- return.data %>% add_column(shares.return.semi.dev = shares.return.semi.dev.cumul)
return.data <- return.data %>% add_column(index.return.semi.dev = index.return.semi.dev.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, shares.return.semi.dev), color = 'blue', size = 1) +
  geom_line(data = return.data, aes(date, index.return.semi.dev), color = 'black', size = 1) +
  labs(title = paste0('MS (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Semi Deviation')

shares.return.down.dev.cumul <- c()
index.return.down.dev.cumul <- c()
for (j in 1:length(shares.list.cumul)) {
  shares.return.cumul <- shares.list.cumul[[j]]
  shares.return.down.dev.values <- shares.return.cumul[shares.return.cumul <= 0]
  shares.return.down.dev.cumul <- c(shares.return.down.dev.cumul, sqrt((1/length(shares.return.down.dev.values))*sum((shares.return.down.dev.values))^2))
  index.return.cumul <- index.list.cumul[[j]]
  index.return.down.dev.values <- index.return.cumul[index.return.cumul <= 0]
  index.return.down.dev.cumul <- c(index.return.down.dev.cumul, sqrt((1/length(index.return.down.dev.values))*sum((index.return.down.dev.values))^2))
}
return.data <- return.data %>% add_column(shares.return.down.dev = shares.return.down.dev.cumul)
return.data <- return.data %>% add_column(index.return.down.dev = index.return.down.dev.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, shares.return.down.dev), color = 'blue', size = 1) +
  geom_line(data = return.data, aes(date, index.return.down.dev), color = 'black', size = 1) +
  labs(title = paste0('MS (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Down Deviation')

beta.cumul <-c()
for (j in 1:length(shares.list.cumul)) {
  beta.cumul <- c(beta.cumul, cov(shares.list.cumul[[j]], index.list.cumul[[j]])/var(shares.list.cumul[[j]]))
}
return.data <- return.data %>% add_column(return.beta = beta.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.beta), color = 'blue', size = 1) +
  labs(title = paste0('MS vs ^GSPC Beta'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Beta')

url <- 'https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=2020'
webpage <- read_html(url)
tbill.yield <- webpage %>% html_nodes('.text_view_data:nth-child(6)') %>% html_text()
tbill.yield <- as.numeric(tbill.yield)/100

alpha.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  alpha.cumul <- c(alpha.cumul, shares.daily.return[j] - (tbill.yield[j] + beta.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
}
return.data <- return.data %>% add_column(return.alpha = alpha.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.alpha), color = 'blue', size = 1) +
  labs(title = paste0('MS vs ^GSPC Alpha'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Alpha')

sharpe.ratio.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  sharpe.ratio.cumul <- c(sharpe.ratio.cumul, (shares.daily.return[j] - tbill.yield[j])/sqrt(var(shares.list.cumul[[j]] - tbill.yield[j])))
}
return.data <- return.data %>% add_column(return.sharpe.ratio = sharpe.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.sharpe.ratio), color = 'blue', size = 1) +
  labs(title = paste0('MS vs ^GSPC Sharpe Ratio'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Sharpe Ratio')

sortino.ratio.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  sortino.ratio.cumul <- c(sortino.ratio.cumul, ((shares.daily.return[j] - tbill.yield[j])/shares.return.down.dev.cumul[j]))
}
return.data <- return.data %>% add_column(return.sortino.ratio = sortino.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.sortino.ratio), color = 'blue', size = 1) +
  labs(title = paste0('MS vs ^GSPC Sortino Ratio'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Treynor Ratio')

treynor.ratio.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  treynor.ratio.cumul <- c(treynor.ratio.cumul, (shares.daily.return[j] - tbill.yield[j])/beta.cumul[j])
}
return.data <- return.data %>% add_column(return.treynor.ratio = treynor.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.treynor.ratio), color = 'blue', size = 1) +
  labs(title = paste0('MS vs ^GSPC Treynor Ratio'),
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Treynor Ratio')


















masterplotlist <- vector(mode = 'list', length = length(random.sample.stocks)/5)
test <- vector(mode = 'list', length = length(random.sample.stocks)/5)
for (i in 1:(length(random.sample.stocks)/5)) {
  plotlist <- vector(mode = 'list', length = 9)
  capital <- 100000
  shares1.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+1])
  shares1.number <- capital/shares1.data$price.open[1]
  shares1.price <- shares1.data$price.open[1]
  shares2.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+2])
  shares2.number <- capital/shares2.data$price.open[1]
  shares2.price <- shares2.data$price.open[1]
  shares3.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+3])
  shares3.number <- capital/shares3.data$price.open[1]
  shares3.price <- shares3.data$price.open[1]
  shares4.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+4])
  shares4.number <- capital/shares4.data$price.open[1]
  shares4.price <- shares4.data$price.open[1]
  shares5.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+5])
  shares5.number <- capital/shares5.data$price.open[1]
  shares5.price <- shares5.data$price.open[1]
  index.data <- index$df.tickers
  index.number <- capital/index.data$price.open[1]
  index.price <- index.data$price.open[1]
  shares1.daily.return <- cumprod(1+(shares1.data$ret.adjusted.prices[-1]))-1
  shares2.daily.return <- cumprod(1+(shares2.data$ret.adjusted.prices[-1]))-1
  shares3.daily.return <- cumprod(1+(shares3.data$ret.adjusted.prices[-1]))-1
  shares4.daily.return <- cumprod(1+(shares4.data$ret.adjusted.prices[-1]))-1
  shares5.daily.return <- cumprod(1+(shares5.data$ret.adjusted.prices[-1]))-1
  index.daily.return <- cumprod(1+(index.data$ret.adjusted.prices[-1]))-1
  return.data <- tibble(date = index.data$ref.date[-1])
  return.data <- return.data %>% add_column(shares1.return.perc = shares1.daily.return)
  return.data <- return.data %>% add_column(shares2.return.perc = shares2.daily.return)
  return.data <- return.data %>% add_column(shares3.return.perc = shares3.daily.return)
  return.data <- return.data %>% add_column(shares4.return.perc = shares4.daily.return)
  return.data <- return.data %>% add_column(shares5.return.perc = shares5.daily.return)
  return.data <- return.data %>% add_column(index.return.perc = index.daily.return)
  
  shares1.return.mean.cumul <- c()
  shares2.return.mean.cumul <- c()
  shares3.return.mean.cumul <- c()
  shares4.return.mean.cumul <- c()
  shares5.return.mean.cumul <- c()
  index.return.mean.cumul <- c()
  for (j in 2:length(shares1.data$ret.adjusted.prices)) {
    shares1.return.mean.cumul <- c(shares1.return.mean.cumul, mean(shares1.data$ret.adjusted.prices[2:j]))
    shares2.return.mean.cumul <- c(shares2.return.mean.cumul, mean(shares2.data$ret.adjusted.prices[2:j]))
    shares3.return.mean.cumul <- c(shares3.return.mean.cumul, mean(shares3.data$ret.adjusted.prices[2:j]))
    shares4.return.mean.cumul <- c(shares4.return.mean.cumul, mean(shares4.data$ret.adjusted.prices[2:j]))
    shares5.return.mean.cumul <- c(shares5.return.mean.cumul, mean(shares5.data$ret.adjusted.prices[2:j]))
    index.return.mean.cumul <- c(index.return.mean.cumul, mean(index.data$ret.adjusted.prices[2:j]))
  }
  shares1.list.cumul <- vector(mode = 'list', length = (length(shares1.data$ret.adjusted.prices)-1))
  shares2.list.cumul <- vector(mode = 'list', length = (length(shares2.data$ret.adjusted.prices)-1))
  shares3.list.cumul <- vector(mode = 'list', length = (length(shares3.data$ret.adjusted.prices)-1))
  shares4.list.cumul <- vector(mode = 'list', length = (length(shares4.data$ret.adjusted.prices)-1))
  shares5.list.cumul <- vector(mode = 'list', length = (length(shares5.data$ret.adjusted.prices)-1))
  index.list.cumul <- vector(mode = 'list', length = (length(index.data$ret.adjusted.prices)-1))
  shares1.return.std.dev.cumul <- c()
  shares2.return.std.dev.cumul <- c()
  shares3.return.std.dev.cumul <- c()
  shares4.return.std.dev.cumul <- c()
  shares5.return.std.dev.cumul <- c()
  index.return.std.dev.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    shares1.list.cumul[[j]] <- c(shares1.data$ret.adjusted.prices[2:(1+j)])
    shares2.list.cumul[[j]] <- c(shares2.data$ret.adjusted.prices[2:(1+j)])
    shares3.list.cumul[[j]] <- c(shares3.data$ret.adjusted.prices[2:(1+j)])
    shares4.list.cumul[[j]] <- c(shares4.data$ret.adjusted.prices[2:(1+j)])
    shares5.list.cumul[[j]] <- c(shares5.data$ret.adjusted.prices[2:(1+j)])
    shares1.return.std.dev.cumul <- c(shares1.return.std.dev.cumul, sqrt(sum((shares1.list.cumul[[j]] - shares1.return.mean.cumul[j])^2)))
    shares2.return.std.dev.cumul <- c(shares2.return.std.dev.cumul, sqrt(sum((shares2.list.cumul[[j]] - shares2.return.mean.cumul[j])^2)))
    shares3.return.std.dev.cumul <- c(shares3.return.std.dev.cumul, sqrt(sum((shares3.list.cumul[[j]] - shares3.return.mean.cumul[j])^2)))
    shares4.return.std.dev.cumul <- c(shares4.return.std.dev.cumul, sqrt(sum((shares4.list.cumul[[j]] - shares4.return.mean.cumul[j])^2)))
    shares5.return.std.dev.cumul <- c(shares5.return.std.dev.cumul, sqrt(sum((shares5.list.cumul[[j]] - shares5.return.mean.cumul[j])^2)))
    index.list.cumul[[j]] <- c(index.data$ret.adjusted.prices[2:(1+j)])
    index.return.std.dev.cumul <- c(index.return.std.dev.cumul, sqrt(sum((index.list.cumul[[j]] - index.return.mean.cumul[j])^2)))
  }
  return.data <- return.data %>% add_column(shares1.return.std.dev = shares1.return.std.dev.cumul)
  return.data <- return.data %>% add_column(shares2.return.std.dev = shares2.return.std.dev.cumul)
  return.data <- return.data %>% add_column(shares3.return.std.dev = shares3.return.std.dev.cumul)
  return.data <- return.data %>% add_column(shares4.return.std.dev = shares4.return.std.dev.cumul)
  return.data <- return.data %>% add_column(shares5.return.std.dev = shares5.return.std.dev.cumul)
  return.data <- return.data %>% add_column(index.return.std.dev = index.return.std.dev.cumul)
  
  shares1.return.semi.dev.cumul <- c()
  shares2.return.semi.dev.cumul <- c()
  shares3.return.semi.dev.cumul <- c()
  shares4.return.semi.dev.cumul <- c()
  shares5.return.semi.dev.cumul <- c()
  index.return.semi.dev.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    shares1.list.cumul[[j]] <- c(shares1.data$ret.adjusted.prices[2:(1+j)])
    shares2.list.cumul[[j]] <- c(shares2.data$ret.adjusted.prices[2:(1+j)])
    shares3.list.cumul[[j]] <- c(shares3.data$ret.adjusted.prices[2:(1+j)])
    shares4.list.cumul[[j]] <- c(shares4.data$ret.adjusted.prices[2:(1+j)])
    shares5.list.cumul[[j]] <- c(shares5.data$ret.adjusted.prices[2:(1+j)])
    shares1.return.semi.dev.values <- shares1.list.cumul[[j]][shares1.list.cumul[[j]] <= mean(shares1.list.cumul[[j]])]
    shares2.return.semi.dev.values <- shares2.list.cumul[[j]][shares2.list.cumul[[j]] <= mean(shares2.list.cumul[[j]])]
    shares3.return.semi.dev.values <- shares3.list.cumul[[j]][shares3.list.cumul[[j]] <= mean(shares3.list.cumul[[j]])]
    shares4.return.semi.dev.values <- shares4.list.cumul[[j]][shares4.list.cumul[[j]] <= mean(shares4.list.cumul[[j]])]
    shares5.return.semi.dev.values <- shares5.list.cumul[[j]][shares5.list.cumul[[j]] <= mean(shares5.list.cumul[[j]])]
    shares1.return.semi.dev.cumul <- c(shares1.return.semi.dev.cumul, sqrt((1/length(shares1.return.semi.dev.values))*sum((shares1.return.semi.dev.values - mean(shares1.list.cumul[[j]]))^2)))
    shares2.return.semi.dev.cumul <- c(shares2.return.semi.dev.cumul, sqrt((1/length(shares2.return.semi.dev.values))*sum((shares2.return.semi.dev.values - mean(shares2.list.cumul[[j]]))^2)))
    shares3.return.semi.dev.cumul <- c(shares3.return.semi.dev.cumul, sqrt((1/length(shares3.return.semi.dev.values))*sum((shares3.return.semi.dev.values - mean(shares3.list.cumul[[j]]))^2)))
    shares4.return.semi.dev.cumul <- c(shares4.return.semi.dev.cumul, sqrt((1/length(shares4.return.semi.dev.values))*sum((shares4.return.semi.dev.values - mean(shares4.list.cumul[[j]]))^2)))
    shares5.return.semi.dev.cumul <- c(shares5.return.semi.dev.cumul, sqrt((1/length(shares5.return.semi.dev.values))*sum((shares5.return.semi.dev.values - mean(shares5.list.cumul[[j]]))^2)))
    index.list.cumul[[j]] <- c(index.data$ret.adjusted.prices[2:(1+j)])
    index.return.semi.dev.values <- index.list.cumul[[j]][index.list.cumul[[j]] <= mean(index.list.cumul[[j]])]
    index.return.semi.dev.cumul <- c(index.return.semi.dev.cumul, sqrt((1/length(index.return.semi.dev.values))*sum((index.return.semi.dev.values - mean(index.list.cumul[[j]]))^2)))
  }
  return.data <- return.data %>% add_column(shares1.return.semi.dev = shares1.return.semi.dev.cumul)
  return.data <- return.data %>% add_column(shares2.return.semi.dev = shares2.return.semi.dev.cumul)
  return.data <- return.data %>% add_column(shares3.return.semi.dev = shares3.return.semi.dev.cumul)
  return.data <- return.data %>% add_column(shares4.return.semi.dev = shares4.return.semi.dev.cumul)
  return.data <- return.data %>% add_column(shares5.return.semi.dev = shares5.return.semi.dev.cumul)
  return.data <- return.data %>% add_column(index.return.semi.dev = index.return.semi.dev.cumul)
  
  shares1.return.down.dev.cumul <- c()
  shares2.return.down.dev.cumul <- c()
  shares3.return.down.dev.cumul <- c()
  shares4.return.down.dev.cumul <- c()
  shares5.return.down.dev.cumul <- c()
  index.return.down.dev.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    shares1.list.cumul[[j]] <- c(shares1.data$ret.adjusted.prices[2:(1+j)])
    shares2.list.cumul[[j]] <- c(shares2.data$ret.adjusted.prices[2:(1+j)])
    shares3.list.cumul[[j]] <- c(shares3.data$ret.adjusted.prices[2:(1+j)])
    shares4.list.cumul[[j]] <- c(shares4.data$ret.adjusted.prices[2:(1+j)])
    shares5.list.cumul[[j]] <- c(shares5.data$ret.adjusted.prices[2:(1+j)])
    shares1.return.down.dev.values <- shares1.list.cumul[[j]][shares1.list.cumul[[j]] <= 0]
    shares2.return.down.dev.values <- shares2.list.cumul[[j]][shares2.list.cumul[[j]] <= 0]
    shares3.return.down.dev.values <- shares3.list.cumul[[j]][shares3.list.cumul[[j]] <= 0]
    shares4.return.down.dev.values <- shares4.list.cumul[[j]][shares4.list.cumul[[j]] <= 0]
    shares5.return.down.dev.values <- shares5.list.cumul[[j]][shares5.list.cumul[[j]] <= 0]
    shares1.return.down.dev.cumul <- c(shares1.return.down.dev.cumul, sqrt((1/length(shares1.return.down.dev.values))*sum((shares1.return.down.dev.values)^2)))
    shares2.return.down.dev.cumul <- c(shares2.return.down.dev.cumul, sqrt((1/length(shares2.return.down.dev.values))*sum((shares2.return.down.dev.values)^2)))
    shares3.return.down.dev.cumul <- c(shares3.return.down.dev.cumul, sqrt((1/length(shares3.return.down.dev.values))*sum((shares3.return.down.dev.values)^2)))
    shares4.return.down.dev.cumul <- c(shares4.return.down.dev.cumul, sqrt((1/length(shares4.return.down.dev.values))*sum((shares4.return.down.dev.values)^2)))
    shares5.return.down.dev.cumul <- c(shares5.return.down.dev.cumul, sqrt((1/length(shares5.return.down.dev.values))*sum((shares5.return.down.dev.values)^2)))
    index.list.cumul[[j]] <- c(index.data$ret.adjusted.prices[2:(1+j)])
    index.return.down.dev.values <- index.list.cumul[[j]][index.list.cumul[[j]] <= 0]
    index.return.down.dev.cumul <- c(index.return.down.dev.cumul, sqrt((1/length(index.return.down.dev.values))*sum((index.return.down.dev.values)^2)))
  }
  return.data <- return.data %>% add_column(shares1.return.down.dev = shares1.return.down.dev.cumul)
  return.data <- return.data %>% add_column(shares2.return.down.dev = shares2.return.down.dev.cumul)
  return.data <- return.data %>% add_column(shares3.return.down.dev = shares3.return.down.dev.cumul)
  return.data <- return.data %>% add_column(shares4.return.down.dev = shares4.return.down.dev.cumul)
  return.data <- return.data %>% add_column(shares5.return.down.dev = shares5.return.down.dev.cumul)
  return.data <- return.data %>% add_column(index.return.down.dev = index.return.down.dev.cumul)
  
  beta1.cumul <-c()
  beta2.cumul <-c()
  beta3.cumul <-c()
  beta4.cumul <-c()
  beta5.cumul <-c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    shares1.list.cumul[[j]] <- c(shares1.data$ret.adjusted.prices[2:(1+j)])
    shares2.list.cumul[[j]] <- c(shares2.data$ret.adjusted.prices[2:(1+j)])
    shares3.list.cumul[[j]] <- c(shares3.data$ret.adjusted.prices[2:(1+j)])
    shares4.list.cumul[[j]] <- c(shares4.data$ret.adjusted.prices[2:(1+j)])
    shares5.list.cumul[[j]] <- c(shares5.data$ret.adjusted.prices[2:(1+j)])
    index.list.cumul[[j]] <- c(index.data$ret.adjusted.prices[2:(1+j)])
    beta1.cumul <- c(beta1.cumul, cov(shares1.list.cumul[[j]], index.list.cumul[[j]])/var(shares1.list.cumul[[j]]))
    beta2.cumul <- c(beta2.cumul, cov(shares2.list.cumul[[j]], index.list.cumul[[j]])/var(shares2.list.cumul[[j]]))
    beta3.cumul <- c(beta3.cumul, cov(shares3.list.cumul[[j]], index.list.cumul[[j]])/var(shares3.list.cumul[[j]]))
    beta4.cumul <- c(beta4.cumul, cov(shares4.list.cumul[[j]], index.list.cumul[[j]])/var(shares4.list.cumul[[j]]))
    beta5.cumul <- c(beta5.cumul, cov(shares5.list.cumul[[j]], index.list.cumul[[j]])/var(shares5.list.cumul[[j]]))
  }
  return.data <- return.data %>% add_column(return1.beta = beta1.cumul)
  return.data <- return.data %>% add_column(return2.beta = beta2.cumul)
  return.data <- return.data %>% add_column(return3.beta = beta3.cumul)
  return.data <- return.data %>% add_column(return4.beta = beta4.cumul)
  return.data <- return.data %>% add_column(return5.beta = beta5.cumul)
  
  alpha1.cumul <- c()
  alpha2.cumul <- c()
  alpha3.cumul <- c()
  alpha4.cumul <- c()
  alpha5.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    alpha1.cumul <- c(alpha1.cumul, shares1.daily.return[j] - (tbill.yield[j] + beta1.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
    alpha2.cumul <- c(alpha2.cumul, shares2.daily.return[j] - (tbill.yield[j] + beta2.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
    alpha3.cumul <- c(alpha3.cumul, shares3.daily.return[j] - (tbill.yield[j] + beta3.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
    alpha4.cumul <- c(alpha4.cumul, shares4.daily.return[j] - (tbill.yield[j] + beta4.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
    alpha5.cumul <- c(alpha5.cumul, shares5.daily.return[j] - (tbill.yield[j] + beta5.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
  }
  return.data <- return.data %>% add_column(return1.alpha = alpha1.cumul)
  return.data <- return.data %>% add_column(return2.alpha = alpha2.cumul)
  return.data <- return.data %>% add_column(return3.alpha = alpha3.cumul)
  return.data <- return.data %>% add_column(return4.alpha = alpha4.cumul)
  return.data <- return.data %>% add_column(return5.alpha = alpha5.cumul)
  
  sharpe.ratio1.cumul <- c()
  sharpe.ratio2.cumul <- c()
  sharpe.ratio3.cumul <- c()
  sharpe.ratio4.cumul <- c()
  sharpe.ratio5.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    sharpe.ratio1.cumul <- c(sharpe.ratio1.cumul, (shares1.daily.return[j] - tbill.yield[j])/sqrt(var(shares1.list.cumul[[j]])))
    sharpe.ratio2.cumul <- c(sharpe.ratio2.cumul, (shares2.daily.return[j] - tbill.yield[j])/sqrt(var(shares2.list.cumul[[j]])))
    sharpe.ratio3.cumul <- c(sharpe.ratio3.cumul, (shares3.daily.return[j] - tbill.yield[j])/sqrt(var(shares3.list.cumul[[j]])))
    sharpe.ratio4.cumul <- c(sharpe.ratio4.cumul, (shares4.daily.return[j] - tbill.yield[j])/sqrt(var(shares4.list.cumul[[j]])))
    sharpe.ratio5.cumul <- c(sharpe.ratio5.cumul, (shares5.daily.return[j] - tbill.yield[j])/sqrt(var(shares5.list.cumul[[j]])))
  }
  return.data <- return.data %>% add_column(return1.sharpe.ratio = sharpe.ratio1.cumul)
  return.data <- return.data %>% add_column(return2.sharpe.ratio = sharpe.ratio2.cumul)
  return.data <- return.data %>% add_column(return3.sharpe.ratio = sharpe.ratio3.cumul)
  return.data <- return.data %>% add_column(return4.sharpe.ratio = sharpe.ratio4.cumul)
  return.data <- return.data %>% add_column(return5.sharpe.ratio = sharpe.ratio5.cumul)
  
  sortino.ratio1.cumul <- c()
  sortino.ratio2.cumul <- c()
  sortino.ratio3.cumul <- c()
  sortino.ratio4.cumul <- c()
  sortino.ratio5.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    sortino.ratio1.cumul <- c(sortino.ratio1.cumul, ((shares1.daily.return[j] - tbill.yield[j])/shares1.return.down.dev.cumul[j]))
    sortino.ratio2.cumul <- c(sortino.ratio2.cumul, ((shares2.daily.return[j] - tbill.yield[j])/shares2.return.down.dev.cumul[j]))
    sortino.ratio3.cumul <- c(sortino.ratio3.cumul, ((shares3.daily.return[j] - tbill.yield[j])/shares3.return.down.dev.cumul[j]))
    sortino.ratio4.cumul <- c(sortino.ratio4.cumul, ((shares4.daily.return[j] - tbill.yield[j])/shares4.return.down.dev.cumul[j]))
    sortino.ratio5.cumul <- c(sortino.ratio5.cumul, ((shares5.daily.return[j] - tbill.yield[j])/shares5.return.down.dev.cumul[j]))
  }
  return.data <- return.data %>% add_column(return1.sortino.ratio = sortino.ratio1.cumul)
  return.data <- return.data %>% add_column(return2.sortino.ratio = sortino.ratio2.cumul)
  return.data <- return.data %>% add_column(return3.sortino.ratio = sortino.ratio3.cumul)
  return.data <- return.data %>% add_column(return4.sortino.ratio = sortino.ratio4.cumul)
  return.data <- return.data %>% add_column(return5.sortino.ratio = sortino.ratio5.cumul)
  
  treynor.ratio1.cumul <- c()
  treynor.ratio2.cumul <- c()
  treynor.ratio3.cumul <- c()
  treynor.ratio4.cumul <- c()
  treynor.ratio5.cumul <- c()
  for (j in 1:(length(shares1.data$ret.adjusted.prices)-1)) {
    treynor.ratio1.cumul <- c(treynor.ratio1.cumul, (shares1.daily.return[j] - tbill.yield[j])/beta1.cumul[j])
    treynor.ratio2.cumul <- c(treynor.ratio2.cumul, (shares2.daily.return[j] - tbill.yield[j])/beta2.cumul[j])
    treynor.ratio3.cumul <- c(treynor.ratio3.cumul, (shares3.daily.return[j] - tbill.yield[j])/beta3.cumul[j])
    treynor.ratio4.cumul <- c(treynor.ratio4.cumul, (shares4.daily.return[j] - tbill.yield[j])/beta4.cumul[j])
    treynor.ratio5.cumul <- c(treynor.ratio5.cumul, (shares5.daily.return[j] - tbill.yield[j])/beta5.cumul[j])
  }
  return.data <- return.data %>% add_column(return1.treynor.ratio = treynor.ratio1.cumul)
  return.data <- return.data %>% add_column(return2.treynor.ratio = treynor.ratio2.cumul)
  return.data <- return.data %>% add_column(return3.treynor.ratio = treynor.ratio3.cumul)
  return.data <- return.data %>% add_column(return4.treynor.ratio = treynor.ratio4.cumul)
  return.data <- return.data %>% add_column(return5.treynor.ratio = treynor.ratio5.cumul)
  
  plotlist[[1]] <- ggplot() +
    geom_line(data = return.data, aes(date, shares1.return.perc, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+1])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares2.return.perc, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+2])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares3.return.perc, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+3])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares4.return.perc, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+4])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares5.return.perc, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+5])), 
              size = 1) +
    geom_line(data = return.data, aes(date, index.return.perc, 
                                      colour = paste0('^GSPC')), size = 1) +
    labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                        random.sample.stocks[(i-1)*5+2], ', ', 
                        random.sample.stocks[(i-1)*5+3], ', ', 
                        random.sample.stocks[(i-1)*5+4], ', ', 
                        random.sample.stocks[(i-1)*5+5], ', ', 'vs ^GSPC'), 
         undertitle = 'Cumulative return', x = 'date', y = 'return percentage') +
    scale_color_manual(name = 'Equities', values = c("#000000", "#FF3300", "#0066CC",
                                                     "#FF9933", "#006600", "#996633"))
  plotlist[[2]] <- ggplot() +
    geom_line(data = return.data, aes(date, shares1.return.std.dev, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+1])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares2.return.std.dev, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+2])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares3.return.std.dev, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+3])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares4.return.std.dev, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+4])), 
              size = 1) +
    geom_line(data = return.data, aes(date, shares5.return.std.dev, 
                                      colour = paste0(random.sample.stocks[(i-1)*5+5])), 
              size = 1) +
    geom_line(data = return.data, aes(date, index.return.std.dev, 
                                      colour = paste0('^GSPC')), size = 1) +
    labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                        random.sample.stocks[(i-1)*5+2], ', ', 
                        random.sample.stocks[(i-1)*5+3], ', ', 
                        random.sample.stocks[(i-1)*5+4], ', ', 
                        random.sample.stocks[(i-1)*5+5], ', ', 'vs ^GSPC'), 
         undertitle = 'Cumulative Standard Deviation', x = 'date', y = 'Standard Deviation') +
    scale_color_manual(name = 'Equities', values = c("#000000", "#FF3300", "#0066CC",
                                                     "#FF9933", "#006600", "#996633"))
    plotlist[[3]] <- ggplot() +
      geom_line(data = return.data, aes(date, shares1.return.semi.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares2.return.semi.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares3.return.semi.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares4.return.semi.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares5.return.semi.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      geom_line(data = return.data, aes(date, index.return.semi.dev, 
                                        colour = paste0('^GSPC')), size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5], ', ', 'vs ^GSPC'), 
           undertitle = 'Cumulative Semi Deviation', x = 'date', y = 'Semi Deviation') +
      scale_color_manual(name = 'Equities', values = c("#000000", "#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    plotlist[[4]] <- ggplot() +
      geom_line(data = return.data, aes(date, shares1.return.down.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares2.return.down.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares3.return.down.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares4.return.down.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, shares5.return.down.dev, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      geom_line(data = return.data, aes(date, index.return.down.dev, 
                                        colour = paste0('^GSPC')), size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5], ', ', 'vs ^GSPC'), 
           undertitle = 'Cumulative Down Deviation', x = 'date', y = 'Down Deviation') +
      scale_color_manual(name = 'Equities', values = c("#000000", "#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    plotlist[[5]] <- ggplot() +
      geom_line(data = return.data, aes(date, return1.beta, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return2.beta, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return3.beta, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return4.beta, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return5.beta, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5]), 
           undertitle = 'Cumulative Beta', x = 'date', y = 'Beta') +
      scale_color_manual(name = 'Equities', values = c("#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    plotlist[[6]] <- ggplot() +
      geom_line(data = return.data, aes(date, return1.alpha, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return2.alpha, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return3.alpha, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return4.alpha, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return5.alpha, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5]), 
           undertitle = 'Cumulative Alpha', x = 'date', y = 'Alpha') +
      scale_color_manual(name = 'Equities', values = c("#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    plotlist[[7]] <- ggplot() +
      geom_line(data = return.data, aes(date, return1.sharpe.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return2.sharpe.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return3.sharpe.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return4.sharpe.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return5.sharpe.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5]), 
           undertitle = 'Cumulative Sharpe Ratio', x = 'date', y = 'Sharpe Ratio') +
      scale_color_manual(name = 'Equities', values = c("#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    plotlist[[8]] <- ggplot() +
      geom_line(data = return.data, aes(date, return1.sortino.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return2.sortino.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return3.sortino.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return4.sortino.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return5.sortino.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5]), 
           undertitle = 'Cumulative Sortino Ratio', x = 'date', y = 'Sortino Ratio') +
      scale_color_manual(name = 'Equities', values = c("#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    plotlist[[9]] <- ggplot() +
      geom_line(data = return.data, aes(date, return1.treynor.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+1])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return2.treynor.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+2])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return3.treynor.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+3])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return4.treynor.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+4])), 
                size = 1) +
      geom_line(data = return.data, aes(date, return5.treynor.ratio, 
                                        colour = paste0(random.sample.stocks[(i-1)*5+5])), 
                size = 1) +
      labs(title = paste0(random.sample.stocks[(i-1)*5+1], ', ', 
                          random.sample.stocks[(i-1)*5+2], ', ', 
                          random.sample.stocks[(i-1)*5+3], ', ', 
                          random.sample.stocks[(i-1)*5+4], ', ', 
                          random.sample.stocks[(i-1)*5+5]), 
           undertitle = 'Cumulative Treynor Ratio', x = 'date', y = 'Treynor Ratio') +
      scale_color_manual(name = 'Equities', values = c("#FF3300", "#0066CC",
                                                       "#FF9933", "#006600", "#996633"))
    masterplotlist[[i]] <- list(plotlist[[1]], plotlist[[2]], plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]], plotlist[[7]], plotlist[[8]], plotlist[[9]])
}
for (i in 1:length(masterplotlist)) {
  grid.arrange(masterplotlist[[i]][[1]], masterplotlist[[i]][[2]], masterplotlist[[i]][[3]], masterplotlist[[i]][[4]], masterplotlist[[i]][[5]], masterplotlist[[i]][[6]], masterplotlist[[i]][[7]], masterplotlist[[i]][[8]], masterplotlist[[i]][[9]])
}









equities <- vector(mode = 'list', length = 25)
share.values <- c(rep(0, 25))
for (i in 1:25) {
  share.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[i])
  share.values[i] <- share.data[1,6]
  equity.return <- share.values[i] * (cumprod(1+(share.data$ret.adjusted.prices[-1]))-1)
  equities[[i]] <- equity.return
}
share.weights <- share.values/sum(share.values)
inv.capital <- sum(share.values)
cumul.return <- 0
for (i in 1:25) {
  cumul.return <- cumul.return + (share.weights[i] * equities[[i]])
}
cumul.perc <- cumul.return/inv.capital
index.cumul.return <- inv.capital * index.daily.return
index.cumul.perc <- index.cumul.return/inv.capital

return.data <- tibble(date = index.data$ref.date[-1])
return.data <- return.data %>% add_column(portfolio.return.perc = cumul.perc)
return.data <- return.data %>% add_column(index.return.perc = index.cumul.perc)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.perc), color = 'blue', size = 1) +
  geom_line(data = return.data, aes(date, index.return.perc), color = 'black', size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative return', x = 'date', y = 'return percentage')

portfolio.return.mean.cumul <- c()
index.return.mean.cumul <- c()
for (i in 2:(length(index.data$ret.adjusted.prices)-1)) {
  portfolio.return.mean.cumul <- c(portfolio.return.mean.cumul, 
                                mean(cumul.perc[1:i]))
  index.return.mean.cumul <- c(index.return.mean.cumul, 
                               mean(index.data$ret.adjusted.prices[2:i]))
}
portfolio.list.cumul <- vector(mode = 'list', length = 
                              (length(index.data$ret.adjusted.prices)-1))
index.list.cumul <- vector(mode = 'list', length = 
                             (length(index.data$ret.adjusted.prices)-1))
portfolio.return.std.dev.cumul <- c()
index.return.std.dev.cumul <- c()
for (j in 1:(length(index.data$ret.adjusted.prices)-1)) {
  portfolio.list.cumul[[j]] <- c(cumul.perc[1:j])
  portfolio.return.std.dev.cumul <- c(portfolio.return.std.dev.cumul, 
                                   sqrt(sum((portfolio.list.cumul[[j]] - 
                                               portfolio.return.mean.cumul[j])^2)))
  index.list.cumul[[j]] <- c(index.data$ret.adjusted.prices[2:(1+j)])
  index.return.std.dev.cumul <- c(index.return.std.dev.cumul, 
                                  sqrt(sum((index.list.cumul[[j]] - 
                                              index.return.mean.cumul[j])^2)))
}
return.data <- return.data %>% add_column(portfolio.return.std.dev = 
                                            portfolio.return.std.dev.cumul)
return.data <- return.data %>% add_column(index.return.std.dev = 
                                            index.return.std.dev.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.std.dev), color = 'blue', 
            size = 1) +
  geom_line(data = return.data, aes(date, index.return.std.dev), color = 'black', 
            size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', 
       x = 'date', y = 'Standard Deviation')

portfolio.return.semi.dev.cumul <- c()
index.return.semi.dev.cumul <- c()
for (j in 1:length(index.list.cumul)) {
  portfolio.return.cumul <- portfolio.list.cumul[[j]]
  portfolio.return.semi.dev.values <- portfolio.return.cumul[portfolio.return.cumul <= 
                                                         mean(portfolio.return.cumul)]
  portfolio.return.semi.dev.cumul <- c(portfolio.return.semi.dev.cumul, 
                                    sqrt((1/length(portfolio.return.semi.dev.values))*
                                           sum((portfolio.return.semi.dev.values - 
                                                  mean(portfolio.return.cumul))^2)))
  index.return.cumul <- index.list.cumul[[j]]
  index.return.semi.dev.values <- index.return.cumul[index.return.cumul <=
                                                       mean(index.return.cumul)]
  index.return.semi.dev.cumul <- c(index.return.semi.dev.cumul, 
                                   sqrt((1/length(index.return.semi.dev.values))*
                                          sum((index.return.semi.dev.values - 
                                                 mean(index.return.cumul))^2)))
}
return.data <- return.data %>% add_column(portfolio.return.semi.dev = 
                                            portfolio.return.semi.dev.cumul)
return.data <- return.data %>% add_column(index.return.semi.dev = 
                                            index.return.semi.dev.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.semi.dev), color = 'blue', 
            size = 1) +
  geom_line(data = return.data, aes(date, index.return.semi.dev), color = 'black', 
            size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', 
       x = 'date', y = 'Semi Deviation')

portfolio.return.down.dev.cumul <- c()
index.return.down.dev.cumul <- c()
for (j in 1:length(index.list.cumul)) {
  portfolio.return.cumul <- portfolio.list.cumul[[j]]
  portfolio.return.down.dev.values <- portfolio.return.cumul[portfolio.return.cumul <= 0]
  portfolio.return.down.dev.cumul <- c(portfolio.return.down.dev.cumul, 
                                    sqrt((1/length(portfolio.return.down.dev.values))*
                                           sum((portfolio.return.down.dev.values))^2))
  index.return.cumul <- index.list.cumul[[j]]
  index.return.down.dev.values <- index.return.cumul[index.return.cumul <= 0]
  index.return.down.dev.cumul <- c(index.return.down.dev.cumul, 
                                   sqrt((1/length(index.return.down.dev.values))*
                                          sum((index.return.down.dev.values))^2))
}
return.data <- return.data %>% add_column(portfolio.return.down.dev = 
                                            portfolio.return.down.dev.cumul)
return.data <- return.data %>% add_column(index.return.down.dev = 
                                            index.return.down.dev.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.down.dev), color = 'blue', 
            size = 1) +
  geom_line(data = return.data, aes(date, index.return.down.dev), color = 'black', 
            size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', 
       x = 'date', y = 'Down Deviation')







portfolio.return.beta.cumul <-c()
for (j in 1:length(index.list.cumul)) {
  portfolio.return.beta.cumul <- c(portfolio.return.beta.cumul, cov(portfolio.return.cumul[1:j], index.return.cumul[1:j])/var(portfolio.return.cumul[1:j]))
}
return.data <- return.data %>% add_column(portfolio.return.beta = portfolio.return.beta.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.beta), color = 'blue', size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC Beta'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Beta')


portfolio.return.alpha.cumul <- c()
for (j in 1:length(index.list.cumul)) {
  portfolio.return.alpha.cumul <- c(portfolio.return.alpha.cumul, portfolio.return.cumul[j] - (tbill.yield[j] + portfolio.return.beta.cumul[j]*(index.return.cumul[j] - tbill.yield[j])))
}
return.data <- return.data %>% add_column(portfolio.return.alpha = portfolio.return.alpha.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.alpha), color = 'blue', size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC Alpha'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Alpha')


portfolio.sharpe.ratio.cumul <- c()
for (j in 1:length(index.list.cumul)) {
  portfolio.sharpe.ratio.cumul <- c(portfolio.sharpe.ratio.cumul, (portfolio.return.cumul[j] - tbill.yield[j])/sqrt(var(portfolio.return.cumul[1:j] - tbill.yield[j])))
}
return.data <- return.data %>% add_column(portfolio.return.sharpe.ratio = portfolio.sharpe.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.sharpe.ratio), color = 'blue', size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC Sharpe Ratio'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Sharpe Ratio')


portfolio.sortino.ratio.cumul <- c()
for (j in 1:length(index.list.cumul)) {
  portfolio.sortino.ratio.cumul <- c(portfolio.sortino.ratio.cumul, ((portfolio.return.cumul[j] - tbill.yield[j])/portfolio.return.down.dev.cumul[j]))
}
return.data <- return.data %>% add_column(portfolio.return.sortino.ratio = portfolio.sortino.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.sortino.ratio), color = 'blue', size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC Sortino Ratio'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Treynor Ratio')


portfolio.treynor.ratio.cumul <- c()
for (j in 1:length(index.list.cumul)) {
  portfolio.treynor.ratio.cumul <- c(portfolio.treynor.ratio.cumul, (portfolio.return.cumul[j] - tbill.yield[j])/portfolio.return.beta.cumul[j])
}
return.data <- return.data %>% add_column(portfolio.return.treynor.ratio = portfolio.treynor.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, portfolio.return.treynor.ratio), color = 'blue', size = 1) +
  labs(title = paste0('Random Portfolio (blue) vs ^GSPC Treynor Ratio'),
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'Treynor Ratio')













