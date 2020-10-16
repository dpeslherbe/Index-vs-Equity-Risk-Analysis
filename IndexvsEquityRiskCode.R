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
end.date <- Sys.Date()
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

stocks$df.control <- stocks$df.control %>%
  filter((threshold.decision != 'OUT'))

set.seed(1)
random.sampling.index <- sample(valid.index, 50, replace = FALSE)
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
  labs(title = paste0('MSCI (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative return', x = 'date', y = 'return percentage')

shares.return.YTD <- return.data$shares.return.perc[dim(return.data)[1]]
index.return.YTD <- return.data$index.return.perc[dim(return.data)[1]]
print(c('MSCI YTD return is ', shares.return.YTD))
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
  labs(title = paste0('MSCI (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

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
  labs(title = paste0('MSCI (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

shares.return.semi.dev.cumul.mod <- c()
index.return.semi.dev.cumul.mod <- c()
for (j in 1:length(shares.list.cumul)) {
  shares.return.cumul.mod <- shares.list.cumul[[j]]
  shares.return.semi.dev.values.mod <- shares.return.cumul.mod[shares.return.cumul.mod <= 0]
  shares.return.semi.dev.cumul.mod <- c(shares.return.semi.dev.cumul.mod, sqrt((1/length(shares.return.semi.dev.values.mod))*sum((shares.return.semi.dev.values.mod))^2))
  index.return.cumul.mod <- index.list.cumul[[j]]
  index.return.semi.dev.values.mod <- index.return.cumul.mod[index.return.cumul.mod <= mean(index.return.cumul)]
  index.return.semi.dev.cumul.mod <- c(index.return.semi.dev.cumul.mod, sqrt((1/length(index.return.semi.dev.values.mod))*sum((index.return.semi.dev.values.mod))^2))
}
return.data <- return.data %>% add_column(shares.return.semi.dev.mod = shares.return.semi.dev.cumul.mod)
return.data <- return.data %>% add_column(index.return.semi.dev.mod = index.return.semi.dev.cumul.mod)
ggplot() +
  geom_line(data = return.data, aes(date, shares.return.semi.dev.mod), color = 'blue', size = 1) +
  geom_line(data = return.data, aes(date, index.return.semi.dev.mod), color = 'black', size = 1) +
  labs(title = paste0('MSCI (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

beta.cumul <-c()
for (j in 1:length(shares.list.cumul)) {
  beta.cumul <- c(beta.cumul, cov(shares.list.cumul[[j]], index.list.cumul[[j]])/var(shares.list.cumul[[j]]))
}
return.data <- return.data %>% add_column(return.beta = beta.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.beta), color = 'blue', size = 1) +
  labs(title = paste0('MSCI vs ^GSPC Beta'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

url <- 'https://www.treasury.gov/resource-center/data-chart-center/interest-rates/pages/TextView.aspx?data=yieldYear&year=2020'
webpage <- read_html(url)
tbill.yield <- webpage %>% html_nodes('.text_view_data:nth-child(6)') %>% html_text()
tbill.yield <- as.numeric(tbill.yield)/100

alpha.cumul <- c()
for (j in 1:length(shares.list.cumul)) {
  alpha.cumul <- c(alpha.cumul, shares.daily.return[j] - (tbill.yield[j] + beta.cumul[j]*(index.daily.return[j] - tbill.yield[j])))
}
return.data <- return.data %>% add_column(return.alpha = alpha.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.alpha), color = 'blue', size = 1) +
  labs(title = paste0('MSCI vs ^GSPC Alpha'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

sharpe.ratio.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  sharpe.ratio.cumul <- c(sharpe.ratio.cumul, (shares.daily.return[j] - tbill.yield[j])/sqrt(var(shares.list.cumul[[j]])))
}
return.data <- return.data %>% add_column(return.sharpe.ratio = sharpe.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.alpha), color = 'blue', size = 1) +
  labs(title = paste0('MSCI vs ^GSPC Sharpe Ratio'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

sortino.ratio.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  sortino.ratio.cumul <- c(sortino.ratio.cumul, ((shares.daily.return[j] - tbill.yield[j])/shares.return.down.dev.cumul[j]))
}
return.data <- return.data %>% add_column(return.sortino.ratio = sortino.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.sortino.ratio), color = 'blue', size = 1) +
  labs(title = paste0('MSCI vs ^GSPC Sortino Ratio'), 
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

treynor.ratio.cumul <- c()
for (j in 1:length(shares.daily.return)) {
  treynor.ratio.cumul <- c(treynor.ratio.cumul, (shares.daily.return[j] - tbill.yield[j])/beta.cumul[j])
}
return.data <- return.data %>% add_column(return.treynor.ratio = treynor.ratio.cumul)
ggplot() +
  geom_line(data = return.data, aes(date, return.treynor.ratio), color = 'blue', size = 1) +
  labs(title = paste0('MSCI vs ^GSPC Treynor Ratio'),
       undertitle = 'Cumulative standard deviations over the year', x = 'date', y = 'return percentage')

















capital <- 100000
shares.data <- filter(stocks$df.tickers, ticker == random.sample.stocks[2])
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
  labs(title = paste0(random.sample.stocks[2], ' (blue) vs ^GSPC (black)'), 
       undertitle = 'Cumulative return', x = 'date', y = 'return percentage')

plotlist <- vector(mode = 'list', length = length(random.sample.stocks)/5)
for (i in 1:(length(random.sample.stocks)/5)) {
  capital <- 100000
  shares.data1 <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+1])
  shares.number1 <- capital/shares.data1$price.open[1]
  shares.price1 <- shares.data1$price.open[1]
  shares.data2 <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+2])
  shares.number2 <- capital/shares.data2$price.open[1]
  shares.price2 <- shares.data2$price.open[1]
  shares.data3 <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+3])
  shares.number3 <- capital/shares.data3$price.open[1]
  shares.price3 <- shares.data3$price.open[1]
  shares.data4 <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+4])
  shares.number4 <- capital/shares.data4$price.open[1]
  shares.price4 <- shares.data4$price.open[1]
  shares.data5 <- filter(stocks$df.tickers, ticker == random.sample.stocks[(i-1)*5+5])
  shares.number5 <- capital/shares.data5$price.open[1]
  shares.price5 <- shares.data5$price.open[1]
  index.data <- index$df.tickers
  index.number <- capital/index.data$price.open[1]
  index.price <- index.data$price.open[1]
  shares1.daily.return <- cumprod(1+(shares.data1$ret.adjusted.prices[-1]))-1
  shares2.daily.return <- cumprod(1+(shares.data2$ret.adjusted.prices[-1]))-1
  shares3.daily.return <- cumprod(1+(shares.data3$ret.adjusted.prices[-1]))-1
  shares4.daily.return <- cumprod(1+(shares.data4$ret.adjusted.prices[-1]))-1
  shares5.daily.return <- cumprod(1+(shares.data5$ret.adjusted.prices[-1]))-1
  index.daily.return <- cumprod(1+(index.data$ret.adjusted.prices[-1]))-1
  return.data <- tibble(date = index.data$ref.date[-1])
  return.data <- return.data %>% add_column(shares1.return.perc = shares1.daily.return)
  return.data <- return.data %>% add_column(shares2.return.perc = shares2.daily.return)
  return.data <- return.data %>% add_column(shares3.return.perc = shares3.daily.return)
  return.data <- return.data %>% add_column(shares4.return.perc = shares4.daily.return)
  return.data <- return.data %>% add_column(shares5.return.perc = shares5.daily.return)
  return.data <- return.data %>% add_column(index.return.perc = index.daily.return)
  plotlist[[i]] <- ggplot() +
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
}

grid.arrange(plotlist[[1]], plotlist[[2]])
grid.arrange(plotlist[[3]], plotlist[[4]], plotlist[[5]], plotlist[[6]])
grid.arrange(plotlist[[7]], plotlist[[8]], plotlist[[9]], plotlist[[10]])

