#install.packages(c("tidyquant",
#                   "plotly",
#                   "timetk"))
#install.packages("tidyr")
#install.packages("timetk")
#install.packages("forcats")

library(tidyquant)
library(plotly)
library(timetk)
library(tidyr)
library(forcats)


############ GET DATA ##############
tick <- c("AOT.BK", "TISCO.BK", "PTT.BK", "EA.BK", "CPALL.BK")

price_data <- tq_get(tick,
                     from = '2021-01-01',
                     to = '2022-03-31',
                     get = 'stock.prices')

########## Find Daily return #############

log_ret_tidy <- price_data %>%
  group_by(symbol) %>%
  tq_transmute(select = adjusted,
               mutate_fun = periodReturn,
               period = 'daily',
               col_rename = 'ret',
               type = 'log')
head(log_ret_tidy)

log_ret_xts <- log_ret_tidy %>%
  spread(symbol, value = ret) %>%
  tk_xts()


head(log_ret_xts)


######## Transform to annual return #########

mean_ret <- colMeans(log_ret_xts)
print(round(mean_ret, 5))

cov_mat <- cov(log_ret_xts) * 252

print(round(cov_mat,4))


########## Find highest sharpe ratio ############

# Calculate the random weights
wts <- runif(n = length(tick))
wts <- wts/sum(wts)

# Calculate the portfolio returns
port_returns <- (sum(wts * mean_ret) + 1)^252 - 1

# Calculate the portfolio risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))

# Calculate the Sharpe Ratio
sharpe_ratio <- port_returns/port_risk

print(wts)
print(port_returns)
print(port_risk)
print(sharpe_ratio)



## create empty vector & matrix
num_port <- 5000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = length(tick))

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)


## run 5000 times
for (i in seq_along(port_returns)) {
  
  wts <- runif(length(tick))
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^252) - 1
  
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)


# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret_xts)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

head(portfolio_values)

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]


## minimize risks
p <- min_var %>%
  gather(AOT.BK:TISCO.BK, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Minimum Variance Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

## maximize return
p <- max_sr %>%
  gather(AOT.BK:TISCO.BK, key = Asset,
         value = Weights) %>%
  mutate(Asset = as.factor(Asset)) %>%
  ggplot(aes(x = fct_reorder(Asset,Weights), y = Weights, fill = Asset)) +
  geom_bar(stat = 'identity') +
  theme_minimal() +
  labs(x = 'Assets', y = 'Weights', title = "Tangency Portfolio Weights") +
  scale_y_continuous(labels = scales::percent) 

ggplotly(p)

max_sr


#### EF of all random portfolio
p <- portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Annualized Risk',
       y = 'Annualized Returns',
       title = "Portfolio Optimization & Efficient Frontier") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'orange') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red') +
  annotate('text', x = 0.20, y = 0.42, label = "Tangency Portfolio") +
  annotate('text', x = 0.15, y = 0.01, label = "Minimum variance portfolio") +
  annotate(geom = 'segment', x = 0.205, xend = 0.207,  y = 0.41, 
           yend = 0.377, color = 'red', arrow = arrow(type = "open")) +
  annotate(geom = 'segment', x = 0.15, xend = 0.126,  y = 0.02, 
           yend = 0.134, color = 'red', arrow = arrow(type = "open"))


ggplotly(p)
