
## Algorithmic Trading Strategy

## Introduction

In this assignment, you will develop an algorithmic trading strategy by incorporating financial metrics to evaluate its profitability. This exercise simulates a real-world scenario where you, as part of a financial technology team, need to present an improved version of a trading algorithm that not only executes trades but also calculates and reports on the financial performance of those trades.

## Background

Following a successful presentation to the Board of Directors, you have been tasked by the Trading Strategies Team to modify your trading algorithm. This modification should include tracking the costs and proceeds of trades to facilitate a deeper evaluation of the algorithm’s profitability, including calculating the Return on Investment (ROI).

After meeting with the Trading Strategies Team, you were asked to include costs, proceeds, and return on investments metrics to assess the profitability of your trading algorithm.

## Objectives

1. **Load and Prepare Data:** Open and run the starter code to create a DataFrame with stock closing data.

2. **Implement Trading Algorithm:** Create a simple trading algorithm based on daily price changes.

3. **Customize Trading Period:** Choose your entry and exit dates.

4. **Report Financial Performance:** Analyze and report the total profit or loss (P/L) and the ROI of the trading strategy.

5. **Implement a Trading Strategy:** Implement a trading strategy and analyze the total updated P/L and ROI. 

6. **Discussion:** Summarise your finding.


## Instructions

### Step 1: Data Loading

Start by running the provided code cells in the "Data Loading" section to generate a DataFrame containing AMD stock closing data. This will serve as the basis for your trading decisions. First, create a data frame named `amd_df` with the given closing prices and corresponding dates. 

```r
# Load data from CSV file
amd_df <- read.csv("AMD.csv")
# Convert the date column to Date type and Adjusted Close as numeric
amd_df$date <- as.Date(amd_df$Date)
amd_df$close <- as.numeric(amd_df$Adj.Close)
amd_df <- amd_df[, c("date", "close")]
```

#### Plotting the Data
Plot the closing prices over time to visualize the price movement.
```r
plot(amd_df$date, amd_df$close,'l')
```

### Step 2: Trading Algorithm
Implement the trading algorithm as per the instructions. You should initialize necessary variables, and loop through the dataframe to execute trades based on the set conditions.

- Initialize Columns: Start by ensuring dataframe has columns 'trade_type', 'costs_proceeds' and 'accumulated_shares'.
- Change the algorithm by modifying the loop to include the cost and proceeds metrics for buys of 100 shares. Make sure that the algorithm checks the following conditions and executes the strategy for each one:
  - If the previous price = 0, set 'trade_type' to 'buy', and set the 'costs_proceeds' column to the current share price multiplied by a `share_size` value of 100. Make sure to take the negative value of the expression so that the cost reflects money leaving an account. Finally, make sure to add the bought shares to an `accumulated_shares` variable.
  - Otherwise, if the price of the current day is less than that of the previous day, set the 'trade_type' to 'buy'. Set the 'costs_proceeds' to the current share price multiplied by a `share_size` value of 100.
  - You will not modify the algorithm for instances where the current day’s price is greater than the previous day’s price or when it is equal to the previous day’s price.
  - If this is the last day of trading, set the 'trade_type' to 'sell'. In this case, also set the 'costs_proceeds' column to the total number in the `accumulated_shares` variable multiplied by the price of the last day.



```r
# Initialize columns for trade type, cost/proceeds, and accumulated shares in
amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
# Loop through each row in the dataframe to apply the trading algorithm
for (i in 1:nrow(amd_df)) {
 current_price <- amd_df$close[i] #Show the closing price for the current

 #Buy Condition: If the previous price is 0 or if the current price is
lower than the previous price
 if (previous_price == 0 || current_price < previous_price){
 amd_df$trade_type[i] <- 'buy'
 amd_df$costs_proceeds[i] <- -current_price * share_size
 accumulated_shares <- accumulated_shares + share_size

 }
 #Sell condition: Sell all accumulated shares on the last day of trading
 if (i == nrow(amd_df)){
 amd_df$trade_type[i] <- 'sell'
 amd_df$costs_proceeds[i] <- accumulated_shares * current_price
 accumulated_shares <- 0
 }
 #Update the previous price to the current price for the next iteration
 previous_price <- current_price
 amd_df$accumulated_shares[i] <- accumulated_shares
 } 
```


### Step 3: Customize Trading Period
- Define a trading period you wanted in the past five years 
```r
# The start and end date for the trading period
start_date <- as.Date("2021-12-01")
end_date <- as.Date("2022-12-01")
# Filter the data to only include information within the specified trading
period
amd_df <- subset(amd_df, date >= start_date & date <= end_date)
#Run the step 2 code again so it is filtered for the specified trading period
# Initialize columns for trade type, cost/proceeds, and accumulated shares in
amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA # Corrected column name
amd_df$accumulated_shares <- 0 # Initialize if needed for tracking
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
# Loop through each row in the dataframe to apply the trading algorithm
for (i in 1:nrow(amd_df)) {
 current_price <- amd_df$close[i]

 #Buy Condition: If the previous price is 0 or if the current price is
lower than the previous price
 if (previous_price == 0 || current_price < previous_price){
 amd_df$trade_type[i] <- 'buy'
 amd_df$costs_proceeds[i] <- -current_price * share_size
 accumulated_shares <- accumulated_shares + share_size

 }
 #Sell condition: Sell all accumulated shares on the last day of trading
 if (i == nrow(amd_df)){
 amd_df$trade_type[i] <- 'sell'
 amd_df$costs_proceeds[i] <- accumulated_shares * current_price
 accumulated_shares <- 0
 }
 previous_price <- current_price
amd_df$accumulated_shares[i] <- accumulated_shares
 }
```


### Step 4: Run Your Algorithm and Analyze Results
After running your algorithm, check if the trades were executed as expected. Calculate the total profit or loss and ROI from the trades.

- Total Profit/Loss Calculation: Calculate the total profit or loss from your trades. This should be the sum of all entries in the 'costs_proceeds' column of your dataframe. This column records the financial impact of each trade, reflecting money spent on buys as negative values and money gained from sells as positive values.
- Invested Capital: Calculate the total capital invested. This is equal to the sum of the 'costs_proceeds' values for all 'buy' transactions. Since these entries are negative (representing money spent), you should take the negative sum of these values to reflect the total amount invested.
- ROI Formula: $$\text{ROI} = \left( \frac{\text{Total Profit or Loss}}{\text{Total Capital Invested}} \right) \times 100$$

```r
# Calculate the total profit or loss
total_profit_or_loss <- sum(amd_df$costs_proceeds, na.rm=TRUE)
# Calculate the total capital invested
total_capital_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type ==
'buy'], na.rm=TRUE)
# Calculate the ROI (Return on Investment) as a percentage
ROI <- (total_profit_or_loss / total_capital_invested) * 100
# Print results
cat("Total Profit/Loss: ", total_profit_or_loss, "\n")
## Total Profit/Loss: -250090
cat("Total Capital Invested: ", total_capital_invested, "\n")
## Total Capital Invested: 1303818
cat("ROI: ", ROI, "%\n")
## ROI: -19.18135 %

```

### Step 5: Profit-Taking Strategy or Stop-Loss Mechanisum (Choose 1)
- Option 1: Implement a profit-taking strategy that you sell half of your holdings if the price has increased by a certain percentage (e.g., 20%) from the average purchase price.
- Option 2: Implement a stop-loss mechanism in the trading strategy that you sell half of your holdings if the stock falls by a certain percentage (e.g., 20%) from the average purchase price. You don't need to buy 100 stocks on the days that the stop-loss mechanism is triggered.


```r
#Create a column showing the daily average share price
amd_df$average_share_price <- NA
# Define stop-loss threshold of 10%
stop_loss_threshold <- 0.10 # 10%
#Initialise columns for trade type, cost/proceeds, and accumulated shares in
amd_df
amd_df$trade_type <- NA
amd_df$costs_proceeds <- NA
amd_df$accumulated_shares <- NA
# Initialize variables for trading logic
previous_price <- 0
share_size <- 100
accumulated_shares <- 0
average_share_price <- 0
# Loop through each row in the dataframe to apply the trading algorithm with
stop-loss mechanism
for (i in 1:nrow(amd_df)) {
 current_price <- amd_df$close[i]
 #Buy Condition: If the previous price is 0 or if the current price is lower
than the previous price
 if (i == 1 || current_price < previous_price) {
 amd_df$trade_type[i] <- 'buy'
 amd_df$costs_proceeds[i] <- -current_price * share_size
 accumulated_shares <- accumulated_shares + share_size
 average_share_price <- (average_share_price * (accumulated_shares -
share_size) + current_price * share_size) / accumulated_shares

 }
 # Stop-loss condition: Sell half of the shares if current price falls below
90% of average share price
 if (current_price < ((1 - stop_loss_threshold) * average_share_price)) {
 amd_df$trade_type[i] <- 'sell'
 amd_df$costs_proceeds[i] <- current_price * (accumulated_shares / 2)
 accumulated_shares <- (accumulated_shares / 2)
 }
 # #Sell condition: Sell all accumulated shares on the last day of trading
 if (i == nrow(amd_df)) {
 amd_df$trade_type[i] <- 'sell'
 amd_df$costs_proceeds[i] <- current_price * accumulated_shares
 accumulated_shares <- 0
 }
 previous_price <- current_price
 amd_df$accumulated_shares[i] <- accumulated_shares
 amd_df$average_share_price[i] <- ifelse(accumulated_shares > 0,
average_share_price, NA)
}

#The financial performance of this trading strategy
# Calculate total profit or loss
total_profit_or_loss <- sum(amd_df$costs_proceeds, na.rm=TRUE)
# Calculate total capital invested
total_capital_invested <- -sum(amd_df$costs_proceeds[amd_df$trade_type ==
'buy'], na.rm=TRUE)
# Calculate ROI
ROI <- (total_profit_or_loss / total_capital_invested) * 100
# Print results
cat("Total Profit or Loss: ", total_profit_or_loss, "\n")
## Total Profit or Loss: 62188.08
cat("Total Capital Invested: ", total_capital_invested, "\n")
## Total Capital Invested: 1130024
cat("ROI: ", ROI, "%\n")
## ROI: 5.503253 %

```


### Step 6: Summarize Your Findings
- Did your P/L and ROI improve over your chosen period?
- Relate your results to a relevant market event and explain why these outcomes may have occurred.


```r
print("The stop-loss mechanism that I have implemented into my complex
trading strategy has outperformed my simple trading strategy in terms of
Profit/Loss and Return on Investment (ROI). The dates I have chosen to trade
between is the 2021/12/01 (1st of December 2021) and 2022/12/01 (1st of
December 2022) which consisted of approximately a 48% decline in AMDs share
price. AMDs analyst team had claimed that this sharp decline in share price
was a result of disappointing third-quarter results in 2022 where the company
only expected a preliminary quarterly revenue of about $5.6 billion due to
reduced processor shipments which is $1 billion below their previous
forecast. This poor performance was also a result of a disastrous launch of
the AMD Radeon RX 6500 XT GPU which lacked innovation and could not compete
with what competitors like Intel and Nvidia were releasing at that time.
Overall, their performance in this trading period was influenced by market
conditions of high inflation and low supply on raw materials, alongside poor
innovation by the company in the products they released.")
## [1] "The stop-loss mechanism that I have implemented into my complex
trading strategy has outperformed my simple trading strategy in terms of
Profit/Loss and Return on Investment (ROI). The dates I have chosen to trade
between is the 2021/12/01 (1st of December 2021) and 2022/12/01 (1st of
December 2022) which consisted of approximately a 48% decline in AMDs share
price. AMDs analyst team had claimed that this sharp decline in share price
was a result of disappointing third-quarter results in 2022 where the company
only expected a preliminary quarterly revenue of about $5.6 billion due to
reduced processor shipments which is $1 billion below their previous
forecast. This poor performance was also a result of a disastrous launch of
the AMD Radeon RX 6500 XT GPU which lacked innovation and could not compete
with what competitors like Intel and Nvidia were releasing at that time.
Overall, their performance in this trading period was influenced by market
conditions of high inflation and low supply on raw materials, alongside poor
innovation by the company in the products they released."
print("I decided to implement my trading strategies during this time period
where the company was showing a weak performance to see how they would
perform in a period of declining share value. My first strategy that involved
only buying shares within this trading period produced a Total Loss of
$250,090, equating to a ROI of -19.18135%. Through analysing the given data,
it is evident that this negative ROI was produced due to a flawed trading
strategy in which I only bought shares on the first day and when the current
share price was less than the previous days share price. This trading
strategy didn't factor in the idea that I was buying shares when the share
price was higher than the value I eventually sold the shares for on the last
day of trading, contributing to a Total Loss and negative ROI. On the other
hand, my second strategy performed better than my first trading strategy over
the same trading period as it produced a Total Profit of $62,188,08 and a ROI
of 5.503253%. After analysing the provided data it can be seen that this
trading strategy performed better as I only purchased shares on the first day
and when the current share price was lower than the previous share price. I
also set a stop-loss mechanism of 10% which entailed that I would sell half
my accumulated shares when the share value dropped below 10% of the
corresponding average share price. This ensured that I was not sustaining a
greater loss by holding onto my shares in a period of declining share value.
Overall, it is evident that my second trading strategy outperformed by first
trading strategy within the given trading period as my second trading
strategy produced a ROI of 5.503253% whereas my first trading strategy
produced a ROI of -19.18135%.")
## [1] "I decided to implement my trading strategies during this time period
where the company was showing a weak performance to see how they would
perform in a period of declining share value. My first strategy that involved
only buying shares within this trading period produced a Total Loss of
$250,090, equating to a ROI of -19.18135%. Through analysing the given data,
it is evident that this negative ROI was produced due to a flawed trading
strategy in which I only bought shares on the first day and when the current
share price was less than the previous days share price. This trading
strategy didn't factor in the idea that I was buying shares when the share
price was higher than the value I eventually sold the shares for on the last
day of trading, contributing to a Total Loss and negative ROI. On the other
hand, my second strategy performed better than my first trading strategy over
the same trading period as it produced a Total Profit of $62,188,08 and a ROI
of 5.503253%. After analysing the provided data it can be seen that this
trading strategy performed better as I only purchased shares on the first day
and when the current share price was lower than the previous share price. I
also set a stop-loss mechanism of 10% which entailed that I would sell half
my accumulated shares when the share value dropped below 10% of the
corresponding average share price. This ensured that I was not sustaining a
greater loss by holding onto my shares in a period of declining share value.
Overall, it is evident that my second trading strategy outperformed by first
trading strategy within the given trading period as my second trading
strategy produced a ROI of 5.503253% whereas my first trading strategy
produced a ROI of -19.18135%."

```

Sample Discussion: On Wednesday, December 6, 2023, AMD CEO Lisa Su discussed a new graphics processor designed for AI servers, with Microsoft and Meta as committed users. The rise in AMD shares on the following Thursday suggests that investors believe in the chipmaker's upward potential and market expectations; My first strategy earned X dollars more than second strategy on this day, therefore providing a better ROI.




