
# CAPM Analysis

## Introduction

In this assignment, you will explore the foundational concepts of the Capital Asset Pricing Model (CAPM) using historical data for AMD and the S&P 500 index. This exercise is designed to provide a hands-on approach to understanding how these models are used in financial analysis to assess investment risks and returns.

## Background

The CAPM provides a framework to understand the relationship between systematic risk and expected return, especially for stocks. This model is critical for determining the theoretically appropriate required rate of return of an asset, assisting in decisions about adding assets to a diversified portfolio.

## Objectives

1. **Load and Prepare Data:** Import and prepare historical price data for AMD and the S&P 500 to ensure it is ready for detailed analysis.
2. **CAPM Implementation:** Focus will be placed on applying the CAPM to examine the relationship between AMD's stock performance and the overall market as represented by the S&P 500.
3. **Beta Estimation and Analysis:** Calculate the beta of AMD, which measures its volatility relative to the market, providing insights into its systematic risk.
4. **Results Interpretation:** Analyze the outcomes of the CAPM application, discussing the implications of AMD's beta in terms of investment risk and potential returns.

## Instructions

### Step 1: Data Loading

- We are using the `quantmod` package to directly load financial data from Yahoo Finance without the need to manually download and read from a CSV file.
- `quantmod` stands for "Quantitative Financial Modelling Framework". It was developed to aid the quantitative trader in the development, testing, and deployment of statistically based trading models.
- Make sure to install the `quantmod` package by running `install.packages("quantmod")` in the R console before proceeding.

```r
# Set start and end dates
start_date <- as.Date("2019-05-20")
end_date <- as.Date("2024-05-20")

# Load data for AMD, S&P 500, and the 1-month T-Bill (DTB4WK)
amd_data <- getSymbols("AMD", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
gspc_data <- getSymbols("^GSPC", src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
rf_data <- getSymbols("DTB4WK", src = "FRED", from = start_date, to = end_date, auto.assign = FALSE)

# Convert Adjusted Closing Prices and DTB4WK to data frames
amd_df <- data.frame(Date = index(amd_data), AMD = as.numeric(Cl(amd_data)))
gspc_df <- data.frame(Date = index(gspc_data), GSPC = as.numeric(Cl(gspc_data)))
rf_df <- data.frame(Date = index(rf_data), RF = as.numeric(rf_data[,1]))  # Accessing the first column of rf_data

# Merge the AMD, GSPC, and RF data frames on the Date column
df <- merge(amd_df, gspc_df, by = "Date")
df <- merge(df, rf_df, by = "Date")
```

#### Data Processing 
```r
colSums(is.na(df))
# Fill N/A RF data
df <- df %>%
  fill(RF, .direction = "down") 
```

### Step 2: CAPM Analysis

The Capital Asset Pricing Model (CAPM) is a financial model that describes the relationship between systematic risk and expected return for assets, particularly stocks. It is widely used to determine a theoretically appropriate required rate of return of an asset, to make decisions about adding assets to a well-diversified portfolio.

#### The CAPM Formula
The formula for CAPM is given by:

$$
E(R_i) = R_f + \beta_i (E(R_m) - R_f)
$$

Where:

- $E(R_i)$ is the expected return on the capital asset,
- $R_f$ is the risk-free rate,
- $\beta_i$ is the beta of the security, which represents the systematic risk of the security,
- $E(R_m)$ is the expected return of the market.



#### CAPM Model Daily Estimation

- **Calculate Returns**: First, we calculate the daily returns for AMD and the S&P 500 from their adjusted closing prices. This should be done by dividing the difference in prices between two consecutive days by the price at the beginning of the period.
  
$$
\text{Daily Return} = \frac{\text{Today's Price} - \text{Previous Trading Day's Price}}{\text{Previous Trading Day's Price}}
$$

```r
# Calculate daily returns for AMD and the S&P 500
df <- df %>%
 mutate(
 AMD_Return = (AMD / lag(AMD) - 1), #Calculates the daily AMD return
 GSPC_Return = (GSPC / lag(GSPC)-1) #Calculates the daily S&P 500 return
 )
df <- na.omit(df) #Omits the NA values
```

- **Calculate Risk-Free Rate**: Calculate the daily risk-free rate by conversion of annual risk-free Rate. This conversion accounts for the compounding effect over the days of the year and is calculated using the formula:
  
$$
\text{Daily Risk-Free Rate} = \left(1 + \frac{\text{Annual Rate}}{100}\right)^{\frac{1}{360}} - 1
$$

```r
# Calculate daily risk-free rate
df <- df %>%
 mutate(RF_Daily = (1 + RF / 100)^(1/360) - 1) #Calculates the Daily Risk
Free Rate
```


- **Calculate Excess Returns**: Compute the excess returns for AMD and the S&P 500 by subtracting the daily risk-free rate from their respective returns.

```r
# Calculate excess returns
df <- df %>%
 mutate(
 AMD_Excess_Return = AMD_Return - RF_Daily, #Calculates the Excess Returns
for AMD
 GSPC_Excess_Return = GSPC_Return - RF_Daily #Calculates the Excess
Returns for S&P 500
 )
```


- **Perform Regression Analysis**: Using linear regression, we estimate the beta (\(\beta\)) of AMD relative to the S&P 500. Here, the dependent variable is the excess return of AMD, and the independent variable is the excess return of the S&P 500. Beta measures the sensitivity of the stock's returns to fluctuations in the market.

```r
# Perform regression analysis
capm_model <- lm(AMD_Excess_Return ~ GSPC_Excess_Return, data = df)
summary(capm_model)
##
## Call:
## lm(formula = AMD_Excess_Return ~ GSPC_Excess_Return, data = df)
##
## Residuals:
## Min 1Q Median 3Q Max
## -0.095781 -0.014735 -0.001152 0.012276 0.173632
##
## Coefficients:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 0.0011041 0.0007243 1.524 0.128
## GSPC_Excess_Return 1.5699987 0.0540654 29.039 <2e-16 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 0.02567 on 1256 degrees of freedom
## Multiple R-squared: 0.4017, Adjusted R-squared: 0.4012
## F-statistic: 843.3 on 1 and 1256 DF, p-value: < 2.2e-16

```


#### Interpretation

What is your \(\beta\)? Is AMD more volatile or less volatile than the market?

**Answer:**


#### Plotting the CAPM Line
Plot the scatter plot of AMD vs. S&P 500 excess returns and add the CAPM regression line.

```r
# Load necessary library for plotting
# Plotting the CAPM line
library(ggplot2)
ggplot(df, aes(x = GSPC_Excess_Return, y = AMD_Excess_Return)) +
 geom_point(alpha = 0.5) +
 geom_smooth(method = "lm", se = FALSE, col = "red") +
 labs(
 title = "CAPM Analysis: AMD vs S&P 500",
 x = "S&P 500 Excess Return",
 y = "AMD Excess Return"
 )
## `geom_smooth()` using formula = 'y ~ x'
```

### Step 3: Predictions Interval
Suppose the current risk-free rate is 5.0%, and the annual expected return for the S&P 500 is 13.3%. Determine a 90% prediction interval for AMD's annual expected return.



**Answer:**

```r
# Given values
current_rf_rate <- 0.05
expected_market_return <- 0.133
# Extract beta and residual standard error from the model
beta <- coef(capm_model)["GSPC_Excess_Return"]
residuals <- capm_model$residuals
se <- sd(residuals)
# Daily to annual conversion
annual_se <- se * sqrt(252)
# Calculates the Expected Annual Return
expected_amd_return <- current_rf_rate + beta * (expected_market_return -
current_rf_rate)
# Calculated the 90% prediction interval
z_score <- qnorm(0.95)
lower_bound <- expected_amd_return - z_score * annual_se
upper_bound <- expected_amd_return + z_score * annual_se
# Print results
cat("Expected AMD Annual Return:", expected_amd_return * 100, "%\n")
## Expected AMD Annual Return: 18.03099 %
cat("90% Prediction Interval: [", lower_bound * 100, "%, ", upper_bound *
100, "%]\n")
## 90% Prediction Interval: [ -48.9696 %, 85.03158 %]
print("Therefore, the 90% prediction interval for AMD's annual expected
return is approximately [-49%,85%], reflecting the range within which we
expect AMD's returns to fall with 90% confidence, given the market's expected
return and the beta value of 1.57.")
## [1] "Therefore, the 90% prediction interval for AMD's annual expected
return is approximately [-49%,85%], reflecting the range within which we
expect AMD's returns to fall with 90% confidence, given the market's expected
return and the beta value of 1.57."
```
