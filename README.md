
<!-- README.md is generated from README.Rmd. Please edit that file -->

# etrader

<!-- badges: start -->

![CRAN
Version](https://www.r-pkg.org/badges/version/etrader?color=green) ![Dev
Version](https://img.shields.io/badge/github-0.1.4-blue.svg)
![Downloads](https://cranlogs.r-pkg.org/badges/grand-total/etrader)
<!-- badges: end -->

R package for the ETRADE API, facilitating authentication, trading,
pricing data, account balances, positions, order history, option chains,
and more. After creating an account, obtain [developer authorization
keys](https://developer.etrade.com/getting-started) to use the API. This
package was built for personal use tokens not vendor tokens. See this
[article](https://exploringfinance.github.io/posts/2020-11-21-the-ultimate-dollar-cost-averaging-strategy/)
for a full example of obtaining keys, logging in, and automating a trade
strategy in production.

## Disclosure

This software is in no way affiliated, endorsed, or approved by ETRADE
or any of its affiliates. It comes with absolutely no warranty and
should not be used in actual trading unless the user can read and
understand the source code. The functions within this package have been
tested under basic scenarios. There may be bugs or issues that could
prevent a user from executing trades or canceling trades. It is also
possible trades could be submitted in error. The user will use this
package at their own risk.

It is highly recommended to use the
[sandbox](https://developer.etrade.com/getting-started/developer-guides#tab_1)
environment prior to trading. Please note, as stated in the
documentation, the sandbox environment will return trades different from
what was entered. The sandbox is to verify the entry is valid.

## Installation

You can install etrader using:

``` r
# Available on CRAN
install.packages("etrader")

# Install development version - same as CRAN
# install.packages("devtools")
devtools::install_github("exploringfinance/etrader")

```

## Authentication

Accessing the ETRADE API requires the use of authorization keys that can
be obtained through the [ETRADE Developer
site](https://developer.etrade.com/getting-started). To use the API, the
user will need to manually log into their ETRADE account at least once a
day; however, this can be automated using `RSelenium` and `Docker` as
shown in this
[article](https://exploringfinance.github.io/posts/2020-11-21-the-ultimate-dollar-cost-averaging-strategy/).

Once authorization tokens have been obtained, authentication can be
achieved in three simple steps: setting credentials, generating a login
URL, and requesting access tokens. Below are all the steps required.
Jump to `Authentication Example` to see the R code example.

1.  Visit the [ETRADE Developer
    site](https://developer.etrade.com/getting-started)
2.  Complete the API User Intent Survey, API Agreement, and Annual
    Attestation
3.  This will generate a key and secret for both a sandbox and
    production environment
4.  Use `etrd_auth_credentials` to set the key and secret for the
    environment
5.  Generate a login URL using `etrd_auth_login_url`
6.  After logging in and accepting, pass the verification code to
    `etrd_auth_access_token`

This will generate Access Tokens that will be good until midnight
Eastern Time. Please be sure to indicate at each step whether sandbox or
production tokens are being used or the authentication will fail.
Production environment is assumed by default.

Steps 1-3 only need to be completed once. 4-6 need to be completed each
trading day but can be automated with `RSelenium` and `Docker`.

## Sandbox Authentication Example

Below is an example to log into the Sandbox environment. This is used to
test order entry and other calls that may be made. To log into
production, ensure to use production API keys and also remove “sandbox =
TRUE”. The entire authentication process must be completed within a few
minutes or the tokens will expire.

Once the authentication is complete, the access tokens are valid for the
rest of the trading day unless there is no activity for two hours.

``` r
# --------- Step 1 -----------
# Obtain a key and secret from ETRADE (https://developer.etrade.com/getting-started)
# Set the key and secret as credentials into an oauth app using etrd_auth_credentials
# sandbox flag defaults to production

# Retrieve ETRADE credentials from secure location
sandbox = TRUE
etkeys = readRDS('/home/rstudio/Secure/etrd_login_ab.rds')
etrade_cred = etrd_auth_credentials(etkeys$sand_key, etkeys$sand_secret, sandbox = sandbox)

# --------- Step 2 -----------
# Generate a URL and log into ETRADE (the site will open automatically)
# The URL may need to be re-pasted after login if you end up on a standaed ETRADE screen
# Credentials are passed by default

loginURL = etrd_auth_login_url(sandbox = sandbox)

# --------- Step 3 -----------
# After a successful login, paste the verification code as shown

access_tok = etrd_auth_access_token(verif_code = 'XX123',sandbox = sandbox)

# If successful, the rest of the etrader functions can now be used
# This process can be automated using Docker and Selenium

# --------- Optional Step 4 -----------
# Tokens expire at midnight Eastern each day and a manual login is required next day
# If two hours elapsed within the trading days, the tokens will be inactivated
# Reactive using etrd_auth_manage_token where tokens need to be explicitly passed
etrd_auth_manage_token(access_tokens = access_tok, etrade_cred = etrade_cred, sandbox = sandbox)
```

## Account Data

Get account data. Access tokens are passed by default.

``` r
# Auth parameters are set by default
sandbox = TRUE
actdata = etrd_account_list(sandbox = sandbox)

str(actdata)

# 'data.frame': 4 obs. of  9 variables:
# $ accountId      : chr  "xxxx"
# $ accountIdKey   : chr  "xxxx" 
# $ accountMode    : chr  "IRA" "BROKERAGE" "CASH" "CASH"
# $ accountDesc    : chr  "Brokerage" "Complete Savings" "INDIVIDUAL" "INDIVIDUAL"
# $ accountName    : chr  "NickName-1" "NickName-2" "NickName-3" ""
# $ accountType    : chr  "MARGIN" "INDIVIDUAL" "INDIVIDUAL" "CASH"
# $ institutionType: chr  "BROKERAGE" "BROKERAGE" "BROKERAGE" "BROKERAGE"
# $ accountStatus  : chr  "ACTIVE" "ACTIVE" "ACTIVE" "CLOSED"
# $ closedDate     : int  0 0 0 1

# Get account balances
account_id = actdata$accountIdKey[1]
etrd_account_balance(account_id, sandbox = sandbox)
```

## Trading

Trade functions have been split up into three categories. Equities,
Mutual Funds, and Options. The order entry for `etrader` has been
greatly simplified compared to what the [ETRADE API
offers](https://apisb.etrade.com/docs/api/order/api-order-v1.html#/definition/orderPreview).
Single order entry is the functionality currently built. Use
`etrd_cancel_order` to cancel open orders.

*Note: the sandbox environment will return previews and order outputs
different than what is submitted. Most submissions sent for execution
will fail. The objective is to confirm the order syntax is correct and
not validate entry. If an order preview is successfully generated then
the syntax works. The next recommended step is to place orders when the
market is closed in the production environment to ensure the order entry
comes through correctly.*

``` r
# Ensure trades are entered in sandbox environment
sandbox = TRUE
actlist = etrd_account_list(sandbox = sandbox)
account = actlist$accountIdKey[1]

# Specify ticker and preview type. Different ticker will be returned in sandbox
# Please note: I am not recommending SPLG I am only using it as an example
ticker = 'SPLG'
previewOrder = 'df'

#### ------- Equity orders ----------- ####
# Limit buy order
limiteq = etrd_place_eq_order(account = account,
                    symbol = ticker,
                    quantity = 5,
                    orderAction = 'buy',
                    priceType = 'limit',
                    limitPrice = 20,
                    previewOrder = previewOrder,
                    sandbox = sandbox)

# Cancel open limit order
etrd_cancel_order(limiteq$accountidKey, limiteq$orderId, sandbox = sandbox)

# stop sell order
etrd_place_eq_order(account = account,
                    symbol = ticker,
                    quantity = 5,
                    orderAction = 'sell',
                    priceType = 'stop',
                    stopPrice = 8,
                    previewOrder = previewOrder,
                    sandbox = sandbox)

# Trailing stop buy
etrd_place_eq_order(account = account,
                    symbol = ticker,
                    quantity = 5,
                    orderAction = 'buy',
                    priceType = 'TRAILING_STOP_PRCT',
                    stopPrice = 1, # Will be percent or dollar value based on priceType
                    previewOrder = previewOrder,
                    sandbox = sandbox)
                  
# market order with no preview - this will be sent immediately for execution
# PLEASE NOTE: THIS ORDER ENTRY IS NOT IN THE SANDBOX ENVIRONMENT. 
# IF THIS ORDER IS COPY/PASTED IT WILL BE EXECUTED IN PRODUCTION.
etrd_place_eq_order(account = account,
                    symbol = ticker,
                    quantity = 1,
                    orderAction = 'buy',
                    priceType = 'market',
                    previewOrder = 'none')                    


#### ------- Mutual Fund orders ----------- ####
# Please note: I am not recommending SWTSX, I am only using it as an example
# Mutual fund sell everything
mforder = etrd_place_mf_order(account = account,
                              symbol = 'SWTSX',
                              quantity = 1,
                              quantityType = 'ALL_I_OWN',
                              mfTransaction = 'sell',
                              previewOrder = 'df',
                              sandbox = sandbox)

# Mutual fund buy
mforder = etrd_place_mf_order(account = account,
                              symbol = 'SWTSX',
                              quantityType = 'DOLLAR',
                              quantity = 2,
                              mfTransaction = 'buy',
                              previewOrder = 'df',
                              sandbox = sandbox)
                              
#### ------- Option orders ----------- ####
# Please note: I am not recommending FB options, I am only using it as an example
# Buy to open
  etrd_place_optn_order(account = account,
                        symbol = 'FB',
                        callPut = 'call',
                        expiryYear = '2018',
                        expiryMonth = '12',
                        expiryDay  = '21',
                        strikePrice = '140',
                        quantity = 1,
                        orderAction = 'BUY_OPEN',
                        priceType = 'market',
                        previewOrder = 'df',
                        sandbox = sandbox)

# Sell to open
  etrd_place_optn_order(account = account,
                        symbol = 'FB',
                        callPut = 'call',
                        expiryYear = '2018',
                        expiryMonth = '12',
                        expiryDay  = '21',
                        strikePrice = '140',
                        quantity = 1,
                        orderAction = 'SELL_OPEN',
                        priceType = 'market',
                        previewOrder = 'df',
                        sandbox = sandbox)

```

## Orders and transactions

Search for orders and transactions. Get detailed transaction data for a
specific transaction.

``` r
actlist = etrd_account_list(sandbox = sandbox)

# Filter for executed 
filterOrd = etrd_order_history(actlist$accountIdKey[1], transactionType = 'BUY', 
                              securityType = 'mf', status = 'EXECUTED', 
                              fromDate = Sys.Date()-30, toDate = Sys.Date(), sandbox = sandbox)
  
  
# Get a list of transactions
transdf = etrd_transactions(actlist$accountIdKey[1], fromDate = '2020-01-01', sandbox = sandbox)

# Get transaction details
transDet_df = etrd_transaction_detail(actlist$accountIdKey[1], transdf$transactionId[1], sandbox = sandbox)
```

## Market Data

Get quotes (delayed or live depending on the user agreement with ETRADE)
and Option Chains.

``` r
  # Quotes for up to 50 securities
  qts = etrd_market_quote(c('AAPL','MSFT','SPY'), sandbox = sandbox)
 
  # Non filtered option chain
  chain_def = etrd_option_chain('SPY', sandbox = sandbox)
  
  # Filtered Option Chain
   chain_filt = etrd_option_chain(symbol = 'AAPL',
                                 expiryYear = 2021,
                                 expiryMonth = 6,
                                 noOfStrikes = 20,
                                 chainType = 'call',
                                 sandbox = sandbox)
```
