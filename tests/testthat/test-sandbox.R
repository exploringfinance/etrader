
test_that("non trading sandbox", {

  # Skip on cran without credentials
  skip_on_cran()

  # Load and revalidate credentials. Need to run login script prior to running
  sandbox = TRUE
  access_tokens = readRDS('/home/rstudio/Secure/ab_sb_acctok.rds')
  etkeys = readRDS('/home/rstudio/Secure/etrd_login_ab.rds')
  etrade_cred = etrd_auth_credentials(etkeys$sand_key, etkeys$sand_secret, sandbox = sandbox)
  etrd_auth_manage_token(access_tokens = access_tokens, etrade_cred = etrade_cred, sandbox = sandbox)


  ##### Authenticate functions #####
  login = etrd_auth_login_url(etrade_cred, sandbox, auto_open = F)
  expect_equal(class(login), 'list')

  ##### Account functions #####
  # Acccount list
  actlist = etrd_account_list(sandbox = sandbox)
  expect_equal(nrow(actlist),4)
  expect_equal(ncol(actlist),9)
  expect_equal(class(etrd_account_list(output = 'list', sandbox = sandbox)),'list')

  # Account balances
  actbal = etrd_account_balance(account = actlist$accountIdKey[1], realTimeNAV = 'true', sandbox = sandbox)
  expect_equal(nrow(actbal),1)
  expect_equal(ncol(actbal),18)
  actblist = etrd_account_balance(account = actlist$accountIdKey[1], output = 'list', sandbox = sandbox)
  expect_equal(class(actblist), 'list')

  Sys.sleep(1)
  # Portfolios
  perf = etrd_account_portfolio(actlist$accountIdKey[1], totalsRequired = 'true',
                                view = 'PERFORMANCE', sandbox = sandbox)
  owatch = etrd_account_portfolio(actlist$accountIdKey[1], lotsRequired = 'true',
                                view = 'OPTIONSWATCH', sandbox = sandbox)
  compl = etrd_account_portfolio(actlist$accountIdKey[1], view = 'COMPLETE', sandbox = sandbox)
  Sys.sleep(1)
  fund = etrd_account_portfolio(actlist$accountIdKey[1], view = 'FUNDAMENTAL', sandbox = sandbox)


  quick = etrd_account_portfolio(actlist$accountIdKey[1], sandbox = sandbox)
  expect_equal(class(perf), 'list')
  expect_equal(class(owatch), 'list')
  expect_equal(class(compl), 'list')
  expect_equal(class(fund), 'list')
  expect_equal(class(quick), 'list')

  # Alerts
  Sys.sleep(1)
  alrts = etrd_alerts(actlist$accountIdKey[1], sandbox = sandbox)
  expect_true(nrow(alrts)>10)
  aldetail = etrd_alert_detail('5')
  expect_equal(class(aldetail), 'list')

  Sys.sleep(1)
  ##### Market functions #####
  # Quotes
  qts = etrd_market_quote(c('AAPL','MSFT','SPY'), sandbox = sandbox)
  expect_true(nrow(qts) >= 1)
  expect_true(ncol(qts) == 59)

  qtslst = etrd_market_quote(c('AAPL','MSFT','SPY'), detailFlag = 'intraday',
                             output = 'list', sandbox = sandbox)
  expect_equal(class(qtslst), 'list')

  # Product search
  search = etrd_product_search('tech', sandbox = sandbox)
  expect_true(nrow(search) == 10)
  expect_true(ncol(search) == 2)

  # Options expiration
  exp = etrd_option_expiration('SPY', sandbox = sandbox)
  expect_true(nrow(exp) >= 5)
  expect_true(ncol(exp) == 4)

  Sys.sleep(1)
  # Options chain
  chain_def = etrd_option_chain('SPY', sandbox = sandbox)
  expect_true(nrow(chain_def) >= 1)
  expect_true(ncol(chain_def) == 52)

  chain_filt = etrd_option_chain(symbol = 'AAPL',
                                 expiryYear = 2021,
                                 expiryMonth = 6,
                                 noOfStrikes = 20,
                                 chainType = 'call',
                                 sandbox = sandbox)
  expect_true(nrow(chain_filt) >= 1)
  expect_true(ncol(chain_filt) == 26)


  ##### Order functions #####
  ### Order History
  filterOrd = etrd_order_history(actlist$accountIdKey[1], transactionType = 'BUY', securityType = 'mf',
                     status = 'EXECUTED', fromDate = Sys.Date()-30, toDate = Sys.Date(), sandbox = sandbox)
  expect_true(nrow(filterOrd) >= 1)
  expect_true(ncol(filterOrd) == 48)

  orderList = etrd_order_history(actlist$accountIdKey[1], output = 'list', sandbox = sandbox)
  expect_equal(class(orderList), 'list')

  Sys.sleep(1)
  ### Transaction search
  transdf = etrd_transactions(actlist$accountIdKey[1], fromDate = '2020-01-01', sandbox = sandbox)
  expect_true(nrow(transdf) >= 1)
  expect_true(ncol(transdf) == 18)

  transList = etrd_transactions(actlist$accountIdKey[1], output = 'list', sandbox = sandbox)
  expect_equal(class(transList), 'list')

  ### Transaction detail
  transDet_df = etrd_transaction_detail(actlist$accountIdKey[1], transdf$transactionId[1], sandbox = sandbox)
  expect_true(nrow(transDet_df) >= 1)
  expect_true(ncol(transDet_df) == 25)

  transDet_list = etrd_transaction_detail(actlist$accountIdKey[1], transdf$transactionId[1],
                                          output = 'list', sandbox = sandbox)
  expect_equal(class(transDet_list), 'list')

})

test_that("trading sandbox", {

  # Load and revalidate credentials. Need to run login script prior to running
  sandbox = TRUE
  access_tokens = readRDS('/home/rstudio/Secure/ab_sb_acctok.rds')
  etkeys = readRDS('/home/rstudio/Secure/etrd_login_ab.rds')
  Sys.sleep(1)
  etrade_cred = etrd_auth_credentials(etkeys$sand_key, etkeys$sand_secret, sandbox = sandbox)
  etrd_auth_manage_token(access_tokens = access_tokens, etrade_cred = etrade_cred, sandbox = sandbox)
  actlist = etrd_account_list(sandbox = sandbox)
  account = actlist$accountIdKey[1]

  ticker = 'PSLV'
  previewOrder = 'none'
  actQuote = etrd_market_quote(ticker, sandbox = sandbox)

  Sys.sleep(1)
  # Limit buy order
  # etrd_place_eq_order(account = account,
  #                     symbol = 'PSLV',
  #                     quantity = 5,
  #                     quantityType = 'DOLLAR',
  #                     orderAction = 'buy',
  #                     priceType = 'limit',
  #                     limitPrice = as.numeric(actQuote$All.ask)-1,
  #                     previewOrder = previewOrder,
  #                     sandbox = sandbox)

  # limit sell order
  etrd_place_eq_order(account = account,
                      symbol = 'PSLV',
                      quantity = 5,
                      quantityType = 'DOLLAR',
                      orderAction = 'sell',
                      priceType = 'limit',
                      limitPrice = 8,
                      previewOrder = previewOrder,
                      sandbox = sandbox)


  etrd_place_eq_order(account = account,
                      symbol = 'PSLV',
                      quantity = 5,
                      quantityType = 'DOLLAR',
                      orderAction = 'buy',
                      priceType = 'TRAILING_STOP_PRCT',
                      stopPrice = 1,
                      previewOrder = previewOrder,
                      sandbox = sandbox)

  # etrd_place_eq_order(account = account,
  #                     symbol = 'PSLV',
  #                     quantity = 5,
  #                     quantityType = 'DOLLAR',
  #                     orderAction = 'sell',
  #                     priceType = 'TRAILING_STOP_CNST',
  #                     stopPrice = 1,
  #                     previewOrder = previewOrder,
  #                     sandbox = sandbox)

  # mforder = etrd_place_mf_order(account = account,
  #                               symbol = 'SWTSX',
  #                               quantity = 1,
  #                               quantityType = 'QUANTITY',
  #                               mfTransaction = 'sell',
  #                               previewOrder = 'json',
  #                               sandbox = sandbox)
  #
  # mforder = etrd_place_mf_order(account = account,
  #                               symbol = 'SWTSX',
  #                               quantityType = 'DOLLAR',
  #                               quantity = 2,
  #                               mfTransaction = 'buy',
  #                               previewOrder = 'df',
  #                               sandbox = sandbox)


  # etrd_place_optn_order(account = account,
  #                       symbol = 'FB',
  #                       callPut = 'call',
  #                       expiryYear = '2018',
  #                       expiryMonth = '12',
  #                       expiryDay  = '21',
  #                       strikePrice = '140',
  #                       quantity = 1,
  #                       orderAction = 'BUY_OPEN',
  #                       priceType = 'market',
  #                       previewOrder = 'df',
  #                       sandbox = sandbox)
  #
  # etrd_place_optn_order(account = account,
  #                       symbol = 'FB',
  #                       callPut = 'call',
  #                       expiryYear = '2018',
  #                       expiryMonth = '12',
  #                       expiryDay  = '21',
  #                       strikePrice = '140',
  #                       quantity = 1,
  #                       orderAction = 'SELL_OPEN',
  #                       priceType = 'market',
  #                       previewOrder = 'df',
  #                       sandbox = sandbox)
  #
  # etrd_place_optn_order(account = account,
  #                       symbol = 'FB',
  #                       callPut = 'call',
  #                       expiryYear = '2018',
  #                       expiryMonth = '12',
  #                       expiryDay  = '21',
  #                       strikePrice = '140',
  #                       quantity = 1,
  #                       orderAction = 'SELL_CLOSE',
  #                       priceType = 'market',
  #                       previewOrder = 'df',
  #                       sandbox = sandbox)

})
