#' ETrade Quotes
#'
#' Retrieves the quote information for one or more specified symbols. To receive
#' access to real-time market data, you would need to sign the market data
#' agreement.Otherwise, you will receive delayed market data.
#'
#'
#' @param symbols One or more symbols for equities or options, up to a maximum
#'   of 50 Symbols for equities are simple, for example, GOOG. Symbols for
#'   options are more complex, consisting of six elements separated by colons,
#'   in this format: underlier:year:month:day:optionType:strikePrice.
#' @param detailFlag Determines the market fields returned from a quote request.
#'   The default is ALL. Options: ALL, FUNDAMENTAL, INTRADAY, OPTIONS, WEEK_52,
#'   MF_DETAIL
#' @inheritParams etrd_account_balance
#'
#' @return a list or data frame of quote data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get quotes for 3 symbols
#' etrd_market_quote(c('AAPL','MSFT','SPY'))
#'
#' }
etrd_market_quote = function(symbols = c('AAPL','MSFT','SPY'),
                             detailFlag = 'ALL',
                             output = c('df','list'),
                             access_tokens = NULL,
                             etrade_cred = NULL,
                             sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Colloect parameters and convert to query
  param_list = list(detailFlag = detailFlag,
                    requireEarningsDate = 'true',
                    overrideSymbolCount = 'true')

  urlquery = paste0(names(unlist(param_list)),'=',unlist(param_list),collapse = '&')

  # Generate URL
  sb = ifelse(sandbox,'sb','')
  quoteURL = paste0('https://api',sb,'.etrade.com/v1/market/quote/',paste0(symbols, collapse = ','),'?',urlquery)

  # Make get request
  quote_list = etrd_get_url(quoteURL, access_tokens, etrade_cred)

  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {quote_out = quote_list} else {
    quote_out = purrr::map_df(quote_list$QuoteResponse$QuoteData,
                              ~as.data.frame(t(unlist(.))))
  }

  # Return output
  quote_out

}


#' Product Search
#'
#' Returns a list of securities of a specified type (e.g., equity stock) based
#' on a full or partial match of any part of the company name. For instance, a
#' search for "jones" returns a list of securities associated with "Jones Soda
#' Co", "Stella Jones Inc", and many others. The list contains the company name,
#' the exchange that lists the security, the security type, and the symbol, for
#' up to 10 matches. The result may include some unexpected matches, because the
#' search includes more than just the display version of the company name. For
#' instance, searching on "etrade" returns securities for "E TRADE" - notice the
#' space in the name. This API is for searching on the company name, not a
#' security symbol. It's commonly used to look up a symbol based on the company
#' name, e.g., "What is the symbol for Google stock?". To look up company
#' information based on a symbol, or to find detailed information on a security,
#' use the quote API.
#'
#' @param search The search request
#' @inheritParams etrd_account_balance
#'
#' @return a Data frame of the search results
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # search for tech firms
#' etrd_product_search('tech')
#'
#' }
etrd_product_search = function(search = 'tech',
                               access_tokens = NULL,
                               etrade_cred = NULL,
                               sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate URL
  sb = ifelse(sandbox,'sb','')
  search_url = paste0('https://api',sb,'.etrade.com/v1/market/lookup/',search)

  # Make get request
  search_detail = etrd_get_url(search_url, access_tokens, etrade_cred)


  # Return account detail
  search_out = purrr::map_df(search_detail$LookupResponse$Data,
                             ~as.data.frame(t(unlist(.))))
  # Return output
  search_out

}



#' Option Expiration Dates
#'
#' Returns a list or data frame of dates suitable for structuring an option
#' table display. The dates are used to group option data (returned by the
#' option chains method) for a specified underlier, creating a table display.
#'
#'
#' @param symbol The symbol in the request
#' @inheritParams etrd_account_balance
#'
#' @return a data frame of expiration dates for the symbol requested
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get SPY Expirations
#' etrd_option_expiration('SPY')
#'
#' }
etrd_option_expiration = function(symbol = 'SPY',
                                  output = c('df','list'),
                                  access_tokens = NULL,
                                  etrade_cred = NULL,
                                  sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate URL
  sb = ifelse(sandbox,'sb','')
  opt_url = paste0('https://api',sb,'.etrade.com/v1/market/optionexpiredate?symbol=',symbol)

  # Make get request
  option_exp_det = etrd_get_url(opt_url, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {exp_out = option_exp_det} else {
    exp_out = purrr::map_df(option_exp_det$OptionExpireDateResponse$ExpirationDate,
                   ~dplyr::mutate_all(as.data.frame(t(unlist(.))), as.character))}

  # Return output
  exp_out

}



#' Option Chains
#'
#' Returns a list of option chains for a specific underlying instrument. The
#' request must specify an instrument, and can include the month the option
#' expires and to show calls, puts, or both. Values returned include the option
#' pair count and information about each option pair, including the type, call
#' count, symbol, product, date, and strike price.
#'
#' @param symbol The market symbol for the instrument
#' @param expiryYear Indicates the expiry year corresponding to which the
#'   optionchain needs to be fetched
#' @param expiryMonth 	Indicates the expiry month corresponding to which the
#'   optionchain needs to be fetched
#' @param expiryDay Indicates the expiry day corresponding to which the
#'   optionchain needs to be fetched
#' @param strikePriceNear The optionchians fetched will have strike price nearer
#'   to this value
#' @param noOfStrikes Indicates number of strikes for which the optionchain
#'   needs to be fetched
#' @param includeWeekly The include weekly options request. Default: false. Can
#'   also be true
#' @param skipAdjusted The skip adjusted request. Default: true. Can also be
#'   false
#' @param optionCategory The option category. Default: STANDARD. options
#'   include: STANDARD, ALL, MINI
#' @param chainType The type of option chain. Default: CALLPUT. Options include:
#'   CALL, PUT, CALLPUT
#' @param priceType The price type. Default: ATNM. Options include ATNM, ALL
#' @inheritParams etrd_account_balance
#'
#' @return a list or data frame of options chains
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get SPY Expirations
#' etrd_option_chain('SPY')
#'
#' }
etrd_option_chain = function(symbol = 'SPY',
                             expiryYear = NULL,
                             expiryMonth = NULL,
                             expiryDay = NULL,
                             strikePriceNear = NULL,
                             noOfStrikes = NULL,
                             includeWeekly = 'false',
                             skipAdjusted = 'true',
                             optionCategory = NULL,
                             chainType = NULL,
                             priceType = NULL,
                             output = c('df','list'),
                             access_tokens = NULL,
                             etrade_cred = NULL,
                             sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Colloect parameters and convert to query
  param_list = list(symbol = symbol,
                    expiryYear = expiryYear,
                    expiryMonth = expiryMonth,
                    expiryDay = expiryDay,
                    strikePriceNear = strikePriceNear,
                    noOfStrikes = noOfStrikes,
                    includeWeekly = includeWeekly,
                    skipAdjusted = skipAdjusted,
                    optionCategory = optionCategory,
                    chainType = chainType)

  urlquery = paste0(names(unlist(param_list)),'=',unlist(param_list),collapse = '&')

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  optionURL = paste0('https://api',sb,'.etrade.com/v1/market/optionchains?',urlquery)

  # Make get request
  chain_list = etrd_get_url(optionURL, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {chain_out = chain_list} else {
    chain_out = purrr::map_df(chain_list$OptionChainResponse$OptionPair, ~as.data.frame(.))
  }

  # Return output
  chain_out

}




