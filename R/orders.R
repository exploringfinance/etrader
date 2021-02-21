#' ETrade order-related information
#'
#' Provides the order details for a selected brokerage account based on the
#' search criteria provided.
#'
#' @inheritParams etrd_account_balance
#' @param count Number of orders to return with a max of 100
#' @param status a valid status: OPEN, EXECUTED, CANCELLED, INDIVIDUAL_FILLS,
#'   CANCEL_REQUESTED, EXPIRED, REJECTED, PARTIAL, DO_NOT_EXERCISE,
#'   DONE_TRADE_EXECUTED
#' @param fromDate a date object for the start of the filter
#' @param toDate a data object for the end of the filter
#' @param symbol a specific symbol to filter for
#' @param securityType a valid security type: EQ, OPTN, MF, MMF
#' @param transactionType a valid transaction type: ATNM, BUY, SELL, SELL_SHORT,
#'   BUY_TO_COVER, MF_EXCHANGE
#' @param marketSession REGULAR, EXTENDED
#'
#' @return a list or df of orders
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Search for mutual fund buys over the last month
#' account_id = etrd_account_list()$accountIdKey[1]
#' etrd_order_history(account_id, transactionType = 'BUY', securityType = 'mf',
#'                 status = 'EXECUTED', fromDate = Sys.Date()-30, toDate = Sys.Date())
#'
#' }
etrd_order_history = function(account,
                              count = 100,
                              status = NULL,
                              fromDate = Sys.Date()-30,
                              toDate = Sys.Date(),
                              symbol = NULL,
                              securityType = NULL,
                              transactionType = NULL,
                              marketSession = NULL,
                              output = c('df','list'),
                              access_tokens = NULL,
                              etrade_cred = NULL,
                              sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Colloect parameters and convert to query
  param_list = list(count = count,
                    status = status,
                    fromDate = format(as.Date(fromDate), '%m%d%Y'),
                    toDate = format(as.Date(toDate), '%m%d%Y'),
                    symbol = symbol,
                    securityType = securityType,
                    transactionType = transactionType,
                    marketSession = marketSession)

  urlquery = paste0(names(unlist(param_list)),'=',unlist(param_list),collapse = '&')

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  ordrURL = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/orders?',urlquery)

  # Make get request
  order_list = etrd_get_url(ordrURL, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {order_out = order_list} else {
    order_out = purrr::map_df(order_list$OrdersResponse$Order, ~as.data.frame(.))
  }

  # Return output
  order_out

}


#' ETrade Transaction Search
#'
#' Provides the transactions for a selected brokerage account based on the
#' search criteria provided.
#'
#' @inheritParams etrd_order_history
#' @param count Number of transactions to return in the response with a max of
#'   50. If not specified, defaults to 50.
#'
#'
#' @return a list or df of transactions
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Search for mutual fund buys over the last month
#' account_id = etrd_account_list()$accountIdKey[1]
#' etrd_transactions(account_id, fromDate = Sys.Date()-30, toDate = Sys.Date())
#'
#' }
etrd_transactions = function(account,
                             count = 50,
                             fromDate = Sys.Date()-30,
                             toDate = Sys.Date(),
                             output = c('df','list'),
                             access_tokens = NULL,
                             etrade_cred = NULL,
                             sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Colloect parameters and convert to query
  param_list = list(count = count,
                    fromDate = format(as.Date(fromDate), '%m%d%Y'),
                    toDate = format(as.Date(toDate), '%m%d%Y'))

  urlquery = paste0(names(unlist(param_list)),'=',unlist(param_list),collapse = '&')

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  tranURL = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/transactions?',urlquery)

  # Make get request
  transaction_list = etrd_get_url(tranURL, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {transaction_out = transaction_list} else {
    transaction_out = purrr::map_df(transaction_list$TransactionListResponse$Transaction,
                                    ~as.data.frame(t(unlist(.))))
  }

  # Return output
  transaction_out

}




#' ETrade Transaction Details
#'
#' Get transaction details for the specified transaction
#'
#' @inheritParams etrd_account_balance
#' @param transactionId A transaction id that comes from
#'   \code{\link{etrd_transactions}}
#'
#' @return a list or data frame of transaction details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Search for mutual fund buys over the last month
#' account_id = etrd_account_list()$accountIdKey[1]
#' transID = etrd_transactions(account_id)$transactionId[1]
#' etrd_transaction_detail(account_id, transID)
#'
#' }
etrd_transaction_detail = function(account,
                                   transactionId,
                                   output = c('df','list'),
                                   access_tokens = NULL,
                                   etrade_cred = NULL,
                                   sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  tranidURL = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/transactions/',transactionId)

  # Make get request
  transaction_det_list = etrd_get_url(tranidURL, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {tranid_out = transaction_det_list} else {
    tranid_out = data.frame(t(unlist(transaction_det_list$TransactionDetailsResponse)))}


  # Return output
  tranid_out

}




