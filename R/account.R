#' ETrade Account Data
#'
#' This API returns the account information for the current user. The
#' information returned includes account type, mode, and details. The Account ID
#' Key is also given which is required for other etrader functions.
#'
#' @inheritParams etrd_auth_manage_token
#' @param output Indicate whether the output should be in the form of a data
#'   frame ('df') or list ('list'). Data frame is returned by default.
#'
#' @return a data frame or list of account data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Once valid access tokens are obtained, the function can be run without parameters
#' etrd_account_list()
#'
#' }
etrd_account_list = function(output = c('df','list'),
                             access_tokens = NULL,
                             etrade_cred = NULL,
                             sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  act_url = paste0('https://api',sb,'.etrade.com/v1/accounts/list')

  # Make get request
  act_detail = etrd_get_url(act_url, access_tokens, etrade_cred)

  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {act_out = act_detail} else
    {act_out = dplyr::bind_rows(lapply(act_detail$AccountListResponse$Accounts$Account,data.frame))}

  # Return output
  act_out

}




#' ETrade Account Balances
#'
#' This API returns detailed balance information for a specified account for the
#' current user. The information returned includes account type, option level,
#' and details on up to four balances - account balance, margin account balance,
#' day trade balance, and cash account balance.
#'
#' @inheritParams etrd_account_list
#' @param account The account ID key, not the account ID associated with the
#'   specific account. Use \code{\link{etrd_account_list}} to identify the
#'   proper account id key.
#' @param realTimeNAV Default is false. If true, fetches real time balance
#'
#' @return df or list of account balance information
#' @export
#'
#' @examples
#' \dontrun{
#'
#' account_id = etrd_account_list()$accountIdKey[1]
#' etrd_account_balance(account_id)
#'
#' }
etrd_account_balance = function(account,
                                realTimeNAV = 'false', # Default is false. If true, fetches real time balance
                                output = c('df','list'),
                                access_tokens = NULL,
                                etrade_cred = NULL,
                                sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  bal_url = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,
                   '/balance?instType=BROKERAGE&realTimeNAV=',realTimeNAV)

  # Make get request
  bal_detail = etrd_get_url(bal_url, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {bal_out = bal_detail} else {
    bal_out = data.frame(t(unlist(bal_detail$BalanceResponse)))}

  # Return output
  bal_out

}



#' ETrade Account Portfolios
#'
#' Provides detailed portfolio information for a selected brokerage account
#'
#' @inheritParams etrd_account_balance
#' @param count The number of positions to return in the response. If not
#'   specified, defaults to 50. 50 is also the maximum.
#' @param marketSession The market session. Default: REGULAR. Options: REGULAR,
#'   EXTENDED
#' @param totalsRequired It gives the total values of the portfolio. Default:
#'   false.
#' @param lotsRequired It gives position lots for positions. Default: false.
#' @param view The view query. Default: Quick. Options: PERFORMANCE,
#'   FUNDAMENTAL, OPTIONSWATCH, QUICK, COMPLETE
#'
#' @return a list of portfolio objects based on request
#' @export
#'
#' @examples
#' \dontrun{
#'
#' account_id = etrd_account_list()$accountIdKey[1]
#' et_act_details = etrd_account_portfolio(account_id)
#' # Convert list to a data frame
#' positions = dplyr::bind_rows(lapply(et_act_details$AccountPortfolio[[1]]$Position,
#'                                    function(x) {data.frame(x)}))
#'
#' }
etrd_account_portfolio = function(account,
                                  count = 50,
                                  marketSession = 'REGULAR',
                                  totalsRequired = 'false',
                                  lotsRequired = 'false',
                                  view = 'QUICK', #PERFORMANCE, FUNDAMENTAL, OPTIONSWATCH, QUICK, COMPLETE
                                  # output = c('df','list'),
                                  access_tokens = NULL,
                                  etrade_cred = NULL,
                                  sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Colloect parameters and convert to query
  param_list = list(count = count,
                    marketSession = marketSession,
                    totalsRequired = totalsRequired,
                    lotsRequired = lotsRequired,
                    view = view)

  urlquery = paste0(names(unlist(param_list)),'=',unlist(param_list),collapse = '&')

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  portURL = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/portfolio?',urlquery)

  # Make get request
  portfolio_list = etrd_get_url(portURL, access_tokens, etrade_cred)

  portfolio_loop = portfolio_list$PortfolioResponse
  pages = portfolio_loop$AccountPortfolio[[1]]$totalPages

  if (pages > 1) {

    url_loop = portfolio_loop$AccountPortfolio[[1]]$`next`
    url_loop = substr(url_loop,1,nchar(url_loop)-1)

    urls_loop = paste0(url_loop, 1:pages)

    portfolio_loop = lapply(urls_loop, function(x) etrd_get_url(x, access_tokens, etrade_cred)$PortfolioResponse)
    # portfolio_loop = do.call(c, portfolio_loop)

  }


  # Return output as list only. To many potential outputs
  portfolio_loop

}



#' ETrade Alert Search
#'
#' Search alerts associated with the provided ETrade Account
#'
#' @inheritParams etrd_account_balance
#' @param count The alert count. By default it returns 25. Max values that can
#'   be returned: 300.
#' @param category The alert category. By default it will return STOCK and
#'   ACCOUNT. Options: STOCK, ACCOUNT
#' @param status The alert status. By default it will return READ and UNREAD.
#'   Options: READ, UNREAD, DELETED
#' @param search The alert search. Search is done based on the subject.
#'
#'
#' @return a list or data frame of alert data
#' @export
#'
#' @examples
#' \dontrun{
#'
#' account_id = etrd_account_list()$accountIdKey[1]
#' etrd_alerts(account_id)
#'
#' }
etrd_alerts = function(account,
                       count = 25,
                       category = NULL, # STOCK, ACCOUNT
                       status = NULL, # READ, UNREAD, DELETED
                       search = NULL,
                       output = c('df','list'),
                       access_tokens = NULL,
                       etrade_cred = NULL,
                       sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Colloect parameters and convert to query
  param_list = list(count = count,
                    category = category,
                    status = status,
                    search = search)

  urlquery = paste0(names(unlist(param_list)),'=',unlist(param_list),collapse = '&')

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  alertURL = paste0('https://api',sb,'.etrade.com/v1/user/alerts?',urlquery)

  # Make get request
  alert_list = etrd_get_url(alertURL, access_tokens, etrade_cred)


  # Return account detail
  if(missing(output)) output = 'df'
  if(output != 'df') {alert_out = alert_list} else {
    alert_out = purrr::map_df(alert_list$AlertsResponse$Alert,
                                    ~as.data.frame(t(unlist(.))))
  }

  # Return output
  alert_out

}




#' ETrade Alert Details
#'
#' Get the details for a specific alert based on the alert ID
#'
#' @inheritParams etrd_account_balance
#' @param alertId An alert id that comes from \code{\link{etrd_alerts}}
#'
#' @return the alert details in list form
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Get an alert detail for a specific account and alert
#' account_id = etrd_account_list()$accountIdKey[1]
#' alert_id = etrd_alerts(account_id)$id[1]
#' etrd_alert_detail(alert_id)
#'
#' }
etrd_alert_detail = function(alertId,
                             access_tokens = NULL,
                             etrade_cred = NULL,
                             sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  tranidURL = paste0('https://api',sb,'.etrade.com/v1/user/alerts/',alertId)

  # Make get request
  alert_det_list = etrd_get_url(tranidURL, access_tokens, etrade_cred)

  # Return output
  alert_det_list

}

