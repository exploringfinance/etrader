#' Get request for a specific URL
#'
#' Some functions may return a URL (for example \code{\link{etrd_option_chain}})
#' that requires a further GET request. This function can return the contents.
#' The access token and etrade credentials must be explicitly passed.
#'
#' @inheritParams etrd_account_list
#' @param etrade_url The URL for the get request
#'
#' @return content of the GET request
#' @export
#'
#' @examples
#' \dontrun{
#'
#' etrd_get_url('https://api.etrade.com/v1/market/quote/SPY:2020:11:20:PUT:185.000000',
#'              access_tokens, etrade_cred)
#'
#' }
etrd_get_url = function(etrade_url,
                        access_tokens,
                        etrade_cred) {


  # Generate signature
  signature = httr::oauth_signature(
    url = etrade_url,
    method = 'GET',
    app = etrade_cred,
    token = access_tokens$oauth_token,
    token_secret = access_tokens$oauth_token_secret
  )

  # pull account list
  get_etrade_url = httr::GET(etrade_url,
                             hlp_etrd_auth_headers(signature))

  # Check status
  hlp_etrd_status(get_etrade_url)
  url_content = httr::content(get_etrade_url)

  # Return output as list
  url_content

}


# -------------- Function 1 hlp_etrd_auth_headers -------------------------
# Turn signature into a text string for header
hlp_etrd_auth_headers = function(signature) {

  ### Use httr header signature
  httr::oauth_header(signature)

}


# -------------- Function 2 hlp_etrd_status -------------------------
# Check if function did not return 200 send back error code
hlp_etrd_status = function(x,msg=NULL){

  # Check if status code is 200 or 201 (201 is specific to orders)
  SC = x$status_code
  if (SC!=200) {

    ErrMsg = x$headers$`www-authenticate`
    # Default to TD Error message and append custom if needed
    if(is.null(ErrMsg)) ErrMsg = rvest::html_text(httr::content(x))
    ErrMsg = substr(ErrMsg,gregexpr('HTTP Status',ErrMsg),nchar(ErrMsg))
    stop(paste0(SC,' - ',ErrMsg,msg), call. = FALSE)

  }

}


# -------------- Function 3 hlp_etrd_check_cred -------------------------
# Validate if passed App is valid or Get App from Options if available.
# No authentication, just retrieving
hlp_etrd_check_cred <- function(etrade_cred, sandbox) {

  # Validate credentials if passed by user
  if (!is.null(etrade_cred)) {

    # check that credentials were created using etrd_auth_credentials
    ErrMsg = paste0('Incorrect object type passed as credentials. ',
                    'Please pass the output from `etrd_auth_credentials` using a valid ETrade key and secret.')
    if (!methods::is(etrade_cred, 'oauth_app')) stop(ErrMsg, call. = FALSE)

    return(etrade_cred)
  }

  # Get credentials from default variable based on sandbox declaration
  if(sandbox) {etrade_cred = getOption('etrade_cred_sand')} else {etrade_cred = getOption('etrade_cred_prod')}


  if (!is.null(etrade_cred)) {

    # Return only the token from the list
    return(etrade_cred)
  }

  # If no access token has been passed or pulled from default, show error
  stop(paste0("ETrade credentials have not yet been set. Please pass a valid ETrade key and secret ",
              "to the `etrd_auth_credentials` function. See the README for more details.", call. = FALSE))


}


# -------------- Function 4 hlp_etrd_check_acctok -------------------------
# Validate if passed access tokens is valid or Get Tokens from Options if available.
# No authentication, just retrieving
hlp_etrd_check_acctok <- function(access_tokens, sandbox) {

  # Validate credentials if passed by user
  if (!is.null(access_tokens)) {

    # check that credentials were created using etrd_auth_credentials
    ErrMsg = paste0('Incorrect object type passed as Access Token. ',
                    'Please pass the output from `etrd_auth_access_token`.')
    if (!methods::is(access_tokens, 'list')) stop(ErrMsg, call. = FALSE)
    if (length(access_tokens) != 4) stop(ErrMsg, call. = FALSE)

    return(access_tokens)
  }

  # Get credentials from default variable based on sandbox declaration
  if(sandbox) {access_tokens = getOption('etrade_acctok_sand')} else {access_tokens = getOption('etrade_acctok_prod')}


  if (!is.null(access_tokens)) {

    # Return only the token from the list
    return(access_tokens)
  }

  # If no access token has been passed or pulled from default, show error
  stop(paste0("ETrade Access Tokens have not yet been generated. Please use `etrd_auth_access_token` ",
              "to generate valid Access Tokens. See the README for more details.", call. = FALSE))


}
