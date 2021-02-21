
#' Auth Step 1: Set ETRADE Key and Secret Credentials
#'
#' Step 1 of ETRADE authentication. Create an oauth app using the key and secret
#' that can be obtained after completing the agreements on
#' \href{https://developer.etrade.com/getting-started}{ETRADE Developer}.
#'
#' Authentication into the ETRADE environment requires a three step process: set
#' ETRADE credentials, generate request tokens to create a login URL, and use
#' the verification code to create access tokens. The Key and Secret need to be
#' fed into this function to set the credentials into an oauth app that can be
#' fed into the next function: \code{\link{etrd_auth_login_url}}.
#'
#' @seealso \code{\link{etrd_auth_credentials}} to set the key and secret into
#'   an oauth app, \code{\link{etrd_auth_login_url}} to generate request tokens
#'   and create a login URL, \code{\link{etrd_auth_access_token}} to use the
#'   verification code to create access tokens,
#'   \code{\link{etrd_auth_manage_token}} to renew or revoke access tokens
#'
#' @param etrade_key Either a sandbox or production key provided by ETRADE after
#'   completing the agreements on
#'   \href{https://developer.etrade.com/getting-started}{ETRADE Developer}
#' @param etrade_secret Either a sandbox or production secret provided by ETRADE
#'   after completing the agreements on
#'   \href{https://developer.etrade.com/getting-started}{ETRADE Developer}
#' @param sandbox ETRADE offers a sandbox environment for validating API calls
#'   and responses. If using the sandbox environment, this must be set to TRUE
#'   in each function called throughout etrader. ETRADE states "Sandbox
#'   responses use stored data that's intended to provide typical responses for
#'   basic use cases. So the responses you receive will not contain current
#'   data, and may not exactly match your requests in other ways." Essentially,
#'   the responses will not match the requests entered but successful pull will
#'   indicate whether the entry was valid or not.
#'
#' @return an oauth app stored into options by default
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Pass the key and secret from ETRADE
#' etrade_cred = etrd_auth_credentials(etrade_key = 'PRODUCTION_ALPHANUMERIC_OAUTH_KEY',
#'                                     etrade_secret = 'PRODUCTION_ALPHANUMERIC_OAUTH_SECRET')
#'
#' }
etrd_auth_credentials = function(etrade_key, etrade_secret, sandbox = FALSE) {

  # Create httr oauth app for downstream oauth functions
  etrade_cred <- httr::oauth_app(
    "etrade",
    key = etrade_key,
    secret = etrade_secret
  )

  # Set Tokens to a default option
  if(sandbox) {options(etrade_cred_sand = etrade_cred)}else{options(etrade_cred_prod = etrade_cred)}

  # Return Tokens in app form
  etrade_cred

}


#' Auth Step 2: Generate ETRADE URL for log in
#'
#' Step 2 of ETRADE authentication. Use the output from
#' \code{\link{etrd_auth_credentials}} to generate Request Tokens and a login
#' URL
#'
#' Authentication into the ETRADE environment requires a three step process: set
#' ETRADE credentials, generate request tokens to create a login URL, and use
#' the verification code to create access tokens. The output from
#' \code{\link{etrd_auth_credentials}} needs to be fed into this function to
#' create the URL.
#'
#' Once the URL is generated it will pop up automatically. Log into the page. If
#' the redirect is to a normal ETRADE landing page, re-paste the URL into the
#' browser. The page header should read: Indicate Terms Agreement. Press
#' "Accept" to generate a 5 digit alpha-numeric Verification Code. The
#' Verification Code will feed into \code{\link{etrd_auth_access_token}} to
#' generate Access Tokens and complete the authorization process.
#'
#' @seealso \code{\link{etrd_auth_credentials}} to set the key and secret into
#'   an oauth app, \code{\link{etrd_auth_login_url}} to generate request tokens
#'   and create a login URL, \code{\link{etrd_auth_access_token}} to use the
#'   verification code to create access tokens,
#'   \code{\link{etrd_auth_manage_token}} to renew or revoke access tokens
#'
#' @inheritParams etrd_auth_credentials
#' @param etrade_cred The output created from
#'   \code{\link{etrd_auth_credentials}} when a valid ETRADE key and secret have
#'   been passed. This entry is not required because the output is saved and
#'   retrieved from R options automatically.
#' @param auto_open indicate whether the browser should open automatically to
#'   the login URL
#'
#' @return a list of three items: a login URL valid for 5 minutes and two
#'   request tokens, a key and secret
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # The etrade_cred has been stored into options so does not need to be passed
#' loginURL = etrd_auth_login_url()
#'
#' }
etrd_auth_login_url = function(etrade_cred = NULL, sandbox = FALSE, auto_open = TRUE) {


  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)

  # Create signature for Request Token
  req_url = 'https://api.etrade.com/oauth/request_token'
  sign = httr::oauth_signature(
    url = req_url,
    method = 'GET',
    app = etrade_cred,
    other_params = c(oauth_callback = 'oob')
  )

  # Make GET Call for Request Token
  req_token = httr::GET(req_url, hlp_etrd_auth_headers(sign))


  # Check status
  hlp_etrd_status(req_token)

  # Obtain keys and save to options
  etrade_reqtok = httr::content(req_token)
  if(sandbox) {options(etrade_reqtok_sand = etrade_reqtok)}else{options(etrade_reqtok_prod = etrade_reqtok)}

  # Create and return auth URL
  login_url = paste0('https://us.etrade.com/e/t/etws/authorize?key=',sign$oauth_consumer_key,
                     '&token=',urltools::url_encode(etrade_reqtok$oauth_token))

  # Print URL and then visit it
  cat(paste0('Visit the login URL to login to Etrade. If the login brings you to a normal ETRADE screen. ',
             'Re-paste the link. Click Accept and copy the verification code. ',
             '\nNOTE: This URL is only valid for 5 minutes.\n'))

  # Open Browser by default
  if(auto_open) utils::browseURL(login_url)

  # return login URL
  urltok = list(loginURL = login_url, req_key = etrade_reqtok$oauth_token, req_secret = etrade_reqtok$oauth_token_secret)
  urltok
}



#' Auth Step 3: Create Access Tokens to use etrader
#'
#' Step 3 of ETRADE authentication. Pass the verification code generated after a
#' successful log into the URL created from \code{\link{etrd_auth_login_url}}.
#'
#' Authentication into the ETRADE environment requires a three step process: set
#' ETRADE credentials, generate request tokens to create a login URL, and use
#' the verification code to create access tokens. The verification code is
#' generated after successfully logging into the URL that comes from
#' \code{\link{etrd_auth_login_url}}.
#'
#' The access tokens expire each day at midnight Eastern Time. The next day Auth
#' step 1-3 will be required to get new access tokens. If two hours have elapsed
#' between API calls during a trading day, the access token is inactivated. Use
#' \code{\link{etrd_auth_manage_token}} to refresh an inactivated access token.
#'
#' @seealso \code{\link{etrd_auth_credentials}} to set the key and secret into
#'   an oauth app, \code{\link{etrd_auth_login_url}} to generate request tokens
#'   and create a login URL, \code{\link{etrd_auth_access_token}} to use the
#'   verification code to create access tokens,
#'   \code{\link{etrd_auth_manage_token}} to renew or revoke access tokens
#'
#' @inheritParams etrd_auth_login_url
#' @param verif_code a 5 digit alpha numeric code created after successfully
#'   logging into the ETRADE URL generated from
#'   \code{\link{etrd_auth_login_url}}
#'
#' @return a key and secret oauth access token
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # The etrade_cred has been stored into options so does not need to be passed
#' # After a successful log in, paste the verification code as shown
#' access_tok = etrd_auth_access_token(verif_code = 'XX123')
#'
#' }
etrd_auth_access_token = function(verif_code, etrade_cred=NULL,  sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)

  # Get request tokens for final credentials
  if(sandbox) {etrade_reqtok = getOption('etrade_reqtok_sand')} else {etrade_reqtok = getOption('etrade_reqtok_prod')}
  if(is.null(etrade_reqtok)) {
    stop('No Request tokens found. First use `etrd_auth_login_url` to generate a login URL and log into ETRADE.', call. = FALSE)
  }

  # Generate signature for access tokens
  acc_url = 'https://api.etrade.com/oauth/access_token'
  sign = httr::oauth_signature(
    url = acc_url,
    method = 'GET',
    app = etrade_cred,
    token = etrade_reqtok$oauth_token,
    token_secret = etrade_reqtok$oauth_token_secret,
    other_params = c(oauth_verifier = verif_code))

  # Create Access Tokens
  access_tokens = httr::GET(acc_url, hlp_etrd_auth_headers(sign))

  # Check status
  hlp_etrd_status(access_tokens)
  acctok = httr::content(access_tokens)


  acctok = httr::content(access_tokens)
  acctok$creatTime = Sys.time()
  acctok$sandbox = sandbox

  # Null out request tokens and save access tokens
  if(sandbox) {options(etrade_reqtok_sand = NULL)}else{options(etrade_reqtok_prod = NULL)}
  if(sandbox) {options(etrade_acctok_sand = acctok)}else{options(etrade_acctok_prod = acctok)}

  # Return Access Tokens
  cat(paste0("Successful authorization. etrader functions are now enabled.\n",
             "Access tokens are saved by default into options so they don't have to be passed to each function.\n"))
  acctok

}



#' Auth - Renew or Revoke Access Tokens as needed
#'
#' #' During the trading day, if two hours have elapsed between API calls, the
#' token is inactivated. Use this function to refresh an inactivated access
#' token. This function can also be used to revoke an active access token. Once
#' a token has been revoked, Auth steps 1-3 will need to be run to get new
#' access tokens.
#'
#' The access tokens expire each day at midnight Eastern Time. The
#' next day, Auth Step 1-3 will need to be run again. This function cannot renew
#' an expired access token.
#'
#'
#' @seealso \code{\link{etrd_auth_credentials}} to set the key and secret into
#'   an oauth app, \code{\link{etrd_auth_login_url}} to generate request tokens
#'   and create a login URL, \code{\link{etrd_auth_access_token}} to use the
#'   verification code to create access tokens,
#'   \code{\link{etrd_auth_manage_token}} to renew or revoke access tokens
#'
#' @inheritParams etrd_auth_login_url
#' @param access_tokens Access tokens are created using
#'   \code{\link{etrd_auth_access_token}}. This entry is not required because
#'   the output is saved and retrieved from R options automatically.
#' @param action Enter 'renew' to activate an inactive access token. Enter
#'   'revoke' to invalidate a current access token.
#'
#' @return a key and secret oauth access token
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Will renew production tokens
#' etrd_auth_manage_token('renew')
#'
#' # Will revoke a sandbox access token
#' etrd_auth_manage_token('revoke', sandbox = TRUE)
#'
#' }
etrd_auth_manage_token = function(action = c('renew','revoke'), access_tokens = NULL, etrade_cred = NULL,  sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate signature
  if(missing(action)) action = 'renew'
  mng_url = ifelse(action == 'renew', 'https://api.etrade.com/oauth/renew_access_token', 'https://api.etrade.com/oauth/revoke_access_token')

  # Make get request
  act_response = etrd_get_url(mng_url, access_tokens, etrade_cred)


  # Save access tokens
  if(sandbox) {
    options(etrade_acctok_sand = access_tokens)
  } else {
      options(etrade_acctok_prod = access_tokens)
    }

  act_response

}

