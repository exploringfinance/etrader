#' Place an Equity Order
#'
#' Execute an equity order on the ETrade platform for the selected account.
#' Users are given the option to preview the order before submitting.
#'
#' @section Warning: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE SENT FOR
#'   EXECUTION. THIS FUNCTION HAS HUNDREDS OF POTENTIAL COMBINATIONS AND ONLY A
#'   HANDFUL HAVE BEEN TESTED. IT IS STRONGLY RECOMMENDED TO TEST THE DESIRED
#'   ORDER USING THE SANDBOX ENVIRONMENT. PLEASE NOTE THE SANDBOX ENVIRONMENT
#'   WILL NOT RETURN THE ORDER ENTERED, IT IS USED FOR CONFIRMING THE ORDER
#'   ENTRY HAS CORRECT SYNTAX. ORDERS CAN ALSO BE TESTED IN OFF MARKET HOURS ON
#'   A VERY SMALL QUANTITY WITH LITTLE MONEY AT STAKE. ANOTHER OPTION IS TO USE
#'   LIMIT ORDERS FAR FROM THE CURRENT PRICE. ETRADE HAS THEIR OWN ERROR
#'   HANDLING BUT IF A SUCCESSFUL COMBINATION IS ENTERED IT COULD BE EXECUTED
#'   UNINTENTIONALLY. DOUBLE CHECK ALL ENTRIES BEFORE SUBMITTING.
#'
#' @inheritParams etrd_account_balance
#' @param symbol The market symbol for the security being bought or sold
#' @param quantity The number of shares to buy or sell
#' @param orderAction The action that the broker is requested to perform: BUY,
#'   SELL, BUY_TO_COVER, SELL_SHORT
#' @param priceType The type of pricing: MARKET, LIMIT, STOP, STOP_LIMIT,
#'   TRAILING_STOP_CNST, TRAILING_STOP_PRCT, MARKET_ON_OPEN, MARKET_ON_CLOSE,
#'   LIMIT_ON_OPEN, LIMIT_ON_CLOSE
#' @param stopPrice The designated boundary price for a stop order. For trailing
#'   stop orders this will represent the dollar amount or percentage trailing
#'   value. Enter percentages as whole numbers.
#' @param limitPrice The highest price at which to buy or the lowest price at
#'   which to sell if specified in a limit order
#' @param stopLimitPrice 	The designated boundary price for a stop-limit order
#' @param quantityType The type of the quantity - QUANTITY, DOLLAR, ALL_I_OWN
#'   (note: DOLLAR is not yet an option)
#' @param orderTerm  The term for which the order is in effect:
#'   GOOD_UNTIL_CANCEL, GOOD_FOR_DAY, IMMEDIATE_OR_CANCEL, FILL_OR_KILL
#' @param marketSession The session in which the order will be placed: REGULAR,
#'   EXTENDED
#' @param allOrNone If TRUE, the transactions specified in the order must be
#'   executed all at once or not at all; default is FALSE
#' @param previewOrder Elect to preview the order before submitting.  Options
#'   are: 'df', 'json', or 'none' for an instant order entry. In a non
#'   interactive environment will default to 'none'. This also indicates how the
#'   order output is shown. json will return the output as a list, otherwise it
#'   will return as a dataframe
#'
#'
#' @return A list or data frame of the order entry details
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' # Place Equity Limit order
#' acts = etrd_account_list()
#' account = acts$accountIdKey[1]
#' etrd_place_eq_order(account = account,
#'                     symbol = 'PSLV',
#'                     quantity = 1,
#'                     orderAction = 'buy',
#'                     priceType = 'limit',
#'                     limitPrice = 8,
#'                     previewOrder = 'df')
#'
#' }
etrd_place_eq_order = function(account,
                               symbol,
                               quantity,
                               orderAction,
                               priceType,
                               stopPrice = '',
                               limitPrice = '',
                               stopLimitPrice = '',
                               quantityType = 'quantity',
                               orderTerm = 'good_for_day',
                               marketSession= 'regular',
                               allOrNone= 'false',
                               previewOrder = 'df',
                               etrade_cred = NULL,
                               access_tokens = NULL,
                               sandbox = FALSE) {

    # Verify passed credentials or get credentials from options
    etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
    access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

    # symbol = 'pslv'; quantity = 1; orderAction = 'buy'; priceType = 'market'; allOrNone= 'false'
    # quantityType = 'quantity'; orderTerm = 'good_for_day'; marketSession= 'regular'
    # stopPrice = ''; limitPrice = ''; stopLimitPrice = ''; sandbox = TRUE; previewOrder = 'df'
    clientOrdr = paste0('ordr',as.numeric(Sys.time()))

    # Collect order details
    orderList = list(
      PreviewOrderRequest = list(
        orderType = 'eq',
        clientOrderId = clientOrdr,
        Order = list(list(
          allOrNone = allOrNone,
          priceType = priceType,
          orderTerm = orderTerm,
          marketSession = marketSession,
          stopPrice = stopPrice,
          limitPrice = limitPrice,
          stopLimitPrice = stopLimitPrice,
          Instrument = list(list(
            Product = list(
              securityType = 'eq',
              symbol = symbol
            ),
            orderAction = orderAction,
            quantityType = quantityType,
            quantity = quantity
          ))
        ))
      )
    )

    hlp_etrd_place_order(account, stopPrice, orderList, clientOrdr, tolower(previewOrder), etrade_cred, access_tokens, sandbox)
}


#' Place a Mutual Fund Order
#'
#' Submit a mutual fund order on the ETrade platform for the selected account.
#' Users are given the option to preview the order before submitting. Mutual
#' fund orders must be received before 4pm or will be executed the following
#' day.
#'
#' @section Warning: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE SENT FOR
#'   EXECUTION. THIS FUNCTION HAS HUNDREDS OF POTENTIAL COMBINATIONS AND ONLY A
#'   HANDFUL HAVE BEEN TESTED. IT IS STRONGLY RECOMMENDED TO TEST THE DESIRED
#'   ORDER USING THE SANDBOX ENVIRONMENT. PLEASE NOTE THE SANDBOX ENVIRONMENT
#'   WILL NOT RETURN THE ORDER ENTERED, IT IS USED FOR CONFIRMING THE ORDER
#'   ENTRY HAS CORRECT SYNTAX. ORDERS CAN ALSO BE TESTED IN OFF MARKET HOURS ON
#'   A VERY SMALL QUANTITY WITH LITTLE MONEY AT STAKE. ETRADE HAS THEIR OWN
#'   ERROR HANDLING BUT IF A SUCCESSFUL COMBINATION IS ENTERED IT COULD BE
#'   EXECUTED UNINTENTIONALLY. DOUBLE CHECK ALL ENTRIES BEFORE SUBMITTING.
#'
#' @inheritParams etrd_place_eq_order
#' @param quantity The amount of the investment in either DOLLARS or
#'   SHARES depending on the input for quantityType
#' @param mfTransaction The transaction for the mutual fund order. Options:	BUY,
#'   SELL
#' @param reInvestOption Indicator flag to specify whether to reinvest profit on
#'   mutual funds. Options: REINVEST, DEPOSIT, CURRENT_HOLDING
#'
#' @return A list or data frame of the order entry details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Place Mutual Fund Buy order for $10
#' acts = etrd_account_list()
#' account = acts$accountIdKey[1]
#' etrd_place_mf_order(account = account,
#'                     symbol = 'SWTSX',
#'                     quantityType = 'DOLLAR',
#'                     quantity = 10,
#'                     mfTransaction = 'buy',
#'                     reInvestOption = 'reinvest',
#'                     previewOrder = 'df')
#'
#' }
etrd_place_mf_order = function(account,
                               symbol,
                               quantity,
                               mfTransaction,
                               reInvestOption = 'REINVEST',
                               quantityType = 'DOLLAR',
                               previewOrder = 'df',
                               etrade_cred = NULL,
                               access_tokens = NULL,
                               sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # reInvestOption = 'REINVEST'; investmentAmount = '10'; symbol = 'SWTSX'
  # mfTransaction = 'BUY'; sandbox = TRUE; previewOrder = 'df'
  clientOrdr = paste0('ordr',as.numeric(Sys.time()))

  # Collect order details
  orderList = list(
    PreviewOrderRequest = list(
      orderType = 'MF',
      clientOrderId = clientOrdr,
      Order = list(list(
        reInvestOption = reInvestOption,
        investmentAmount = quantity,
        Instrument = list(list(
          Product = list(
            securityType = 'MF',
            symbol = symbol
          ),
          mfQuantity = list(cash = quantity),
          orderAction = mfTransaction,
          mfTransaction = mfTransaction,
          quantityType = quantityType
        ))
      ))
    )
  )

  # Mutual Funds do not require stop prices
  stopPrice = ''
  hlp_etrd_place_order(account, stopPrice, orderList, clientOrdr, tolower(previewOrder), etrade_cred, access_tokens, sandbox)
}


#' Place an Option Order
#'
#' Execute an option order on the ETrade platform for the selected account.
#' Users are given the option to preview the order before submitting. Note:
#' ETrade offers significantly more complex order structures than what is
#' offered in this function. See the
#' \href{https://apisb.etrade.com/docs/api/order/api-order-v1.html#}{ETrade
#' documentation} for more details on submitting complex option strategies.
#'
#' @section Warning: TRADES THAT ARE SUCCESSFULLY ENTERED WILL BE SENT FOR
#'   EXECUTION. THIS FUNCTION HAS HUNDREDS OF POTENTIAL COMBINATIONS AND ONLY A
#'   HANDFUL HAVE BEEN TESTED. IT IS STRONGLY RECOMMENDED TO TEST THE DESIRED
#'   ORDER USING THE SANDBOX ENVIRONMENT. PLEASE NOTE THE SANDBOX ENVIRONMENT
#'   WILL NOT RETURN THE ORDER ENTERED, IT IS USED FOR CONFIRMING THE ORDER
#'   ENTRY HAS CORRECT SYNTAX. ORDERS CAN ALSO BE TESTED IN OFF MARKET HOURS ON
#'   A VERY SMALL QUANTITY WITH LITTLE MONEY AT STAKE. ANOTHER OPTION IS TO USE
#'   LIMIT ORDERS FAR FROM THE CURRENT PRICE. ETRADE HAS THEIR OWN ERROR
#'   HANDLING BUT IF A SUCCESSFUL COMBINATION IS ENTERED IT COULD BE EXECUTED
#'   UNINTENTIONALLY. DOUBLE CHECK ALL ENTRIES BEFORE SUBMITTING.
#'
#' @inheritParams etrd_place_eq_order
#' @param orderAction The action that the broker is requested to perform:
#'   BUY_OPEN, BUY_CLOSE, SELL_OPEN, SELL_CLOSE
#' @param callPut The option type:	CALL, PUT
#' @param expiryYear The four-digit year the option will expire
#' @param expiryMonth The month (1-12) the option will expire
#' @param expiryDay The day (1-31) the option will expire
#' @param strikePrice The strike price for the option
#'
#' @return A list or data frame of the order entry details
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # Place Option Buy_to_open order
#' acts = etrd_account_list()
#' account = acts$accountIdKey[1]
#' etrd_place_optn_order(account = account,
#'                       symbol = 'FB',
#'                       callPut = 'call',
#'                       expiryYear = '2018',
#'                       expiryMonth = '12',
#'                       expiryDay  = '21',
#'                       strikePrice = '140',
#'                       quantity = 1,
#'                       orderAction = 'BUY_OPEN',
#'                       priceType = 'market',
#'                       previewOrder = 'df')
#'
#' }
etrd_place_optn_order = function(account,
                                 symbol,
                                 callPut,
                                 expiryYear,
                                 expiryMonth,
                                 expiryDay,
                                 strikePrice,
                                 quantity,
                                 orderAction,
                                 priceType,
                                 stopPrice = '',
                                 limitPrice = '',
                                 stopLimitPrice = '',
                                 quantityType = 'quantity',
                                 orderTerm = 'good_for_day',
                                 marketSession= 'regular',
                                 allOrNone= 'false',
                                 previewOrder = 'df',
                                 etrade_cred = NULL,
                                 access_tokens = NULL,
                                 sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # symbol = 'pslv'; quantity = 1; orderAction = 'buy_open'; priceType = 'market'; allOrNone= 'false'
  # quantityType = 'quantity'; orderTerm = 'good_for_day'; marketSession= 'regular'
  # stopPrice = ''; limitPrice = ''; stopLimitPrice = ''; sandbox = TRUE; previewOrder = 'df'
  # callPut = 'CALL'; expiryYear = 2018; expiryMonth = 12; expiryDay = 21; strikePrice = 140

  clientOrdr = paste0('ordr',as.numeric(Sys.time()))

  # Collect order details
  orderList = list(
    PreviewOrderRequest = list(
      orderType = 'optn',
      clientOrderId = clientOrdr,
      Order = list(list(
        allOrNone = allOrNone,
        priceType = priceType,
        orderTerm = orderTerm,
        marketSession = marketSession,
        stopPrice = stopPrice,
        limitPrice = limitPrice,
        stopLimitPrice = stopLimitPrice,
        Instrument = list(list(
          Product = list(
            securityType = 'optn',
            symbol = symbol,
            callPut = callPut,
            expiryYear = expiryYear,
            expiryMonth = expiryMonth,
            expiryDay = expiryDay,
            strikePrice = strikePrice
          ),
          orderAction = orderAction,
          orderedQuantity = quantity,
          quantity = quantity
        ))
      ))
    )
  )



  hlp_etrd_place_order(account, stopPrice, orderList, clientOrdr, tolower(previewOrder), etrade_cred, access_tokens, sandbox)
}

#' Cancel an existing order
#'
#' Cancel an open order that has been submitted. Note: Verify the cancel request
#' was received and processed
#'
#' @inheritParams etrd_account_balance
#' @param orderId Order confirmation Id for the order placed.
#'
#' @return a response validating that the order has been canceled
#' @export
#'
#' @examples
#' \dontrun{
#' # Place Mutual Fund Buy order for $10
#' acts = etrd_account_list()
#' account = acts$accountIdKey[1]
#' mforder = etrd_place_mf_order(account = account,
#'                     symbol = 'SWTSX',
#'                     quantityType = 'DOLLAR',
#'                     investmentAmount = 10,
#'                     mfTransaction = 'buy',
#'                     reInvestOption = 'reinvest',
#'                     previewOrder = 'df')
#'
#' etrd_cancel_order(mforder$accountidKey, mforder$orderid.orderId)
#'
#' }
etrd_cancel_order = function(account,
                             orderId,
                             output = c('df','list'),
                             access_tokens = NULL,
                             etrade_cred = NULL,
                             sandbox = FALSE) {

  # Verify passed credentials or get credentials from options
  etrade_cred = hlp_etrd_check_cred(etrade_cred, sandbox)
  access_tokens = hlp_etrd_check_acctok(access_tokens, sandbox)

  # Generate signature
  sb = ifelse(sandbox,'sb','')
  cancelURL = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/orders/cancel')
  signature = httr::oauth_signature(
    url = cancelURL,
    method = 'PUT',
    app = etrade_cred,
    token = access_tokens$oauth_token,
    token_secret = access_tokens$oauth_token_secret
  )

  # pull account list
  cancel_req = httr::PUT(cancelURL,
                         hlp_etrd_auth_headers(signature),
                         body=list(CancelOrderRequest = list(orderId = orderId)),encode='json')

  # Check status
  hlp_etrd_status(cancel_req)
  cancel_req_resp = httr::content(cancel_req)

  # Return output
  cancel_req_resp

}





# ---------- Helper Trade Function 1 hlp_etrd_trd_status ----------------
# Check if trade function did not return 200 send back error code
hlp_etrd_trd_status = function(x,msg=NULL){

  # Check if status code is 200 or 201 (201 is specific to orders)
  SC = x$status_code
  if (SC!=200) {

    # Default to TD Error message and append custom if needed
    ErrMsg = httr::content(x)
    stop(paste0(SC,' - ',ErrMsg), call. = FALSE)
  }

}


# ---------- Helper Trade Function 2 hlp_etrd_place_order ----------------
# Place order collected from trade function
hlp_etrd_place_order = function(account,
                                stopPrice, # For trailing stop this must be explicitly passed
                                orderList,
                                clientOrdr,
                                previewOrder,
                                etrade_cred,
                                access_tokens,
                                sandbox) {

    # Create URL and generate signature
    sb = ifelse(sandbox,'sb','')
    ordrprevURL = paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/orders/preview.json')
    sign = httr::oauth_signature(
      url = ordrprevURL,
      method = 'POST',
      app = etrade_cred,
      token = access_tokens$oauth_token,
      token_secret = access_tokens$oauth_token_secret
    )

    # Post order for preview
    order_prev = httr::POST(ordrprevURL,hlp_etrd_auth_headers(sign),body=orderList,encode='json')

    # Check preview status and pull content
    hlp_etrd_trd_status(order_prev)
    ordrCon = httr::content(order_prev)


    # If interactive and preview requested, then show order and ask for confirmation
    if (previewOrder != 'none' & interactive()==TRUE) {
      # Display order details
      dfOut = data.frame(orderDetails = c(accountid = ordrCon$PreviewOrderResponse$accountId,
                                          unlist(ordrCon$PreviewOrderResponse$Order)))
      jsonOut = jsonlite::toJSON(ordrCon, pretty = TRUE, auto_unbox = TRUE)

      printOut = if(previewOrder == 'json') {jsonOut} else {dfOut}
      print(printOut)

      # Show order output as data frame
      # purrr::map_df(ordrCon, ~as.data.frame(.))

      # Ask for confirmation
      prevorder <- utils::menu(c('Yes','No'),
                           title = paste0('\nDo you want to place the order as shown above?'))

      # If user selects No or exits, do not place order

      ####****** Revisit with cancel order and indicate order is canceled
      if (prevorder %in% c(0,2)) stop('Order Not Placed', call. = FALSE)
    }


    # Ensure stop price matches original entry - necessary for trailing stops
    ordrCon$PreviewOrderResponse$Order[[1]]$stopPrice <- stopPrice

    # Create list of order to place using output
    placeList <- list(
      PlaceOrderRequest = list(
        orderType = ordrCon$PreviewOrderResponse$orderType,
        clientOrderId = clientOrdr,
        PreviewIds = ordrCon$PreviewOrderResponse$PreviewIds,
        Order = ordrCon$PreviewOrderResponse$Order
      )
    )

    # Generate URL and signature for placing order
    sb <- ifelse(sandbox,'sb','')
    ordrplaceURL <- paste0('https://api',sb,'.etrade.com/v1/accounts/',account,'/orders/place.json')
    sign <- httr::oauth_signature(
      url = ordrplaceURL,
      method = 'POST',
      app = etrade_cred,
      token = access_tokens$oauth_token,
      token_secret = access_tokens$oauth_token_secret
    )

    # Place Order
    order_place <- httr::POST(ordrplaceURL,hlp_etrd_auth_headers(sign),body=placeList,encode='json')

    # Verify order placed successfully
    hlp_etrd_trd_status(order_place)

    # Return order content
    orderList <- httr::content(order_place)

    orderDF <- data.frame(accountid = orderList$PlaceOrderResponse$accountId,
                       accountidKey = account,
                       orderid = orderList$PlaceOrderResponse$OrderIds[[1]],
                       t(unlist(orderList$PlaceOrderResponse$Order)))

    printOut <- if(previewOrder == 'json') {orderList} else {orderDF}
    printOut


}
