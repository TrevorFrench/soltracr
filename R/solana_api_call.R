#' solana_api_call
#'
#' @param url the RPC url for your API call
#' @param request_body the request body for your API call
#'
#' @return returns data from your solana API call
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "http://localhost:8899"
#' data <- solana_api_call(url)}

solana_api_call <- function(url, request_body) {
  headers <- c("Content-Type" = "application/json")

  response <- httr::POST(url, httr::add_headers(.headers = headers), body = request_body)

  if (httr::status_code(response) == 200) {
    return(httr::content(response))
  } else {
    print(httr::content(response, as = "text"))
    stop("Request failed with status code: ", httr::status_code(response))
  }
}

#' get_signature_for_address
#'
#' @param url the RPC url for your API call
#'
#' @return Returns signatures for confirmed transactions that include the given
#' address in their accountKeys list. Returns signatures backwards in time from
#' the provided signature or most recent confirmed block
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "http://localhost:8899"
#' data <- get_signature_for_address(url)}

get_signature_for_address <- function(url) {

  request_body <- '{
    "jsonrpc": "2.0",
    "id": null,
    "method": "getSignaturesForAddress",
    "params": ["7H3bquJXx8nhnYUDFcJixrkX8GkpgL8Sv4ZGP34M9gnx", {"limit": 1000}]
  }'

  solana_api_call(url, request_body)
}
