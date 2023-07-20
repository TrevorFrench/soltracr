#' solana_api_call
#'
#' @param url the RPC url for your API call
#'
#' @return returns data from your solana API call
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "http://localhost:8899"
#' data <- solana_api_call(url)}

solana_api_call <- function(url) {
  headers <- c("Content-Type" = "application/json")

  request_body <- '{
    "jsonrpc": "2.0",
    "id": null,
    "method": "getSignaturesForAddress",
    "params": ["7H3bquJXx8nhnYUDFcJixrkX8GkpgL8Sv4ZGP34M9gnx", {"limit": 1000}]
  }'

  response <- httr::POST(url, httr::add_headers(.headers = headers), body = request_body)

  if (httr::status_code(response) == 200) {
    return(httr::content(response, as = "text"))
  } else {
    print(httr::content(response, as = "text"))
    stop("Request failed with status code: ", httr::status_code(response))
  }
}
