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

#' assemble_key_pair
#'
#' @param key the key for your key pair
#' @param pair the pair for your key pair
#'
#' @return Returns your key pair if it exists or a blank string if it doesn't
#' exist
#' @export
#'
#' @examples
#' \dontrun{
#' limit <- NULL
#' limit <- assemble_key_pair('limit', limit)}

assemble_key_pair <- function(key, pair) {
  if(is.null(pair)){
    keypair = ''
  } else {
    keypair = paste('"',key,'":',pair, sep = '')
  }
  return(keypair)
}

#' assemble_config_object
#'
#' @param character_vector the character vector used to create the config object
#' @return Returns your config object
#' @export
#'
#' @examples
#' \dontrun{
#' limit <- NULL
#' limit <- assemble_key_pair('limit', limit)}

assemble_list <- function(character_vector) {
  character_vector <- character_vector[character_vector != '']
  character_vector <- paste(character_vector, collapse = ',')
  return(character_vector)
}

#' assemble_request_body
#'
#' @param jsonrpc the jsonrpc for your request body
#' @param id the id for your request body
#' @param method the method for your request body
#' @param params the parameters for your request body
#'
#' @return Returns the request body for your solana API call
#' @export
#'
#' @examples
#' \dontrun{
#' limit <- assemble_key_pair('limit', limit)
#' params <- paste('["',address,'", {',limit,'}]', sep = '')
#' request_body <- assemble_request_body('2.0', 'null', 'getSignaturesForAddress', params)}

assemble_request_body <- function(jsonrpc, id, method, params) {
  jsonrpc <- assemble_key_pair('jsonrpc', jsonrpc)
  id <- assemble_key_pair('id', id)
  method <- assemble_key_pair('method', method)
  params <- assemble_key_pair('params', params)
  character_vector <- c(jsonrpc, id, method, params)
  body <- assemble_list(character_vector)
  request_body <- paste('{',body,'}', sep = '')
  return(request_body)
}

#' get_signature_for_address
#'
#' @param url the RPC url for your API call
#' @param address the address for which you're retrieving signatures
#' @param limit maximum transaction signatures to return (between 1 and 1,000).
#' Default is 1,000.
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

get_signature_for_address <- function(url, address, limit = NULL) {
  limit <- assemble_key_pair('limit', limit)
  character_vector <- c(limit)
  config_object <- assemble_list(character_vector)
  params <- paste('["',address,'", {',config_object,'}]', sep = '')
  request_body <- assemble_request_body('"2.0"', 'null', '"getSignaturesForAddress"', params)
  solana_api_call(url, request_body)
}

#' get_account_info
#'
#' @param url the RPC url for your API call
#' @param address the address for which you're retrieving signatures
#'
#' @return Returns all information associated with the account of provided Pubkey
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "http://localhost:8899"
#' data <- get_signature_for_address(url)}

get_account_info <- function(url, address) {
  params <- paste('["',address,'"]', sep = '')
  request_body <- assemble_request_body('"2.0"', 'null', '"getAccountInfo"', params)
  solana_api_call(url, request_body)
}
