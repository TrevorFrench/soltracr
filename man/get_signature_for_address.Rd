% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/solana_api_call.R
\name{get_signature_for_address}
\alias{get_signature_for_address}
\title{get_signature_for_address}
\usage{
get_signature_for_address(url, address, limit = NULL)
}
\arguments{
\item{url}{the RPC url for your API call}

\item{address}{the address for which you're retrieving signatures}

\item{limit}{maximum transaction signatures to return (between 1 and 1,000).
Default is 1,000.}
}
\value{
Returns signatures for confirmed transactions that include the given
address in their accountKeys list. Returns signatures backwards in time from
the provided signature or most recent confirmed block
}
\description{
get_signature_for_address
}
\examples{
\dontrun{
url <- "http://localhost:8899"
data <- get_signature_for_address(url)}
}
