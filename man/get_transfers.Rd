\name{get_transfers}
\alias{get_transfers}
\title{Retrieve transfers export}
\usage{
get_transfers(urlBase, auth, maxAttempts = 5, scopeModelId = "PATIENT")
}
\arguments{
\item{urlBase}{Base URL of the Rodano API.}

\item{auth}{Authentication object, usually from \code{create_authentication()}.}

\item{maxAttempts}{Maximum number of retry attempts for failed requests.}

\item{scopeModelId}{Scope model ID used to retrieve transfers.}
}
\value{
A data frame parsed from CSV transfers content.
}
\description{
Retrieves the transfers report for the selected scope model from the Rodano API.
}
\examples{
# auth <- create_authentication(list(Token = "..."))
# trf <- get_transfers("https://study.rodano.ch/api", auth)
}
\seealso{
\code{\link{get_extract}}, \code{\link{get_report}}
}
