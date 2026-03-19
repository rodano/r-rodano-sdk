\name{get_extract}
\alias{get_extract}
\title{Retrieve a data extract}
\usage{
get_extract(
  urlBase,
  auth,
  expName,
  maxAttempts = 5,
  guessMax = 0,
  includeModifDate = FALSE,
  scopePk = NULL
)
}
\arguments{
\item{urlBase}{Base URL of the Rodano API.}

\item{auth}{Authentication object, usually from \code{create_authentication()}.}

\item{expName}{Dataset model ID (extract name) to retrieve.}

\item{maxAttempts}{Maximum number of retry attempts for failed requests.}

\item{guessMax}{Number of rows used for CSV type inference.}

\item{includeModifDate}{If \code{TRUE}, includes field modification dates.}

\item{scopePk}{Optional scope primary key used to filter extract rows.}
}
\value{
A data frame parsed from CSV extract content.
}
\description{
Calls the Rodano extract endpoint and returns the resulting CSV as a data frame.
}
\examples{
# auth <- create_authentication(list(Token = "..."))
# df <- get_extract("https://study.rodano.ch/api", auth, "MY_DATASET")
}
\seealso{
\code{\link{get_report}}, \code{\link{get_transfers}}
}
