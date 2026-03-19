\name{get_report}
\alias{get_report}
\title{Retrieve a workflow summary report}
\usage{
get_report(
  urlBase,
  auth,
  repName,
  maxAttempts = 5,
  guessMax = 0,
  withHistory = FALSE
)
}
\arguments{
\item{urlBase}{Base URL of the Rodano API.}

\item{auth}{Authentication object, usually from \code{create_authentication()}.}

\item{repName}{Report name.}

\item{maxAttempts}{Maximum number of retry attempts for failed requests.}

\item{guessMax}{Number of rows used for CSV type inference.}

\item{withHistory}{If \code{TRUE}, retrieves the history export endpoint.}
}
\value{
A data frame parsed from CSV report content.
}
\description{
Retrieves workflow summary report data from the Rodano API.
}
\examples{
# auth <- create_authentication(list(Token = "..."))
# rep <- get_report("https://study.rodano.ch/api", auth, "my_report")
}
\seealso{
\code{\link{get_extract}}, \code{\link{get_transfers}}
}
