\name{get_connection_robot}
\alias{get_connection_robot}
\title{Retrieve a robot account}
\usage{
get_connection_robot(urlBase, token, role = NULL, autoLogout = FALSE)
}
\arguments{
\item{urlBase}{Base URL of the Rodano API.}

\item{token}{Bearer token of a user allowed to list robots.}

\item{role}{Optional role/profile ID used to filter robots.}

\item{autoLogout}{If \code{TRUE}, logs out the user session used to query robots.}
}
\value{
A robot object parsed from API JSON content.
}
\description{
Retrieves available robots from the Rodano API. If multiple robots match,
interactive selection is proposed.
}
\examples{
# token <- get_connection_token("https://study.rodano.ch/api", "user@email.com", "password")
# robot <- get_connection_robot("https://study.rodano.ch/api", token)
}
\seealso{
\code{\link{get_connection_token}}, \code{\link{rodano-package}}
}
