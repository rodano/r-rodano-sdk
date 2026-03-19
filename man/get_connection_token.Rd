\name{get_connection_token}
\alias{get_connection_token}
\title{Retrieve a connection token}
\usage{
get_connection_token(urlBase, email = NULL, pwd = NULL)
}
\arguments{
\item{urlBase}{Base URL of the Rodano API.}

\item{email}{Optional login e-mail. If \code{NULL}, the user is prompted.}

\item{pwd}{Optional password. If \code{NULL}, the user is prompted securely.}
}
\value{
A character string containing the session token.
}
\description{
Authenticates against the Rodano API and returns a bearer token for
subsequent authenticated requests.
}
\examples{
# token <- get_connection_token("https://study.rodano.ch/api", "user@email.com", "password")
}
\seealso{
\code{\link{get_connection_robot}}, \code{\link{rodano-package}}
}
