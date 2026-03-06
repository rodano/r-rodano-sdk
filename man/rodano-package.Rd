\name{rodano-package}
\alias{rodano-package}
\alias{rodano}
\docType{package}
\title{
  R SDK for Rodano API
}
\description{
  This package provides an R SDK for interacting with the Rodano API. 
  It includes helper functions, communication utilities, and test scripts 
  to facilitate integration with the Rodano platform.
}
\details{
  The package includes three main feature areas:
  
  \itemize{
    \item Data Export: Pull and export data from the Rodano platform
    \item Data Management: Write or remove data using the Rodano fine-grained rights API
    \item Workflow Management: Manage and automate workflows within the Rodano environment
  }
  
  The SDK provides secure authentication methods to connect to the Rodano platform,
  supporting user login, session management, and token-based authentication.
  
  Key functions include:
  \itemize{
    \item \code{get_connection_token()}: Retrieve authentication token
    \item \code{get_connection_robot()}: Get robot connection
    \item \code{get_extract()}: Pull data extracts
    \item \code{get_report()}: Retrieve reports
    \item \code{build_url()}: Build API URLs
    \item \code{build_payload()}: Build request payloads
  }
}
\author{
  Rodano Team <info@rodano.ch>
}
\references{
  \url{https://github.com/rodano/r-rodano-sdk}
}
\keyword{ package }
\seealso{
  \code{\link{get_connection_token}}, \code{\link{get_extract}}
}
\examples{
  # Connect to Rodano API
  # token <- get_connection_token("https://study.rodano.ch/api", "user@email.com", "password")
  
  # Get configuration
  # auth <- create_authentication(list(Token = token))
  # config <- get_config("https://study.rodano.ch/api", auth)
}
