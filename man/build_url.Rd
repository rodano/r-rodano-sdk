\name{build_url}
\alias{build_url}
\title{Build dataset update URLs}
\usage{
build_url(df, urlBase, v = FALSE, scopeModelId = "PATIENT")
}
\arguments{
\item{df}{Data frame containing required IDs for each update row.}

\item{urlBase}{Base URL of the Rodano API.}

\item{v}{If \code{TRUE}, enables progress bar display.}

\item{scopeModelId}{Scope model ID used to resolve scope identifier column names.}
}
\value{
A character vector containing one URL per row of \code{df}.
}
\description{
Constructs dataset update endpoints used for bulk PUT operations.
}
\examples{
# urls <- build_url(df, "https://study.rodano.ch/api")
}
\seealso{
\code{\link{build_payload}}, \code{\link{rodano-package}}
}
