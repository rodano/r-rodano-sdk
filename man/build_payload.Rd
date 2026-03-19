\name{build_payload}
\alias{build_payload}
\title{Build dataset update payloads}
\usage{
build_payload(df, v = FALSE)
}
\arguments{
\item{df}{Data frame containing fields to update and a \code{DATASET_ID} column.}

\item{v}{If \code{TRUE}, enables progress bar display.}
}
\value{
A list of payload objects, one per dataset instance.
}
\description{
Builds request payloads for dataset update operations.
}
\examples{
# payload <- build_payload(df)
}
\seealso{
\code{\link{build_url}}, \code{\link{rodano-package}}
}
