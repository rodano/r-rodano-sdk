\name{check_and_create_path}
\alias{check_and_create_path}
\title{Create a directory if it does not exist}
\usage{
check_and_create_path(dirPath)
}
\arguments{
\item{dirPath}{Character string giving the path of the directory to check.}
}
\value{
Called for its side effect. Creates the directory when needed.
}
\description{
Checks whether \code{dirPath} exists and creates it when it does not.
}
\examples{
tmp_dir <- file.path(tempdir(), "rodano-output")
check_and_create_path(tmp_dir)
dir.exists(tmp_dir)
}
\seealso{
\code{\link{rodano-package}}
}
