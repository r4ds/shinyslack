% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/encrypt.R
\name{.shinyslack_encrypt}
\alias{.shinyslack_encrypt}
\title{Encrypt a String If Possible}
\usage{
.shinyslack_encrypt(string, shinyslack_key = Sys.getenv("SHINYSLACK_KEY"))
}
\arguments{
\item{string}{A length-1 character to encrypt (or decrypt).}

\item{shinyslack_key}{(optional) A key to use to encrypt the string. If not
set, the string is returned unencrypted.}
}
\value{
If \code{shinyslack_key} is non-empty, the encrypted string. Otherwise the
original string is returned.
}
\description{
Encrypt a String If Possible
}
\keyword{internal}
