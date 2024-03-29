% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/uis.R
\name{.parse_auth_code}
\alias{.parse_auth_code}
\title{Convert a Slack Authorization Code to a Token}
\usage{
.parse_auth_code(
  team_id,
  expiration,
  shinyslack_key = Sys.getenv("SHINYSLACK_KEY")
)
}
\arguments{
\item{team_id}{The Slack team ID through which the user is being
authenticated.}

\item{expiration}{Days after which the cookie should expire. To remove an
HttpOnly cookie, send a negative value for this attribute.}

\item{shinyslack_key}{(optional) A key to use to encrypt the string. If not
set, the string is returned unencrypted.}
}
\value{
A ui function that takes a request and returns a \code{\link[shiny:reexports]{shiny::tagList()}}
that sets the cookie then reloads the site.
}
\description{
Convert a Slack Authorization Code to a Token
}
\keyword{internal}
