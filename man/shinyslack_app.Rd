% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wrapper.R
\name{shinyslack_app}
\alias{shinyslack_app}
\title{Launch a Shiny App with a Slack Login}
\usage{
shinyslack_app(
  ui,
  server,
  team_id,
  ...,
  expiration = 90,
  shinyslack_key = Sys.getenv("SHINYSLACK_KEY")
)
}
\arguments{
\item{ui}{A 0- or 1-argument function defining the UI of a Shiny app, or a
\code{\link[shiny:reexports]{shiny::tagList()}}.}

\item{server}{A function with three parameters: \code{input}, \code{output}, and
\code{session}. The function is called once for each session ensuring that each
app is independent.}

\item{team_id}{The Slack team ID through which the user is being
authenticated.}

\item{...}{Additional parameters passed on to \code{\link[shiny:shinyApp]{shiny::shinyApp()}}.}

\item{expiration}{Days after which the cookie should expire. To remove an
HttpOnly cookie, send a negative value for this attribute.}

\item{shinyslack_key}{(optional) A key to use to encrypt the string. If not
set, the string is returned unencrypted.}
}
\value{
An object that represents the app. See \code{\link[shiny:shinyApp]{shiny::shinyApp()}} for
details.
}
\description{
Launch a \code{\link[shiny:shinyApp]{shiny::shinyApp()}} with an integrated Slack login.
}
