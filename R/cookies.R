#' The Name of the Slack Token Cookie
#' @inheritParams .shared-parameters
#' @return The name of the Slack token cookie
#' @keywords internal
.slack_token_cookie_name <- function(team_id) {
  return(
    paste(team_id, "slack_token", sep = "_")
  )
}

#' Add the js-cookie Javascript Library
#'
#' We only use the `set` method from this library for easier cookie handling,
#' but we make it easy to add the library in case you wish to manipulate
#' cookies. We also load the cookie object into the input.
#'
#' @return An html_dependency, which Shiny uses to add the js-cookie Javascript
#'   library exactly once.
#' @export
include_cookies <- function() {
  return(
    htmltools::htmlDependency(
      name = "shinyslack",
      version = "1.0.0",
      src = "www",
      package = "shinyslack",
      script = c("js.cookie.js", "cookie_input.js")
    )
  )
}

#' Shiny Tags to Add Cookies
#'
#' This function generates javascript which will set a cookie in the user's
#' browser.
#'
#' @param contents The contents of the cookie. Right now this should be a single
#'   character value.
#' @param cookie_name A name for the cookie. Must be a valid cookie name.
#' @param expiration Days after which the cookie should expire.
#'
#' @return A [shiny::tagList()] that provides the HTML and javascript to set the
#'   cookie.
#' @export
set_cookie <- function(contents, cookie_name, expiration = 90) {
  return(
    shiny::tagList(
      include_cookies(),
      shiny::tags$script(
        shiny::HTML(
          sprintf(
            "Cookies.set('%s', '%s', { expires: %i });",
            cookie_name,
            contents,
            expiration
          )
        )
      )
    )
  )
}

#' Make Sure a Cookie Token Works
#'
#' @inheritParams .shared-parameters
#' @param cookie_token A character with the code used to authenticate the user.
#'
#' @return A logical indicating whether the token works for testing
#'   authentication for this team.
#' @keywords internal
.validate_cookie_token <- function(cookie_token, team_id) {
  cookie_token <- .shinyslack_decrypt(cookie_token)

  Sys.setenv(
    SLACK_API_TOKEN = cookie_token
  )
  auth_test <- slackcalls::post_slack(
    slack_method = "auth.test"
  )

  auth_test$ok && auth_test$team_id == team_id
}

#' Check Slack Login
#'
#' Confirm that a user is logged into Slack.
#'
#' @inheritParams .shared-parameters
#'
#' @return A [shiny::reactive()] which returns a logical indicating whether the
#'   user is logged in with proper API access.
#' @export
check_login <- function(input, team_id) {
  return(
    shiny::reactive({
      slack_cookie <- input$cookies[[.slack_token_cookie_name(team_id)]]
      return(
        !is.null(slack_cookie) && .validate_cookie_token(
          cookie_token = slack_cookie,
          team_id = team_id
        )
      )
    })
  )
}
