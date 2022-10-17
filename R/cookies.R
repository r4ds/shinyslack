#' The Name of the Slack Token Cookie
#' @inheritParams .shared-parameters
#' @return The name of the Slack token cookie
#' @keywords internal
.slack_token_cookie_name <- function(team_id) {
  return(
    paste(team_id, "slack_token", sep = "_")
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
#' @param session The shiny session object. In most situations you should use
#'   the default.
#'
#' @return A [shiny::reactive()] which returns a logical indicating whether the
#'   user is logged in with proper API access.
#' @export
check_login <- function(team_id, session = shiny::getDefaultReactiveDomain()) {
  return(
    shiny::reactive({
      slack_cookie <- cookies::extract_cookie(
        session$request,
        .slack_token_cookie_name(team_id)
      )

      return(
        !is.null(slack_cookie) && .validate_cookie_token(
          cookie_token = slack_cookie,
          team_id = team_id
        )
      )
    })
  )
}
