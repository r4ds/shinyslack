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
.validate_cookie_token <- function(cookie_token,
                                   team_id,
                                   shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  slack_token <- .shinyslack_decrypt(cookie_token, shinyslack_key)
  return(.validate_slack_token(slack_token, team_id))
}

.validate_slack_token <- function(slack_token, team_id) {
  if (is.null(slack_token) || slack_token == "bad_string") {
    return(FALSE)
  }
  auth_test <- slackcalls::post_slack(
    slack_method = "auth.test",
    token = slack_token
  )

  return(auth_test$ok && auth_test$team_id == team_id)
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
check_login <- function(team_id = get_shinyslack_team_id(),
                        session = shiny::getDefaultReactiveDomain(),
                        shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  return(
    shiny::reactive({
      slack_token <- .get_slack_cookie_token(team_id, shinyslack_key, session)
      token_is_valid <- .validate_slack_token(slack_token, team_id)
      if (token_is_valid) {
        session$userData$shinyslack_api_key <- slack_token
      }

      return(token_is_valid)
    })
  )
}

.get_slack_cookie_token <- function(team_id, shinyslack_key, session) {
  cookie_token <- cookies::get_cookie(
    .slack_token_cookie_name(team_id),
    session = session
  )
  return(.shinyslack_decrypt(cookie_token, shinyslack_key))
}

.update_shinyslack_api_key <- function(slack_api_key,
                                       team_id,
                                       session,
                                       shinyslack_key) {
  if (is.null(slack_api_key)) {
    slack_token <- .get_slack_cookie_token(team_id, shinyslack_key, session)
    if (.validate_slack_token(slack_token, team_id)) {
      session$userData$shinyslack_api_key <- slack_token
    }
  }
  return(session$userData$shinyslack_api_key)
}
