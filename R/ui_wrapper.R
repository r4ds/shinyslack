#' Launch a Shiny App with a Slack Login
#'
#' Launch a [shiny::shinyApp()] with an integrated Slack login.
#' @inheritParams slack_shiny_ui
#' @inheritParams shiny::shinyApp
#' @param ... Additional parameters passed on to [shiny::shinyApp()].
#'
#' @return An object that represents the app. See [shiny::shinyApp()] for
#'   details.
#' @export
shinyslack_app <- function(ui,
                           server,
                           team_id,
                           ...,
                           expiration = 90,
                           shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  dots <- rlang::list2(...)
  dots$options <- .parse_app_args(dots$options)

  return(
    rlang::exec(
      shiny::shinyApp,
      ui = slack_shiny_ui(ui, team_id, expiration, shinyslack_key),
      server = server,
      !!!dots
    )
  )
}

#' Figure Out Args for shinyslack_app
#'
#' @inheritParams shinyslack_app
#' @inheritParams shiny::shinyApp
#' @importFrom rlang %||%
#'
#' @return The parsed `options`.
#' @keywords internal
.parse_app_args <- function(options) {
  options <- options %||% list()
  if (interactive()) {
    options$port <- options$port %||% 4242L
    options$launch.browser <- TRUE
  }

  return(options)
}

#' Require Slack login to a Shiny app
#'
#' This is a function factory that wraps a Shiny ui. If the user does not have a
#' cookie for that site, they are prompted to login. Once they have a cookie,
#' the UI displays as normal. #5
#'
#' @inheritParams .shared-parameters
#' @inheritParams .parse_auth_code
#'
#' @return A function defining the UI of a Shiny app (either with login or
#'   without).
#' @export
slack_shiny_ui <- function(ui,
                           team_id,
                           expiration = 90,
                           shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  has_cookie_token <- .ui_has_cookie_token(ui, team_id, shinyslack_key)
  has_oauth_code <- .ui_has_oauth_code(team_id, expiration, shinyslack_key)
  needs_login <- scenes::set_scene(ui = .do_login(team_id))

  return(scenes::change_scene(has_cookie_token, has_oauth_code, needs_login))
}

.ui_has_cookie_token <- function(ui, team_id, shinyslack_key) {
  return(
    scenes::set_scene(
      ui = ui,
      scenes::req_has_cookie(
        cookie_name = .slack_token_cookie_name(team_id),
        validation_fn = .validate_cookie_token,
        team_id = team_id,
        shinyslack_key = shinyslack_key
      )
    )
  )
}

.ui_has_oauth_code <- function(team_id, expiration, shinyslack_key) {
  return(
    scenes::set_scene(
      ui = .parse_auth_code(team_id, expiration, shinyslack_key),
      scenes::req_has_query(key = "code")
    )
  )
}
