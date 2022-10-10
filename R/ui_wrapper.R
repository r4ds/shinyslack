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
                           site_url = NULL, #25
                           expiration = 90,
                           ...) {
  dots <- rlang::list2(...)
  parsed <- .parse_app_args(dots$options, site_url) #25
  dots$options <- NULL

  return(
    rlang::exec(
      shiny::shinyApp,
      ui = slack_shiny_ui(
        ui = ui,
        team_id = team_id,
        site_url = parsed$site_url, #25
        expiration = expiration
      ),
      server = server,
      options = parsed$options,
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
#' @return A list with elements `options` and `site_url`.
#' @keywords internal
.parse_app_args <- function(options, site_url) {
  options <- options %||% list()
  if (interactive()) {
    options$port <- options$port %||% 4242L
    site_url <- paste0("http://127.0.0.1:", options$port)
    options$launch.browser <- TRUE
  }

  if (is.null(site_url)) {
    rlang::abort(
      "You must supply a site_url to shinyslack_app in non-interactive mode.",
      class = "missing_site_url"
    )
  }
  return(
    list(
      options = options,
      site_url = site_url
    )
  )
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
slack_shiny_ui <- function(ui, team_id, site_url, expiration = 90) {
  # Case 1: They already have a cookie token.
  has_cookie_token <- scenes::set_scene(
    ui = ui,
    scenes::req_has_cookie(
      cookie_name = .slack_token_cookie_name(team_id),
      validation_fn = .validate_cookie_token,
      team_id = team_id
    )
  )

  # Case1b: No cookies. #5

  # Case 2: They are returning from the oauth endpoint, which has granted them
  # an authorization code.
  has_oauth_code <- scenes::set_scene(
    ui = .parse_auth_code(
      site_url = site_url, #25
      team_id = team_id,
      expiration = expiration
    ),
    scenes::req_has_query(key = "code")
  )

  # Case 3 (default): They have neither a token nor a code to exchange for a
  # token.
  needs_login <- scenes::set_scene(
    ui = .do_login(
      site_url = site_url, #25
      team_id = team_id
    )
  )

  return(
    scenes::change_scene(
      has_cookie_token,
      has_oauth_code,
      needs_login
    )
  )
}
