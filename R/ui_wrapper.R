#' Launch a Shiny App with a Slack Login
#'
#' @inheritParams slack_shiny_ui
#' @inheritParams shiny::shinyApp
#' @param ... Additional parameters passed on to \code{\link[shiny]{shinyApp}}.
#' @importFrom rlang %||%
#'
#' @return An object that represents the app. See \code{\link[shiny]{shinyApp}}
#'   for details.
#' @export
shinyslack_app <- function(ui,
                           server,
                           team_id,
                           site_url = NULL,
                           expiration = 90,
                           ...) {
  dots <- rlang::list2(...)
  options <- dots$options %||% list()
  dots$options <- NULL
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
    rlang::exec(
      shiny::shinyApp,
      ui = slack_shiny_ui(
        ui = ui,
        team_id = team_id,
        site_url = site_url,
        expiration = expiration
      ),
      server = server,
      options = options,
      !!!dots
    )
  )
}


#' Require Slack login to a Shiny app
#'
#'
#' This is a function factory that wraps a Shiny ui. If the user does not have a
#' cookie for that site, they are prompted to login. Once they have a cookie,
#' the UI displays as normal.
#'
#' @inheritParams .shared-parameters
#' @inheritParams .parse_auth_code
#' @param expiration Days after which the user's login cookie should expire.
#'
#' @return A function defining the UI of a Shiny app (either with login or
#'   without).
#' @export
slack_shiny_ui <- function(ui, team_id, site_url, expiration = 90) {
  # Case 1: They already have a cookie token. In this case we return the ui.
  # Note: This means .validate_cookie_token has already parsed their token and
  # set it as the SLACK_API_TOKEN environment variable used by slackcalls, so no
  # further validation is needed by apps that wish to use Slack.
  has_cookie_token <- scenes::set_scene(
    ui = .add_cookies(ui), # Should this happen in {scenes}?
    scenes::req_has_cookie(
      cookie_name = .slack_token_cookie_name(team_id),
      validation_fn = .validate_cookie_token,
      team_id = team_id
    )
  )

  # TODO: In theory we should probably allow tokens via URL? I fake had it in
  # here before but I don't validate it and don't have a specific usecase yet.

  # Case 2: They are returning from the oauth endpoint, which has granted them
  # authorization. The url will now have a `code` parameter. If someone puts in
  # a fake one they'll still get this parser UI, but it will fail and tell them
  # why.
  has_oauth_code <- scenes::set_scene(
    ui = .parse_auth_code(
      site_url = site_url,
      team_id = team_id,
      expiration = expiration
    ),
    scenes::req_has_query(key = "code")
  )

  # Case 3 (default): They have neither a token nor a code to exchange for a
  # token.
  needs_login <- scenes::set_scene(
    ui = .do_login(
      site_url = site_url,
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
