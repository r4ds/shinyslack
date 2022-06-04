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
#' @inheritParams .parse_ui
#' @inheritParams .parse_auth_code
#' @param expiration Days after which the user's login cookie should expire.
#'
#' @return A function defining the UI of a Shiny app (either with login or
#'   without).
#' @export
slack_shiny_ui <- function(ui, team_id, site_url, expiration = 90) {
  return(
    function(request) {
      if (.has_token(request, team_id)) {
        # Case 1: They already have a token. In this case we return the ui.
        # Note: .has_token has already parsed their token and set it as the
        # SLACK_API_TOKEN environment variable used by slackcalls, so no further
        # validation is needed by apps that wish to use Slack.
        return(.parse_ui(ui, request))
      } else if (.has_auth_code(request)) {
        # Case 2: They are returning from the oauth endpoint, which has granted
        # them authorization. The url will now have a `code` parameter.
        return(
          .parse_auth_code(
            request = request,
            site_url = site_url,
            team_id = team_id,
            expiration = expiration
          )
        )
      } else {
        # Case 3: They have neither a token nor a code to exchange for a token.
        return(
          .do_login(
            request = request,
            site_url = site_url,
            team_id = team_id
          )
        )
      }
    }
  )
}

#' Prepare a Shiny UI for Display
#'
#' @inheritParams .shared-parameters
#' @param ui A function defining the UI of a Shiny app, or a
#'   \code{\link[shiny]{tagList}}.
#'
#' @return The parsed ui, as a \code{\link[shiny]{tagList}}.
#' @keywords internal
.parse_ui <- function(ui, request) {
  # ui can be a tagList, a 0-argument function, or a 1-argument function. Deal
  # with those.
  if (is.function(ui)) {
    if (length(formals(ui))) {
      ui <- ui(request)
    } else {
      ui <- ui()
    }
  }
  return(
    shiny::tagList(
      include_cookies(),
      ui
    )
  )
}

#' Convert a Slack Authorization Code to a Token
#'
#' @inheritParams .shared-parameters
#' @inheritParams set_cookie
#'
#' @return A \code{\link[shiny]{tagList}} that sets the cookie then reloads the
#'   site.
#' @keywords internal
.parse_auth_code <- function(request, site_url, team_id, expiration) {
  # Do the call to the access url to exchange the code for a token, then
  # save that token in a cookie and redirect them to the base url of this
  # site.

  # Pass along query parameters from the request.
  site_url <- .update_site_url(site_url, request)

  # Exchange the code for a token.
  token <- slackteams::add_team_code(
    code = .extract_auth_code(request),
    redirect_uri = site_url,
    verbose = FALSE
  )

  # Encrypt the token before saving it in a cookie.
  token <- .shinyslack_encrypt(token)

  # Have the browser set the cookie then reload.
  return(
    shiny::tagList(
      set_cookie(
        contents = token,
        cookie_name = .slack_token_cookie_name(team_id),
        expiration = expiration
      ),
      # Reload the page to re-process the request.
      shiny::tags$script(
        shiny::HTML(
          sprintf("location.replace('%s');", site_url)
        )
      )
    )
  )
}

#' Perform the Login Via Slack
#'
#' @inheritParams .shared-parameters
#'
#' @return A \code{\link[shiny]{tagList}} that sends the user to the proper
#'   Slack api URL in order to authenticate.
#' @keywords internal
.do_login <- function(request, site_url, team_id) {
  auth_url <- slackteams::auth_url(
    scopes = slackteams::load_scopes(which = "slackverse"),
    redirect_uri = .update_site_url(site_url, request),
    team_code = team_id
  )
  return(
    shiny::tagList(
      shiny::p(
        "Login via Slack to access this site."
      ),
      shiny::a(
        href = auth_url,
        style = .slack_button_style,
        shiny::HTML(.slack_logo_svg),
        "Sign in with Slack"
      )
    )
  )
}

#' Keep Url Bits
#'
#' @inheritParams .shared-parameters
#'
#' @return The site_url with query parameters included.
#' @keywords internal
.update_site_url <- function(site_url, request) {
  # First extract any query parameters that are there other than the code.
  query_list <- shiny::parseQueryString(request$QUERY_STRING)
  query_list$code <- NULL
  # For the moment I'm explicitly setting blank things to NULL, but that might
  # change.
  query_list[query_list == ""] <- NULL
  url_list <- httr::parse_url(site_url)
  url_list$query <- c(url_list$query, query_list)
  return(httr::build_url(url_list))
}
