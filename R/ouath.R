#' Require Slack login to a Shiny app
#'
#' This function is intended to wrap a Shiny ui. If the user does not have a
#' cookie for that site, they are prompted to login. Once they have a cookie,
#' the UI displays as normal.
#'
#' @inheritParams .parse_ui
#' @inheritParams .parse_auth_code
#'
#' @return A function defining the UI of a Shiny app (either with login or
#'   without).
#' @export
slack_shiny_ui <- function(ui, team_id, site_url) {
  force(ui)
  function(request) {
    if (.has_token(request, team_id)) {
      # Case 1: They already have a token. In this case we return the actual ui.
      return(.parse_ui(ui, request))
    } else if (.has_auth_code(request)) {
      # Case 2: They are returning from the oauth endpoint, which has granted
      # them authorization. The url will now have a `code` parameter.
      return(
        .parse_auth_code(
          request = request,
          site_url = site_url,
          team_id = team_id
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
  # with those. We need to add the cookie handler to that UI.
  if (is.function(ui)) {
    if (length(formals(ui))) {
      ui <- ui(request)
    } else {
      ui <- ui()
    }
  }
  return(
    shiny::tagList(
      shinycookie::initShinyCookie("shinycookie"),
      ui
    )
  )
}

#' Convert a Slack Authorization Code to a Token
#'
#' @inheritParams .shared-parameters
#'
#' @return A \code{\link[shiny]{tagList}} that sets the cookie then reloads the
#'   site.
#' @keywords internal
.parse_auth_code <- function(request, site_url, team_id) {
  # Do the call to the access url to exchange the code for a token, then
  # save that token in a cookie and redirect them to the base url of this
  # site. If they don't allow you to save a cookie, put the token in the url
  # as a parameter. 100% of this should occur in javascript. Show a GDPR
  # thing on this screen, just use the oauth_token parameter in the URL if
  # they don't accept.
  site_url <- .update_site_url(site_url, request)
  token <- slackteams::add_team_code(
    code = .extract_auth_code(request),
    redirect_uri = site_url,
    verbose = FALSE
  )
  return(
    shiny::tagList(
      # Set up javascript for handling cookies.
      shinycookie::initShinyCookie("shinycookie"),
      # Add the code to add the cookie.
      set_cookie(
        contents = token,
        cookie_name = .slack_token_cookie_name(team_id)
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
  # To do (probably places other than here): Deal with state. That makes things
  # more secure, but I need to wrap my head around it.

  # We also need to sort out cookie stuff for EU here, ideally.
  auth_url <- slackteams::auth_url(
    scopes = slackteams::load_scopes(which = "slackverse"),
    redirect_uri = .update_site_url(site_url, request),
    team_code = team_id
  )
  return(
    shiny::tags$script(
      shiny::HTML(
        sprintf("location.replace(\"%s\");", auth_url)
      )
    )
  )
}

#' Confirm that the User is Logged In
#'
#' @inheritParams .shared-parameters
#'
#' @return A \code{\link[shiny]{reactive}} which returns a logical indicating
#'   whether the user is logged in with proper API access.
#' @export
check_login <- function(input, team_id) {
  shiny::reactive({
    Sys.setenv(
      SLACK_API_TOKEN = input$shinycookie[[.slack_token_cookie_name(team_id)]]
    )
    auth_test <- slackcalls::post_slack(
      slack_method = "auth.test"
    )
    auth_test$ok && auth_test$team_id == team_id
  })
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
