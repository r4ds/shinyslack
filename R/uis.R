#' Convert a Slack Authorization Code to a Token
#'
#' @inheritParams .shared-parameters
#' @inheritParams set_cookie
#'
#' @return A function that takes a request and returns a [shiny::tagList()]that
#'   sets the cookie then reloads the site.
#' @keywords internal
.parse_auth_code <- function(site_url, team_id, expiration) {
  force(team_id)
  force(expiration)
  return(
    function(request) {
      # Do the call to the access url to exchange the code for a token, then
      # save that token in a cookie and redirect them to the base url of this
      # site.

      # Pass along query parameters from the request.
      site_url <- .update_site_url(site_url, request)

      # Exchange the code for a token.
      token <- slackteams::add_team_code(
        code = shiny::parseQueryString(request$QUERY_STRING)$code,
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
  )
}

#' Perform the Login Via Slack
#'
#' @inheritParams .shared-parameters
#'
#' @return A function that takes a request and returns a [shiny::tagList()]that
#'   sends the user to the proper Slack api URL in order to authenticate.
#' @keywords internal
.do_login <- function(site_url, team_id) {
  force(site_url)
  force(team_id)
  return(
    function(request) {
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
  )
}

.add_cookies <- function(ui) {
  force(ui)
  return(
    function(request) {
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
