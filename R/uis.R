#' Convert a Slack Authorization Code to a Token
#'
#' @inheritParams .shared-parameters
#' @inheritParams scenes::set_cookie
#'
#' @return A function that takes a request and returns a [shiny::tagList()] that
#'   sets the cookie then reloads the site.
#' @keywords internal
.parse_auth_code <- function(site_url, team_id, expiration) {
  force(site_url) #25
  force(team_id)
  force(expiration)
  return(
    function(request) {
      # Pass along query parameters from the request when we come back.
      site_url <- .update_site_url(site_url, request) #25

      # Exchange the code for a token.
      token <- slackteams::add_team_code(
        code = shiny::parseQueryString(request$QUERY_STRING)$code,
        redirect_uri = site_url,
        verbose = FALSE
      )

      # Encrypt the token.
      token <- .shinyslack_encrypt(token)

      # Have the browser set the cookie then reload.
      return(
        shiny::tagList(
          scenes::set_cookie( #5
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
  force(site_url) #25
  force(team_id)
  return(
    function(request) {
      auth_url <- slackteams::auth_url(
        scopes = slackteams::load_scopes(which = "slackverse"), #17
        redirect_uri = .update_site_url(site_url, request), #25
        team_code = team_id #18
      )
      return(
        shiny::tagList( #15
          shiny::p(
            "Login via Slack to access this site."
          ),
          shiny::a(
            href = auth_url,
            style = .slack_button_style,
            shiny::HTML(.slack_logo_svg),
            "Sign in with Slack"
          ) #5
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
  # I think this is the place where #25 will actually be implemented.

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
