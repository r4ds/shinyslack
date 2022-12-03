#' Convert a Slack Authorization Code to a Token
#'
#' @inheritParams .shared-parameters
#' @inheritParams cookies::set_cookie_response
#'
#' @return A ui function that takes a request and returns a [shiny::tagList()]
#'   that sets the cookie then reloads the site.
#' @keywords internal
.parse_auth_code <- function(team_id, expiration) {
  force(team_id)
  force(expiration)
  return(
    function(request) {
      # Pass along query parameters from the request when we come back.
      redirect_url <- .extract_full_url(request)

      # Exchange the code for a token.
      token <- slackteams::add_team_code(
        code = shiny::parseQueryString(request$QUERY_STRING)$code,
        redirect_uri = redirect_url,
        verbose = FALSE
      )

      # Encrypt the token.
      token <- .shinyslack_encrypt(token)

      # Have the browser set the cookie then load the updated url.
      return(
        cookies::set_cookie_response(
          cookie_name = .slack_token_cookie_name(team_id),
          cookie_value = token,
          expiration = expiration,
          secure_only = TRUE,
          http_only = TRUE,
          redirect = redirect_url
        )
      )
    }
  )
}

#' Perform the Login Via Slack
#'
#' @inheritParams .shared-parameters
#'
#' @return A ui function that takes a request and returns a [shiny::tagList()]
#'   that sends the user to the proper Slack api URL in order to authenticate.
#' @keywords internal
.do_login <- function(team_id) {
  force(team_id)
  return(
    function(request) {
      auth_url <- slackteams::auth_url(
        scopes = slackteams::load_scopes(which = "slackverse"), #17
        redirect_uri = .extract_full_url(request),
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
#' @return The url with query parameters included.
#' @keywords internal
.extract_full_url <- function(request) {
  url <- .extract_server_url(request)

  # First extract any query parameters that are there other than the code.
  query_list <- shiny::parseQueryString(request$QUERY_STRING)
  query_list$code <- NULL
  # For the moment I'm explicitly setting blank things to NULL, but that might
  # change.
  query_list[query_list == ""] <- NULL
  url_list <- httr::parse_url(url)
  url_list$query <- c(url_list$query, query_list)
  return(httr::build_url(url_list))
}

#' Extract the server URL from the request
#'
#' @param request A request object.
#'
#' @return A character with the url.
#' @keywords internal
.extract_server_url <- function(request) {
  this_url <- request$`X-REDX-FRONTEND-NAME` %||%
    request$`x-redx-frontend-name` %||%
    request$SERVER_NAME %||%
    request$server_name

  if (is.null(this_url)) {
    cli::cli_abort(
      message = c(x = "Could not determine url.")
    )
  }

  return(this_url)
}
