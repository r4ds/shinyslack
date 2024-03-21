#' Convert a Slack Authorization Code to a Token
#'
#' @inheritParams .shared-parameters
#' @inheritParams cookies::set_cookie_response
#'
#' @return A ui function that takes a request and returns a [shiny::tagList()]
#'   that sets the cookie then reloads the site.
#' @keywords internal
.parse_auth_code <- function(team_id,
                             expiration,
                             shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  force(team_id)
  force(expiration)
  return(
    function(request) {
      redirect_url <- .extract_full_url(request)
      token <- .get_encrypted_token(request, redirect_url, shinyslack_key)
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

.get_encrypted_token <- function(request, redirect_url, shinyslack_key) {
  token <- slackteams::add_team_code(
    code = shiny::parseQueryString(request$QUERY_STRING)$code,
    redirect_uri = redirect_url,
    verbose = FALSE
  )
  token <- .shinyslack_encrypt(token, shinyslack_key)
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
          shiny::p("Login via Slack to access this site."),
          shiny::p(
            paste(
              "Note: If you are logged into multiple slack teams, you may be",
              "initially presented with the wrong team on the next page.",
              "Select the proper team in the drop-down at the top-right of",
              "that page if a drop-down is present."
            )
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
  if (any(
    c("x-redx-frontend-name", "http_x_redx_frontend_name")
    %in% tolower(names(request))
  )) {
    url <- request$HTTP_X_REDX_FRONTEND_NAME %||%
      request$http_x_redx_frontend_name %||%
      request$`X-REDX-FRONTEND-NAME` %||%
      request$`x-redx-frontend-name`

    scheme <- request$HTTP_X_FORWARDED_PROTO %||%
      request$http_x_forwarded_proto %||%
      request$`X-FORWARDED-PROTO` %||%
      request$`x-forwarded-proto`
  } else {
    url <- request$SERVER_NAME %||% request$server_name

    if (is.null(url)) {
      cli::cli_abort(
        message = c(x = "Could not determine url.")
      )
    }

    port <- request$SERVER_PORT %||% request$server_port

    if (!is.null(port)) {
      url <- paste(url, port, sep = ":")
    }

    scheme <- request$rook.url_scheme
  }

  return(
    paste0(
      scheme,
      "://",
      url
    )
  )
}
