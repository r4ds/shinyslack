#' Request Predicates
#'
#' These predicate functions check for the presence of various objects in the
#' request object.
#' @inheritParams .shared-parameters
#' @return Logical scalar.
#' @name request-predicates
#' @keywords internal
NULL

#' @rdname request-predicates
.has_token <- function(request, team_id) {
  return(
    .has_cookie_token(request, team_id) ||
      .has_url_token(request)
  )
}

#' @rdname request-predicates
.has_cookie_token <- function(request, team_id) {
  cookie_token <- .extract_cookie(
    request,
    .slack_token_cookie_name(team_id)
  )

  return(
    !is.na(cookie_token) && .validate_cookie_token(cookie_token, team_id)
  )
}

#' @rdname request-predicates
.has_url_token <- function(request) {
  return(
    !is.null(.extract_url_token(request))
  )
}

#' @rdname request-predicates
.has_auth_code <- function(request) {
  # Ideally we should check for a state parameter in the query and make sure
  # it's what we expect. But we can just return the code for now.
  !is.null(.extract_auth_code(request))
}

#' Extract Oauth Objects from a Shiny Request
#'
#' @inheritParams .shared-parameters
#'
#' @return The oauth object if it exists, or NULL.
#' @name oauth-extractors
#' @keywords internal
NULL

#' @rdname oauth-extractors
.extract_url_token <- function(request) {
  return(
    shiny::parseQueryString(request$QUERY_STRING)$oauth_token
  )
}

#' @rdname oauth-extractors
.extract_auth_code <- function(request) {
  shiny::parseQueryString(request$QUERY_STRING)$code
}
