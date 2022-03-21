#' Extract an Individual Cookie from a Shiny Request
#'
#' @inheritParams .extract_cookies
#' @param cookie_name The name of the individual cookie to extract.
#'
#' @return The contents of that cookie.
#' @keywords internal
.extract_cookie <- function(request, cookie_name) {
  cookies <- .extract_cookies(request = request)

  if (length(cookies) && cookie_name %in% names(cookies)) {
    return(cookies[cookie_name])
  } else {
    return(NA_character_)
  }
}

#' Extract All Cookies from a Shiny Request
#'
#' @inheritParams .shared-parameters
#'
#' @return All cookies in the request, as a list.
#' @keywords internal
.extract_cookies <- function(request) {
  cookies <- request$HTTP_COOKIE

  # Based on shiny::parseQueryString
  if (is.null(cookies) || nchar(cookies) == 0) {
    return(NULL)
  }
  pairs <- strsplit(cookies, "; ", fixed = TRUE)[[1]]
  pairs <- pairs[pairs != ""]
  pairs <- strsplit(pairs, "=", fixed = TRUE)
  keys <- vapply(pairs, function(x) x[1], FUN.VALUE = character(1))
  values <- vapply(pairs, function(x) x[2], FUN.VALUE = character(1))
  values[is.na(values)] <- ""
  keys <- gsub("+", " ", keys, fixed = TRUE)
  values <- gsub("+", " ", values, fixed = TRUE)
  keys <- httpuv::decodeURIComponent(keys)
  values <- httpuv::decodeURIComponent(values)
  res <- stats::setNames(as.list(values), keys)
  return(res)
}

#' The Name of the Slack Token Cookie
#' @inheritParams .shared-parameters
#' @return The name of the Slack token cookie
#' @keywords internal
.slack_token_cookie_name <- function(team_id) {
  return(
    paste(team_id, "slack_token", sep = "_")
  )
}

#' Shiny Tags to Add Cookies
#'
#' This function generates javascript which will set a cookie in the user's
#' browser. It requires that the cookie javascript is already initialized using
#' \code{\link[shinycookie]{initShinyCookie}}.
#'
#' @param contents The contents of the cookie. Right now this should be a single
#'   character value.
#' @param cookie_name A name for the cookie. Must be a valid cookie name.
#' @param expiration Days after which the cookie should expire.
#'
#' @return A \code{\link[shiny]{tagList}} that provides the HTML and javascript
#'   to set the cookie.
#' @export
set_cookie <- function(contents, cookie_name, expiration = 90) {
  return(
    shiny::tags$script(
      shiny::HTML(
        sprintf(
          "Cookies.set('%s', '%s', { expires: %i });",
          cookie_name,
          contents,
          expiration
        )
      )
    )
  )
}
