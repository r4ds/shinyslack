#' Fetch Slack User Info
#'
#' Get information about the logged-in user from the Slack API.
#'
#' @param components A character vector of user components to include
#'   (\code{"user_name"} and/or \code{"user_id"}).
#'
#' @return A \code{\link[shiny]{reactive}} with a named character vector.
#' @export
user_info <- function(components = c("user_name", "user_id"),
                      simplify = FALSE) {
  components <- match.arg(components, several.ok = TRUE)
  return(
    shiny::reactive({
      slack_info <- slackcalls::post_slack(
        slack_method = "auth.test"
      )
      c(
        user_name = slack_info$user,
        user_id = slack_info$user_id
      )[components]
    })
  )
}
