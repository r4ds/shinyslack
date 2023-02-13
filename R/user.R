#' Fetch Slack User Info
#'
#' Get information about the logged-in user from the Slack API.
#'
#' @param components A character vector of user components to include. Current
#'   options are:
#'   - user_id: The ID used to uniquely identify this user on this Slack team.
#'   - real_name: The full name of this user as entered in their profile.
#'   - display_name: The name that the user has chosen to display to other users in Slack.
#'   - pronouns: The pronouns set by this user, if any.
#'   - user_name: You probably do not want this. It is a legacy piece of information.
#'
#' @return A [shiny::reactive()] with a named character vector.
#' @export
user_info <- function(components = c("user_id",
                                     "real_name",
                                     "display_name",
                                     "pronouns",
                                     "user_name")) {
  components <- match.arg(components, several.ok = TRUE)
  return(
    shiny::reactive({
      # We need their ID in order to get the rest.
      user_id <- slackcalls::post_slack("auth.test")$user_id

      slack_info <- slackcalls::post_slack("users.info", user = user_id)$user

      c(
        user_id = slack_info$id,
        real_name = slack_info$profile$real_name,
        display_name = slack_info$profile$display_name,
        pronouns = slack_info$profile$pronouns,
        user_name = slack_info$name
      )[components]
    })
  )
}
