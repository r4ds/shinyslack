#' Fetch Slack User Info
#'
#' Get information about the logged-in user from the Slack API.
#'
#' @inheritParams .shared-parameters
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
                                     "user_name"),
                      session = shiny::getDefaultReactiveDomain(),
                      slack_api_key = session$userData$shinyslack_api_key,
                      team_id = get_shinyslack_team_id(),
                      shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  components <- match.arg(components, several.ok = TRUE)
  return(
    shiny::reactive({
      slack_api_key <- .update_shinyslack_api_key(
        slack_api_key,
        team_id,
        session,
        shinyslack_key
      )
      if (!is.null(slack_api_key)) {
        basics <- slackcalls::post_slack("auth.test", token = slack_api_key)
        if (all(components %in% c("user_id", "user_name"))) {
          return(c(user_id = basics$user_id, user_name = basics$user)[components])
        }
        return(.get_more_user_info(basics$user_id, slack_api_key, components))
      }
      return(NULL)
    })
  )
}

.get_more_user_info <- function(user_id, slack_api_key, components) {
  slack_info <- slackcalls::post_slack(
    "users.info", user = user_id, token = slack_api_key
  )$user
  return(
    c(
      user_id = slack_info$id,
      real_name = slack_info$profile$real_name,
      display_name = slack_info$profile$display_name,
      pronouns = slack_info$profile$pronouns,
      user_name = slack_info$name
    )[components]
  )
}
