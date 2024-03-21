#' Parameters Used in Various Functions
#'
#' @param request The shiny request object to parse.
#' @param team_id The Slack team ID through which the user is being
#'   authenticated.
#' @param ui A 0- or 1-argument function defining the UI of a Shiny app, or a
#'   [shiny::tagList()].
#' @param session The shiny session object. The default
#'   [shiny::getDefaultReactiveDomain()] is likely always sufficient outside of
#'   tests.
#' @param shinyslack_key (optional) A key to use to encrypt the string. If not
#'   set, the string is returned unencrypted.
#' @param slack_api_key The Slack API key to use. The default value should
#'   likely always be used outside of tests.
#' @name .shared-parameters
#' @keywords internal
NULL
