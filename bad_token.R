# Launch the ShinyApp (Do not remove this comment)
# This sets a bad token intentionally. Only used locally for testing.

team_id <- "T6UC1DKJQ"

devtools::load_all(".")

ui <- shiny::fluidPage(
  # Add the code to add the cookie.
  cookies::set_cookie_response(
    cookie_name = .slack_token_cookie_name(team_id),
    cookie_value = "bad_token"
  )
)

server <- function(input, output, session) {
}

shiny::shinyApp(
  ui = ui,
  server = server,
  options = list(
    port = 4242L,
    launch.browser = TRUE
  )
)
