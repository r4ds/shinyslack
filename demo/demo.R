library(shiny)
library(shinyslack)

ui <- fluidPage(
  textOutput("user_name")
)

server <- function(input, output, session) {
  team_id <- "T6UC1DKJQ"
  is_logged_in <- check_login(input, team_id)

  username <- shiny::reactive({
    shiny::req(is_logged_in())
    user_info <- slackcalls::post_slack(
      slack_method = "auth.test"
    )
    user_info$user
  })

  output$user_name <- shiny::renderText(
    username()
  )
}

site_url <- "http://127.0.0.1:4242/"
shiny::shinyApp(
  ui = slack_shiny_ui(
    ui,
    team_id = "T6UC1DKJQ",
    site_url = site_url
  ),
  server = server,
  options = list(
    port = 4242L,
    launch.browser = TRUE
  )
)
