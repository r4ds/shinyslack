# To deploy, run: rsconnect::deployApp() (or click the blue button)
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)

team_id <- "T6UC1DKJQ"
Sys.unsetenv("SLACK_API_TOKEN")

ui <- shiny::fluidPage(shiny::textOutput("user_name"))

server <- function(input, output, session) {
  user_name <- user_info(components = "display_name")
  output$user_name <- shiny::renderText({
    user_name()
  })
}

shinyslack_app(
  ui = ui,
  server = server,
  team_id = team_id
)
