# To deploy, run: rsconnect::deployApp() (or click the blue button)
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)

team_id <- "T6UC1DKJQ"

ui <- shiny::fluidPage(shiny::textOutput("user_name"))

server <- function(input, output, session) {
  username <- user_info(
    components = "user_name"
  )

  output$user_name <- shiny::renderText({
    shiny::req(check_login(team_id)())
    username()
  })
}

shinyslack_app(
  ui = ui,
  server = server,
  team_id = team_id
)
