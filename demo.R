# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)

if (interactive()) {
  site_url <- "http://127.0.0.1:4242/"
  options <- list(
    port = 4242L,
    launch.browser = TRUE
  )
} else {
  site_url <- "https://r4dscommunity.shinyapps.io/shinyslack/"
  options <- list()
}

team_id <- "T6UC1DKJQ"

ui <- shiny::fluidPage(
  shiny::textOutput("user_name")
)

server <- function(input, output, session) {
  username <- user_info(
    components = "user_name"
  )

  output$user_name <- shiny::renderText(
    username()
  )
}

shiny::shinyApp(
  ui = slack_shiny_ui(
    ui,
    team_id = team_id,
    site_url = site_url
  ),
  server = server,
  options = options
)
