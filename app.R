# To deploy, run: rsconnect::deployApp() (or click the blue button)
pkgload::load_all(
  export_all = FALSE,
  helpers = FALSE,
  attach_testthat = FALSE,
  quiet = TRUE
)

team_id <- "T6UC1DKJQ"

ui <- cookies::add_cookie_handlers(
  shiny::fluidPage(shiny::textOutput("user_name"))
)

server <- function(input, output, session) {
  is_logged_in <- check_login(
    input = input,
    team_id = team_id
  )

  username <- user_info(
    components = "user_name"
  )

  output$user_name <- shiny::renderText({
    shiny::req(is_logged_in())
    username()
  })
}

shinyslack_app(
  ui = ui,
  server = server,
  team_id = team_id,
  site_url = "https://r4dscommunity.shinyapps.io/shinyslack/"
)
