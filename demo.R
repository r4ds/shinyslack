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
shinyslack_key <- readLines(".secret", 1)

Sys.setenv(
  shinyslack_key = shinyslack_key
)

ui <- shiny::fluidPage(
  shiny::textOutput("user_name")
)

server <- function(input, output, session) {
  is_logged_in <- check_login(input, team_id)

  username <- shiny::reactive({
    shiny::req(is_logged_in())
    user_info <- slackcalls::post_slack(
      slack_method = "auth.test"
    )

    # Log the current user.
    message(
      "***** Login by: ",
      user_info$user
    )
    user_info$user
  })

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
