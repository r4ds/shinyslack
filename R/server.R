.shinyslack_server <- function(server, team_id) {
  force(team_id)
  return(
    function(input, output, session) {
      session$userData$shinyslack_team_id = team_id
      server(input, output, session)
    }
  )
}
