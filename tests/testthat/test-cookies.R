test_that(".slack_token_cookie_name works", {
  expect_identical(
    .slack_token_cookie_name(team_id = "myteam"),
    "myteam_slack_token"
  )
})
