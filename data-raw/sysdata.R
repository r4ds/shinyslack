# Do this once to get a permanent nonce used by this package.

# .shinyslack_nonce <- sodium::random(24)
# saveRDS(.shinyslack_nonce, "data-raw/nonce.rds")

.shinyslack_nonce <- readRDS("data-raw/nonce.rds")

.slack_logo_svg <- readLines("data-raw/slack_logo.svg")

.slack_button_style <- paste(
  sep = ";",
  "align-items:center",
  "color:#000",
  "background-color:#fff",
  "border:1px solid #ddd",
  "border-radius:4px",
  "display:inline-flex",
  "font-family:Lato, sans-serif",
  "font-size:18px",
  "font-weight:600",
  "height:56px",
  "justify-content:center",
  "text-decoration:none",
  "width:296px"
)

usethis::use_data(
  .shinyslack_nonce,
  .slack_logo_svg,
  .slack_button_style,
  overwrite = TRUE,
  internal = TRUE
)

rm(
  .shinyslack_nonce,
  .slack_logo_svg,
  .slack_button_style
)
