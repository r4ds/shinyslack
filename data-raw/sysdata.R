# Do this once to get a permanent nonce used by this package.

# .shinyslack_nonce <- sodium::random(24)
# saveRDS(.shinyslack_nonce, "data-raw/nonce.rds")

.shinyslack_nonce <- readRDS("data-raw/nonce.rds")

usethis::use_data(
  .shinyslack_nonce,
  overwrite = TRUE,
  internal = TRUE
)

rm(.shinyslack_nonce)
