#' Encrypt a String If Possible
#'
#' @param string A length-1 character to encrypt (or decrypt).
#'
#' @return If `shinyslack_key` environment variable is set, the encrypted
#'   string. Otherwise the original string is returned.
#' @keywords internal
.shinyslack_encrypt <- function(string) {
  shinyslack_key <- Sys.getenv("shinyslack_key", NA)

  if (!is.na(shinyslack_key)) {
    string <- sodium::bin2hex(
      sodium::data_encrypt(
        msg = charToRaw(string),
        key = charToRaw(shinyslack_key),
        nonce = .shinyslack_nonce
      )
    )
  }

  return(string)
}

#' Decrypt a String If Possible
#'
#' @inheritParams .shinyslack_encrypt
#'
#' @return If `shinyslack_key` environment variable is set, the decrypted string
#'   (or "bad_string" if decryption fails). Otherwise the original string is
#'   returned.
#' @keywords internal
.shinyslack_decrypt <- function(string) {
  shinyslack_key <- Sys.getenv("shinyslack_key", NA)

  if (!is.na(shinyslack_key)) {
    string <- tryCatch(
      error = function(cnd) "bad_string",
      rawToChar(
        sodium::data_decrypt(
          bin = sodium::hex2bin(string),
          key = charToRaw(shinyslack_key),
          nonce = .shinyslack_nonce
        )
      )
    )
  }

  return(string)
}
