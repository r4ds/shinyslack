#' Encrypt a String If Possible
#'
#' @inheritParams .shared-parameters
#' @param string A length-1 character to encrypt (or decrypt).
#'
#' @return If `shinyslack_key` is non-empty, the encrypted string. Otherwise the
#'   original string is returned.
#' @keywords internal
.shinyslack_encrypt <- function(string,
                                shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  if (isTRUE(as.logical(nchar(shinyslack_key)))) {
    cli::cli_inform(c(
      "shinyslack: shinyslack_key set.",
      v = "Encrypting string."
    ))
    string <- .sodium_encrypt(string, shinyslack_key)
  } else {
    cli::cli_warn(c(
      "shinyslack: shinyslack_key not found.",
      x = "String not encoded."
    ))
  }

  return(string)
}

.sodium_encrypt <- function(string, key) {
  return(
    sodium::bin2hex(sodium::data_encrypt(
      msg = charToRaw(string),
      key = charToRaw(key),
      nonce = .shinyslack_nonce
    ))
  )
}

#' Decrypt a String If Possible
#'
#' @inheritParams .shared-parameters
#'
#' @return If `shinyslack_key` is non-empty, the decrypted string (or
#'   "bad_string" if decryption fails). Otherwise the original string is
#'   returned.
#' @keywords internal
.shinyslack_decrypt <- function(string,
                                shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  if (length(string) && isTRUE(as.logical(nchar(shinyslack_key)))) {
    string <- tryCatch(
      error = function(cnd) "bad_string",
      .sodium_decrypt(string, shinyslack_key)
    )
  }
  return(string)
}

.sodium_decrypt <- function(string, key) {
  return(
    rawToChar(sodium::data_decrypt(
      bin = sodium::hex2bin(string),
      key = charToRaw(key),
      nonce = .shinyslack_nonce
    ))
  )
}
