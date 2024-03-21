#' Encrypt a String If Possible
#'
#' @param string A length-1 character to encrypt (or decrypt).
#' @param shinyslack_key (optional) A key to use to encrypt the string. If not
#'   set, the string is returned unencrypted.
#'
#' @return If `shinyslack_key` is non-empty, the encrypted string. Otherwise the
#'   original string is returned.
#' @keywords internal
.shinyslack_encrypt <- function(string,
                                shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  if (isTRUE(as.logical(nchar(shinyslack_key)))) {
    cli::cli_inform(c("shinyslack_key set.", v = "Encrypting string."))
    string <- sodium::bin2hex(
      sodium::data_encrypt(
        msg = charToRaw(string),
        key = charToRaw(shinyslack_key),
        nonce = .shinyslack_nonce
      )
    )
  } else {
    cli::cli_warn(c("shinyslack_key not found.", x = "String not encoded."))
  }

  return(string)
}

#' Decrypt a String If Possible
#'
#' @inheritParams .shinyslack_encrypt
#'
#' @return If `shinyslack_key` non-empty, the decrypted string (or "bad_string"
#'   if decryption fails). Otherwise the original string is returned.
#' @keywords internal
.shinyslack_decrypt <- function(string,
                                shinyslack_key = Sys.getenv("SHINYSLACK_KEY")) {
  if (isTRUE(as.logical(nchar(shinyslack_key)))) {
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
