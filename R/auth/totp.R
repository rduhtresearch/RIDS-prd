# RFC 6238 TOTP (time-based one-time passwords), pure R.
# HMAC-SHA1, 6 digits, 30-second steps — the defaults every authenticator
# app (Google Authenticator, Authy, 1Password, ...) expects.

TOTP_DIGITS <- 6L
TOTP_STEP_SECONDS <- 30L

# RFC 4648 base32 (no padding), as used in otpauth:// URIs.
base32_encode <- function(raw_bytes) {
  alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "")[[1]]
  bits <- as.integer(rawToBits(raw_bytes))
  # rawToBits is little-endian per byte; regroup to MSB-first per byte
  bits <- unlist(lapply(seq(1, length(bits), by = 8), function(i) rev(bits[i:(i + 7)])))

  pad <- (5 - length(bits) %% 5) %% 5
  bits <- c(bits, rep(0L, pad))

  chunks <- matrix(bits, nrow = 5)
  values <- as.integer(2^(4:0) %*% chunks)
  paste(alphabet[values + 1], collapse = "")
}

base32_decode <- function(text) {
  alphabet <- strsplit("ABCDEFGHIJKLMNOPQRSTUVWXYZ234567", "")[[1]]
  chars <- strsplit(toupper(gsub("[ =-]", "", text)), "")[[1]]
  values <- match(chars, alphabet) - 1L
  if (any(is.na(values))) {
    stop("Invalid base32 input.")
  }

  bits <- unlist(lapply(values, function(v) as.integer(intToBits(v))[5:1]))
  n_bytes <- length(bits) %/% 8
  bits <- bits[seq_len(n_bytes * 8)]
  chunks <- matrix(bits, nrow = 8)
  as.raw(as.integer(2^(7:0) %*% chunks))
}

totp_generate_secret <- function() {
  base32_encode(sodium::random(20))
}

totp_current_step <- function(time = Sys.time()) {
  as.numeric(time) %/% TOTP_STEP_SECONDS
}

totp_code_for_step <- function(secret_base32, step) {
  key <- base32_decode(secret_base32)

  counter <- raw(8)
  remaining <- step
  for (i in 8:1) {
    counter[i] <- as.raw(remaining %% 256)
    remaining <- remaining %/% 256
  }

  mac <- digest::hmac(key, counter, algo = "sha1", raw = TRUE)
  offset <- as.integer(mac[20]) %% 16L
  code <- (as.integer(mac[offset + 1]) %% 128L) * 2^24 +
    as.integer(mac[offset + 2]) * 2^16 +
    as.integer(mac[offset + 3]) * 2^8 +
    as.integer(mac[offset + 4])

  sprintf(paste0("%0", TOTP_DIGITS, "d"), code %% 10^TOTP_DIGITS)
}

#' Verify a TOTP code within +/- window steps of the current time.
#' Returns the matched step (for replay protection) or NULL if no match.
totp_verify_code <- function(secret_base32, code, window = 1L, time = Sys.time()) {
  code <- gsub("[^0-9]", "", as.character(code %||% ""))
  if (nchar(code) != TOTP_DIGITS) {
    return(NULL)
  }

  current <- totp_current_step(time)
  for (offset in seq(-window, window)) {
    step <- current + offset
    if (identical(totp_code_for_step(secret_base32, step), code)) {
      return(step)
    }
  }

  NULL
}

totp_provisioning_uri <- function(secret_base32, username, issuer = "RIDS") {
  sprintf(
    "otpauth://totp/%s:%s?secret=%s&issuer=%s&algorithm=SHA1&digits=%d&period=%d",
    utils::URLencode(issuer, reserved = TRUE),
    utils::URLencode(username, reserved = TRUE),
    secret_base32,
    utils::URLencode(issuer, reserved = TRUE),
    TOTP_DIGITS,
    TOTP_STEP_SECONDS
  )
}
