# Unit tests for the pure TOTP implementation (R/auth/totp.R).

test_that("base32 round-trips arbitrary bytes", {
  source_from_root("R/auth/totp.R")

  for (n in c(1, 5, 10, 20, 33)) {
    bytes <- as.raw(sample(0:255, n, replace = TRUE))
    expect_identical(base32_decode(base32_encode(bytes)), bytes)
  }
})

test_that("TOTP matches the RFC 6238 SHA-1 test vector", {
  source_from_root("R/auth/totp.R")

  # RFC 6238 Appendix B: ASCII secret "12345678901234567890", T=59s
  # -> 8-digit code 94287082; the 6-digit code is its last six digits.
  secret <- base32_encode(charToRaw("12345678901234567890"))
  t59 <- as.POSIXct(59, origin = "1970-01-01", tz = "UTC")

  expect_identical(totp_code_for_step(secret, totp_current_step(t59)), "287082")

  # T=1111111109 -> 07081804
  t2 <- as.POSIXct(1111111109, origin = "1970-01-01", tz = "UTC")
  expect_identical(totp_code_for_step(secret, totp_current_step(t2)), "081804")
})

test_that("verification accepts codes within the window and rejects others", {
  source_from_root("R/auth/totp.R")

  secret <- totp_generate_secret()
  now <- Sys.time()
  current_step <- totp_current_step(now)

  expect_identical(
    totp_verify_code(secret, totp_code_for_step(secret, current_step), time = now),
    current_step
  )
  # previous/next step accepted within window = 1
  expect_identical(
    totp_verify_code(secret, totp_code_for_step(secret, current_step - 1), time = now),
    current_step - 1
  )
  # two steps out is rejected
  expect_null(
    totp_verify_code(secret, totp_code_for_step(secret, current_step + 2), time = now)
  )
  # garbage rejected
  expect_null(totp_verify_code(secret, "000000x", time = now))
  expect_null(totp_verify_code(secret, NULL, time = now))
})

test_that("provisioning URI carries the expected fields", {
  source_from_root("R/auth/totp.R")

  uri <- totp_provisioning_uri("ABC234", "jane.doe")
  expect_match(uri, "^otpauth://totp/RIDS:jane.doe\\?secret=ABC234&issuer=RIDS")
  expect_match(uri, "digits=6")
  expect_match(uri, "period=30")
})
