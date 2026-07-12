# Authentication: password hashing, roles, sessions, user accounts.
# All database access goes through the repositories in
# R/persistence/repositories/ (via the rids_repos() accessor).

source("R/persistence/connection.R", local = FALSE)
source("R/persistence/repositories/settings_repository.R", local = FALSE)
source("R/persistence/repositories/app_log_repository.R", local = FALSE)
source("R/persistence/repositories/api_credential_repository.R", local = FALSE)
source("R/persistence/repositories/user_repository.R", local = FALSE)
source("R/persistence/repositories/session_repository.R", local = FALSE)
source("R/persistence/repositories/auth_audit_repository.R", local = FALSE)
source("R/persistence/repositories/study_repository.R", local = FALSE)
source("R/persistence/repositories/template_version_repository.R", local = FALSE)
source("R/persistence/repositories/ict_costing_repository.R", local = FALSE)
source("R/persistence/repositories/posting_line_repository.R", local = FALSE)
source("R/persistence/repositories/rules_repository.R", local = FALSE)
source("R/persistence/repositories/speciality_repository.R", local = FALSE)
source("R/persistence/repositories/mfa_repository.R", local = FALSE)

hash_password <- function(pw) {
  tryCatch({
    sodium::password_store(pw)
  }, error = function(e) {
    app_log_exception("auth", "Password hashing failed", e)
    NULL
  })
}

verify_password <- function(pw, pw_hash) {
  tryCatch({
    isTRUE(sodium::password_verify(pw_hash, pw))
  }, error = function(e) {
    app_log_exception("auth", "Password verification failed", e)
    FALSE
  })
}

# Role model: user / admin only. (The vestigial 'developer' role was removed;
# migration 0006 maps any existing developer rows to admin.)
normalize_role <- function(role) {
  role <- tolower(trimws(role %||% "user"))

  if (role == "admin") {
    return("admin")
  }

  "user"
}

is_admin <- function(role) {
  identical(normalize_role(role), "admin")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    return(y)
  }

  x
}

AUTH_COOKIE_NAME <- "rids_auth_token"

session_duration_hours <- function() {
  if (exists("AUTH_SESSION_HOURS", inherits = TRUE)) {
    return(AUTH_SESSION_HOURS)
  }

  10
}

session_duration_seconds <- function() {
  as.integer(session_duration_hours() * 60 * 60)
}

generate_session_token <- function() {
  sodium::bin2hex(sodium::random(32))
}

hash_session_token <- function(token) {
  sodium::bin2hex(sodium::hash(charToRaw(token)))
}

get_cookie_value <- function(cookie_header, cookie_name) {
  if (is.null(cookie_header) || identical(cookie_header, "")) {
    return("")
  }

  cookie_parts <- strsplit(cookie_header, ";", fixed = TRUE)[[1]]
  cookie_parts <- trimws(cookie_parts)
  cookie_prefix <- paste0(cookie_name, "=")
  matched_cookie <- cookie_parts[startsWith(cookie_parts, cookie_prefix)]

  if (length(matched_cookie) == 0) {
    return("")
  }

  utils::URLdecode(sub(cookie_prefix, "", matched_cookie[[1]], fixed = TRUE))
}

sanitize_text_value <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }

  x <- as.character(x)
  vapply(x, function(value) {
    raw_value <- charToRaw(enc2utf8(value))
    raw_value <- raw_value[raw_value != as.raw(0)]
    rawToChar(raw_value)
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

users_exist <- function() {
  tryCatch({
    rids_repos()$users$count() > 0
  }, error = function(e) {
    app_log_exception("auth", "User existence check failed", e)
    FALSE
  })
}

log_auth_event <- function(event_type,
                           user_id = NULL,
                           actor_user_id = NULL,
                           username = NULL,
                           success = TRUE,
                           message = NULL,
                           session_id = NULL) {
  tryCatch({
    user_id <- user_id %||% NA_integer_
    actor_user_id <- actor_user_id %||% NA_integer_
    username <- username %||% NA_character_
    message <- message %||% NA_character_
    session_id <- session_id %||% NA_integer_

    rids_repos()$auth_audit$record(
      event_type = event_type,
      user_id = user_id,
      actor_user_id = actor_user_id,
      username = username,
      success = success,
      message = message,
      session_id = session_id
    )

    if (exists("log_event", mode = "function")) {
      level <- if (isTRUE(success)) {
        "INFO"
      } else if (event_type %in% c("session_expired", "session_revoked")) {
        "WARN"
      } else {
        "WARN"
      }

      log_event(
        level = level,
        area = "auth",
        message = message %||% gsub("_", " ", event_type),
        user_id = user_id,
        username = username,
        session_id = session_id,
        details = list(
          auth_event_type = event_type,
          actor_user_id = actor_user_id,
          success = isTRUE(success)
        )
      )
    }
  }, error = function(e) {
    app_log_exception("auth", "Auth audit log write failed", e)
  })
}

get_user_by_username <- function(username) {
  tryCatch({
    rids_repos()$users$find_by_username(username)
  }, error = function(e) {
    app_log_exception("auth", "Lookup by username failed", e, list(username = username))
    NULL
  })
}

get_user_by_id <- function(user_id) {
  tryCatch({
    rids_repos()$users$find_by_id(user_id)
  }, error = function(e) {
    app_log_exception("auth", "Lookup by user id failed", e, list(user_id = user_id))
    NULL
  })
}

touch_last_login <- function(user_id) {
  tryCatch({
    rids_repos()$users$touch_last_login(user_id)
  }, error = function(e) {
    app_log_exception("auth", "Last login update failed", e, list(user_id = user_id))
  })
}

create_auth_session <- function(user_id, user_agent = NULL, duration_hours = session_duration_hours()) {
  token <- generate_session_token()
  token_hash <- hash_session_token(token)
  expires_at <- format(Sys.time() + (duration_hours * 60 * 60), "%Y-%m-%d %H:%M:%S")

  session_id <- rids_repos()$sessions$insert(
    user_id = user_id,
    token_hash = token_hash,
    expires_at = expires_at,
    user_agent = user_agent %||% NA_character_
  )

  list(
    session_id = session_id,
    token = token,
    max_age = as.integer(duration_hours * 60 * 60)
  )
}

revoke_auth_session <- function(session_id, actor_user_id = NULL, event_type = "logout", message = "Session revoked") {
  if (is.null(session_id) || is.na(session_id)) {
    return(invisible(FALSE))
  }

  session_row <- rids_repos()$sessions$find_brief(session_id)

  rids_repos()$sessions$revoke(session_id)

  if (nrow(session_row) > 0) {
    log_auth_event(
      event_type = event_type,
      user_id = session_row$user_id[[1]],
      actor_user_id = actor_user_id %||% session_row$user_id[[1]],
      username = session_row$username[[1]],
      success = TRUE,
      message = message,
      session_id = session_id
    )
  }

  invisible(TRUE)
}

restore_auth_session <- function(token) {
  if (is.null(token) || identical(token, "")) {
    return(list(success = FALSE, status = "missing"))
  }

  token_hash <- hash_session_token(token)

  session_row <- rids_repos()$sessions$find_by_token_hash(token_hash)

  if (nrow(session_row) == 0) {
    return(list(success = FALSE, status = "invalid"))
  }

  row <- session_row[1, , drop = FALSE]
  now <- Sys.time()
  expires_at <- as.POSIXct(row$expires_at[[1]], tz = Sys.timezone())

  if (!is.na(row$revoked_at[[1]])) {
    log_auth_event(
      event_type = "session_revoked",
      user_id = row$user_id[[1]],
      actor_user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "Attempted restore for revoked session",
      session_id = row$session_id[[1]]
    )
    return(list(success = FALSE, status = "revoked", session = row))
  }

  if (is.na(expires_at) || expires_at <= now) {
    log_auth_event(
      event_type = "session_expired",
      user_id = row$user_id[[1]],
      actor_user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "Attempted restore for expired session",
      session_id = row$session_id[[1]]
    )
    return(list(success = FALSE, status = "expired", session = row))
  }

  if (!isTRUE(row$active[[1]])) {
    log_auth_event(
      event_type = "session_revoked",
      user_id = row$user_id[[1]],
      actor_user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "Attempted restore for inactive user",
      session_id = row$session_id[[1]]
    )
    return(list(success = FALSE, status = "inactive", session = row))
  }

  log_auth_event(
    event_type = "session_restored",
    user_id = row$user_id[[1]],
    actor_user_id = row$user_id[[1]],
    username = row$username[[1]],
    success = TRUE,
    message = "Session restored from cookie",
    session_id = row$session_id[[1]]
  )

  list(success = TRUE, status = "ok", session = row)
}

authenticate_user <- function(username, password) {
  user_row <- get_user_by_username(username)

  if (is.null(user_row)) {
    log_auth_event(
      event_type = "login_failure",
      username = username,
      success = FALSE,
      message = "Unknown username"
    )
    return(list(success = FALSE, reason = "invalid_credentials"))
  }

  row <- user_row[1, , drop = FALSE]

  if (!isTRUE(row$active[[1]])) {
    log_auth_event(
      event_type = "login_failure",
      user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "Inactive user"
    )
    return(list(success = FALSE, reason = "inactive"))
  }

  if (is.na(row$password_hash[[1]]) || identical(row$password_hash[[1]], "")) {
    log_auth_event(
      event_type = "login_failure",
      user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "User has no password hash"
    )
    return(list(success = FALSE, reason = "invalid_credentials"))
  }

  if (!verify_password(password, row$password_hash[[1]])) {
    log_auth_event(
      event_type = "login_failure",
      user_id = row$user_id[[1]],
      username = row$username[[1]],
      success = FALSE,
      message = "Password verification failed"
    )
    return(list(success = FALSE, reason = "invalid_credentials"))
  }

  list(success = TRUE, user = row)
}

create_user_account <- function(name,
                                username,
                                email = NULL,
                                role = "user",
                                temporary_password,
                                active = TRUE,
                                actor_user_id = NULL) {
  username <- trimws(username)
  email <- trimws(email %||% "")
  role <- normalize_role(role)
  name <- trimws(name %||% "")

  existing <- get_user_by_username(username)
  if (!is.null(existing)) {
    return(list(success = FALSE, message = "That username already exists."))
  }

  password_hash <- hash_password(temporary_password)
  if (is.null(password_hash)) {
    return(list(success = FALSE, message = "Failed to create password hash."))
  }

  user_id <- rids_repos()$users$insert(
    name = if (identical(name, "")) NA_character_ else name,
    username = username,
    email = if (identical(email, "")) NA_character_ else email,
    password_hash = password_hash,
    role = role,
    active = active,
    force_password_change = TRUE
  )
  user_row <- get_user_by_id(user_id)

  log_auth_event(
    event_type = "user_created",
    user_id = user_id,
    actor_user_id = actor_user_id,
    username = username,
    success = TRUE,
    message = paste("Role:", role)
  )

  list(success = TRUE, user = user_row, temporary_password = temporary_password)
}

update_user_account <- function(user_id,
                                name,
                                username,
                                email,
                                role,
                                active,
                                actor_user_id = NULL) {
  current_user <- get_user_by_id(user_id)
  if (is.null(current_user)) {
    return(list(success = FALSE, message = "User not found."))
  }

  username <- trimws(username)
  email <- trimws(email %||% "")
  role <- normalize_role(role)
  name <- trimws(name %||% "")

  if (rids_repos()$users$username_taken_by_other(username, user_id)) {
    return(list(success = FALSE, message = "That username already exists."))
  }

  rids_repos()$users$update_account(
    user_id = user_id,
    name = if (identical(name, "")) NA_character_ else name,
    username = username,
    email = if (identical(email, "")) NA_character_ else email,
    role = role,
    active = active
  )

  current_active <- isTRUE(current_user$active[[1]])
  new_active <- isTRUE(active)

  if (current_active != new_active) {
    log_auth_event(
      event_type = if (new_active) "user_reactivated" else "user_deactivated",
      user_id = user_id,
      actor_user_id = actor_user_id,
      username = username,
      success = TRUE,
      message = if (new_active) "User reactivated" else "User deactivated"
    )
  }

  list(success = TRUE, user = get_user_by_id(user_id))
}

set_user_active <- function(user_id, active, actor_user_id = NULL) {
  current_user <- get_user_by_id(user_id)
  if (is.null(current_user)) {
    return(list(success = FALSE, message = "User not found."))
  }

  rids_repos()$users$set_active(user_id, active)

  if (!isTRUE(active)) {
    rids_repos()$sessions$revoke_all_for_user(user_id)
  }

  log_auth_event(
    event_type = if (isTRUE(active)) "user_reactivated" else "user_deactivated",
    user_id = user_id,
    actor_user_id = actor_user_id,
    username = current_user$username[[1]],
    success = TRUE,
    message = if (isTRUE(active)) "User reactivated" else "User deactivated"
  )

  list(success = TRUE, user = get_user_by_id(user_id))
}

reset_user_password <- function(user_id, temporary_password, actor_user_id = NULL) {
  user_row <- get_user_by_id(user_id)
  if (is.null(user_row)) {
    return(list(success = FALSE, message = "User not found."))
  }

  password_hash <- hash_password(temporary_password)
  if (is.null(password_hash)) {
    return(list(success = FALSE, message = "Failed to reset password."))
  }

  rids_repos()$users$set_password_hash(user_id, password_hash, force_password_change = TRUE)
  rids_repos()$sessions$revoke_all_for_user(user_id)

  log_auth_event(
    event_type = "admin_password_reset",
    user_id = user_id,
    actor_user_id = actor_user_id,
    username = user_row$username[[1]],
    success = TRUE,
    message = "Temporary password issued"
  )

  list(success = TRUE, temporary_password = temporary_password, user = get_user_by_id(user_id))
}

change_user_password <- function(user_id,
                                 current_password,
                                 new_password,
                                 actor_user_id = NULL) {
  user_row <- get_user_by_id(user_id)
  if (is.null(user_row)) {
    return(list(success = FALSE, message = "User not found."))
  }

  if (!verify_password(current_password, user_row$password_hash[[1]])) {
    log_auth_event(
      event_type = "password_change",
      user_id = user_id,
      actor_user_id = actor_user_id %||% user_id,
      username = user_row$username[[1]],
      success = FALSE,
      message = "Current password verification failed"
    )
    return(list(success = FALSE, message = "Current password is incorrect."))
  }

  password_hash <- hash_password(new_password)
  if (is.null(password_hash)) {
    return(list(success = FALSE, message = "Failed to update password."))
  }

  rids_repos()$users$set_password_hash(user_id, password_hash, force_password_change = FALSE)

  log_auth_event(
    event_type = "password_changed",
    user_id = user_id,
    actor_user_id = actor_user_id %||% user_id,
    username = user_row$username[[1]],
    success = TRUE,
    message = "Password changed successfully"
  )

  list(success = TRUE, user = get_user_by_id(user_id))
}

bootstrap_admin_account <- function(name, username, password) {
  if (users_exist()) {
    return(list(success = FALSE, message = "An account already exists."))
  }

  password_hash <- hash_password(password)
  if (is.null(password_hash)) {
    return(list(success = FALSE, message = "Failed to create admin account."))
  }

  user_id <- rids_repos()$users$insert(
    name = trimws(name),
    username = trimws(username),
    email = NA_character_,
    password_hash = password_hash,
    role = "admin",
    active = TRUE,
    force_password_change = FALSE
  )
  touch_last_login(user_id)

  log_auth_event(
    event_type = "user_created",
    user_id = user_id,
    actor_user_id = user_id,
    username = username,
    success = TRUE,
    message = "Initial admin account created"
  )

  list(success = TRUE, user = get_user_by_id(user_id))
}

list_users_for_admin <- function() {
  rids_repos()$users$list_all()
}
