# Auth provider boundary.
#
# build_auth_provider() returns the complete authentication interface the UI
# layer is allowed to touch. Today there is one implementation — the local
# provider (sodium password hashing, DB-backed sessions, TOTP MFA) composed
# from R/utils/auth.r and R/auth/mfa.R. Swapping in an external provider
# (e.g. Supabase Auth) later means writing another build function with the
# same method set; no UI or service code changes.

source("R/auth/mfa.R", local = FALSE)

build_auth_provider <- function() {
  list(
    # credentials + accounts
    authenticate = authenticate_user,
    change_password = change_user_password,
    bootstrap_admin = bootstrap_admin_account,
    users_exist = users_exist,
    get_user_by_id = get_user_by_id,

    # sessions (opaque token contract shared with www/app-shell.js)
    create_session = create_auth_session,
    restore_session = restore_auth_session,
    revoke_session = revoke_auth_session,
    touch_last_login = touch_last_login,

    # MFA
    mfa_enrolled = user_mfa_enrolled,
    start_mfa_enrollment = start_mfa_enrollment,
    confirm_mfa_enrollment = confirm_mfa_enrollment,
    verify_mfa = verify_mfa_code,
    admin_reset_mfa = admin_reset_user_mfa,

    # self-service reset (MFA-gated)
    reset_password_with_mfa = reset_user_password_with_mfa
  )
}
