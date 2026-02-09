#' Check a single Bluesky account for exclusion criteria
#'
#' Queries the profile and checks whether the bio contains `#nobot` or
#' `#nobots`, or the account has a "no-unauthenticated" label (protected).
#'
#' @param acct Character. A Bluesky handle (e.g. "someone.bsky.social").
#' @param auth Authentication object.
#'
#' @return A one-row tibble with columns: Account, NoBots (logical),
#'   Protected (logical).
#' @keywords internal
check_account <- function(acct, auth) {
  user_info <- tryCatch(
    bskyr::bs_get_profile(acct, auth = auth),
    error = function(e) NULL
  )

  if (is.null(user_info) || nrow(user_info) == 0) {
    return(dplyr::tibble(Account = acct, NoBots = FALSE, Protected = FALSE))
  }

  # Check bio for #nobot / #nobots
  bio <- tolower(user_info$description %||% "")
  no_bots <- grepl("#nobots?\\b", bio)

  # Check labels for no-unauthenticated (protected accounts)
  protected <- FALSE

  if ("labels_val" %in% names(user_info)) {
    vals <- user_info$labels_val
    if (!all(is.na(vals))) {
      protected <- any(
        !is.na(vals) & vals %in% c("!no-unauthenticated", "no-unauthenticated")
      )
    }
  } else if ("labels" %in% names(user_info)) {
    lbl <- user_info$labels[[1]]
    if (!is.null(lbl) && length(lbl) > 0) {
      vals <- tryCatch(
        unlist(purrr::map(lbl, ~ purrr::pluck(.x, "val", .default = NA_character_))),
        error = function(e) character()
      )
      protected <- any(
        !is.na(vals) & vals %in% c("!no-unauthenticated", "no-unauthenticated")
      )
    }
  }

  dplyr::tibble(Account = acct, NoBots = no_bots, Protected = protected)
}


#' Check multiple accounts and return handles to exclude
#'
#' Checks each account for `#nobot`/`#nobots` in bio and protected status.
#' Caches results to avoid re-checking known accounts on subsequent runs.
#'
#' @param accounts Character vector. Bluesky handles to check.
#' @param cache_dir Character. Directory to store/read account check caches.
#' @param auth Authentication object.
#'
#' @return Character vector of account handles to exclude.
#' @keywords internal
check_accounts_for_exclusion <- function(accounts, cache_dir, auth) {
  accounts_file <- file.path(cache_dir, "existing_posting_accounts.Rds")
  checks_file <- file.path(cache_dir, "account_checks.Rds")

  all_accounts <- sort(unique(accounts))

  # Load previously seen accounts

  if (file.exists(accounts_file)) {
    existing_accounts <- readRDS(accounts_file)
  } else {
    existing_accounts <- character(0)
  }

  # Load previous check results
  if (file.exists(checks_file)) {
    checks_df <- readRDS(checks_file)
  } else {
    checks_df <- NULL
  }

  # Only check accounts we haven't seen before
  new_accounts <- setdiff(all_accounts, existing_accounts)

  if (length(new_accounts) > 0) {
    new_checks <- purrr::map(new_accounts, check_account, auth = auth) |>
      dplyr::bind_rows()
    checks_df <- dplyr::bind_rows(checks_df, new_checks) |>
      dplyr::distinct(Account, .keep_all = TRUE)
    saveRDS(checks_df, checks_file)
  }

  # Update the known accounts list
  saveRDS(union(existing_accounts, all_accounts), accounts_file)

  # Return handles that should be excluded
  if (is.null(checks_df) || nrow(checks_df) == 0) {
    return(character(0))
  }

  checks_df |>
    dplyr::filter(NoBots | Protected) |>
    dplyr::distinct() |>
    dplyr::pull(Account)
}
