#' Update conference posts from Bluesky
#'
#' Fetches recent posts for specified hashtags, deduplicates them,
#' checks for excluded accounts (bots, protected), merges with previously
#' saved posts, and produces a display-ready data file for the Shiny app.
#'
#' This function is designed to be called from a cron job script. It reads
#' and writes data files in `data_dir`.
#'
#' @param hashtags Character vector. Hashtags to search for (e.g.
#'   `c("#PositConf2025", "#PositConf")`).
#' @param data_dir Character. Path to the directory where data files
#'   are stored. Must exist.
#' @param posts_per_hashtag Integer. Number of posts to fetch per
#'   hashtag. Default: 250.
#' @param min_date Date or `NULL`. Exclude posts before this date from
#'   the display table. Default: `NULL` (no filtering).
#' @param exclude_accounts Character vector. Account handles to always
#'   exclude. Default: `character(0)`.
#' @param exclude_accounts_regex Character or `NULL`. Regex pattern for
#'   account handles to exclude. Default: `NULL`.
#' @param raw_file Character. Filename for the raw deduplicated posts
#'   archive within `data_dir`. Default: `"previous_retrieved_posts.Rds"`.
#' @param display_file Character. Filename for the display-ready table
#'   within `data_dir`. Default: `"table_posts.Rds"`.
#' @param auth Authentication object. Default: [bs_conference_auth()].
#' @param verbose Logical. Print progress messages? Default: `TRUE`.
#'
#' @return Invisibly returns the display-ready [data.table::data.table].
#' @export
#'
#' @examples
#' \dontrun{
#' update_conference_posts(
#'   hashtags = c("#NICAR26", "#NICAR2026"),
#'   data_dir = "path/to/data",
#'   min_date = as.Date("2026-01-01")
#' )
#' }
update_conference_posts <- function(
    hashtags,
    data_dir,
    posts_per_hashtag = 250L,
    min_date = NULL,
    exclude_accounts = character(0),
    exclude_accounts_regex = NULL,
    raw_file = "previous_retrieved_posts.Rds",
    display_file = "table_posts.Rds",
    auth = bs_conference_auth(),
    verbose = TRUE
) {
  if (!dir.exists(data_dir)) {
    cli::cli_abort("Data directory does not exist: {.path {data_dir}}")
  }

  raw_path <- file.path(data_dir, raw_file)
  display_path <- file.path(data_dir, display_file)

  # ---- Fetch posts for all hashtags ----
  if (verbose) cli::cli_inform("Fetching posts for {length(hashtags)} hashtag{?s}...")

  all_recent_posts <- purrr::map(hashtags, function(ht) {
    if (verbose) cli::cli_inform("  Searching {.val {ht}}...")
    fetch_hashtag_posts(hashtag = ht, limit = posts_per_hashtag, auth = auth)
  }) |>
    dplyr::bind_rows()

  if (nrow(all_recent_posts) == 0) {
    cli::cli_warn("No posts found for the specified hashtags.")
    return(invisible(NULL))
  }

  if (verbose) cli::cli_inform("Found {nrow(all_recent_posts)} post{?s} total.")

  # ---- Check accounts for exclusion ----
  if (verbose) cli::cli_inform("Checking accounts for exclusion criteria...")

  auto_exclude <- check_accounts_for_exclusion(
    accounts = unique(all_recent_posts$By),
    cache_dir = data_dir,
    auth = auth
  )

  all_exclude <- unique(c(exclude_accounts, auto_exclude))
  all_exclude <- all_exclude[nzchar(all_exclude)]

  # ---- Deduplicate and filter ----
  deduped <- all_recent_posts |>
    dplyr::distinct(URI, .keep_all = TRUE) |>
    dplyr::filter(!(By %in% all_exclude))

  if (!is.null(exclude_accounts_regex) && any(nzchar(exclude_accounts_regex))) {
    regex_pattern <- paste(exclude_accounts_regex, collapse = "|")
    deduped <- deduped |>
      dplyr::filter(!stringr::str_detect(By, regex_pattern))
  }

  if (verbose) cli::cli_inform("{nrow(deduped)} unique post{?s} after filtering.")

  # ---- Merge with archive ----
  if (file.exists(raw_path)) {
    previous <- readRDS(raw_path)
    combined <- dplyr::bind_rows(previous, deduped) |>
      dplyr::group_by(URI) |>
      dplyr::arrange(dplyr::desc(TimePulled)) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::arrange(dplyr::desc(CreatedAt))
  } else {
    combined <- deduped
  }

  saveRDS(combined, raw_path)
  if (verbose) cli::cli_inform("Archive saved: {nrow(combined)} total post{?s}.")

  # ---- Create display table ----
  display_table <- prepare_display_table(combined, min_date = min_date)
  saveRDS(display_table, display_path)

  if (verbose) {
    cli::cli_inform("Display table saved: {nrow(display_table)} post{?s} for app.")
  }

  invisible(display_table)
}
