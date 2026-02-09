#' Read a conference configuration file
#'
#' Reads a YAML config file (`_bskyconf.yml`) and returns a named list
#' with all conference settings.
#'
#' @param path Character. Path to the YAML config file.
#'   Default: `"_bskyconf.yml"`.
#'
#' @return A named list with configuration values including:
#'   `conference_name`, `conference_slug`, `hashtags`, `data_dir`,
#'   `posts_per_hashtag`, `min_date`, `exclude_accounts`,
#'   `exclude_accounts_regex`, `about_text`, `update_text`.
#' @export
#'
#' @examples
#' \dontrun{
#' config <- read_conference_config("_bskyconf.yml")
#' config$hashtags
#' }
read_conference_config <- function(path = "_bskyconf.yml") {
  if (!file.exists(path)) {
    cli::cli_abort("Config file not found: {.path {path}}")
  }

  config <- yaml::read_yaml(path)

  # Apply defaults for optional fields
  config$posts_per_hashtag <- config$posts_per_hashtag %||% 250L
  config$data_dir <- config$data_dir %||% "data"
  config$exclude_accounts <- config$exclude_accounts %||% character(0)

  config
}


#' Write a conference configuration file
#'
#' Creates a YAML config file with the given conference parameters.
#'
#' @param path Character. Path to write the config file.
#' @param conference_name Character. Display name for the conference.
#' @param conference_slug Character. Short name with no spaces.
#' @param hashtags Character vector. Hashtags to track.
#' @param data_dir Character. Relative path for data storage.
#' @param posts_per_hashtag Integer. Posts to fetch per hashtag.
#' @param min_date Date or character. Earliest date for posts.
#' @param exclude_accounts Character vector. Accounts to exclude.
#' @param exclude_accounts_regex Character or `NULL`. Regex for exclusion.
#' @param about_text Character. Description for the FAQ modal.
#' @param update_text Character. Update frequency description.
#'
#' @return The config list, invisibly.
#' @keywords internal
write_conference_config <- function(
    path,
    conference_name,
    conference_slug,
    hashtags,
    data_dir = "data",
    posts_per_hashtag = 250L,
    min_date = NULL,
    exclude_accounts = character(0),
    exclude_accounts_regex = NULL,
    about_text = NULL,
    update_text = NULL
) {
  config <- list(
    conference_name = conference_name,
    conference_slug = conference_slug,
    hashtags = as.list(hashtags),
    data_dir = data_dir,
    posts_per_hashtag = posts_per_hashtag,
    min_date = if (!is.null(min_date)) as.character(min_date) else NULL,
    exclude_accounts = if (length(exclude_accounts) > 0) {
      as.list(exclude_accounts)
    },
    exclude_accounts_regex = exclude_accounts_regex,
    about_text = about_text %||% paste(
      "This app displays posts on Bluesky with tags related to",
      conference_name
    ),
    update_text = update_text %||% paste(
      "The data is re-pulled periodically.",
      "During the conference, updates happen every 30 to 60 minutes."
    )
  )

  yaml::write_yaml(config, path)

  invisible(config)
}
