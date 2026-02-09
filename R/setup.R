#' Set up a new conference directory
#'
#' Creates a directory structure with template files for a new conference,
#' ready for Shiny Server deployment and cron job updates.
#'
#' @param path Character. Directory path to create. Must not already exist.
#' @param conference_name Character. Display name (e.g. `"NICAR 2026"`).
#' @param hashtags Character vector. Hashtags to track
#'   (e.g. `c("#NICAR26", "#NICAR2026")`).
#' @param min_date Date. Earliest date for posts. Default: January 1 of
#'   the current year.
#' @param posts_per_hashtag Integer. Posts to fetch per hashtag. Default: 250.
#' @param exclude_accounts Character vector. Account handles to always
#'   exclude. Default: `character(0)`.
#' @param exclude_accounts_regex Character or `NULL`. Regex pattern for
#'   account exclusion. Default: `NULL`.
#'
#' @return The path, invisibly. Called for side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' setup_conference(
#'   path = "~/my_conferences/nicar2026",
#'   conference_name = "NICAR 2026",
#'   hashtags = c("#NICAR26", "#NICAR2026"),
#'   min_date = as.Date("2026-01-01")
#' )
#' }
setup_conference <- function(
    path,
    conference_name,
    hashtags,
    min_date = as.Date(paste0(format(Sys.Date(), "%Y"), "-01-01")),
    posts_per_hashtag = 250L,
    exclude_accounts = character(0),
    exclude_accounts_regex = NULL
) {
  if (dir.exists(path)) {
    cli::cli_abort("Directory already exists: {.path {path}}")
  }

  slug <- make_slug(conference_name)

  # Create directory structure
  dir.create(path, recursive = TRUE)
  dir.create(file.path(path, "data"), recursive = TRUE)

  # Write config file
  write_conference_config(
    path = file.path(path, "_bskyconf.yml"),
    conference_name = conference_name,
    conference_slug = slug,
    hashtags = hashtags,
    data_dir = "data",
    posts_per_hashtag = posts_per_hashtag,
    min_date = min_date,
    exclude_accounts = exclude_accounts,
    exclude_accounts_regex = exclude_accounts_regex
  )

  # Copy template files
  template_dir <- system.file("templates", package = "bskyconf")

  file.copy(
    file.path(template_dir, "app.R"),
    file.path(path, "app.R")
  )
  file.copy(
    file.path(template_dir, "update_posts.R"),
    file.path(path, "update_posts.R")
  )

  cli::cli_inform(c(
    "v" = "Conference directory created at {.path {path}}",
    "*" = "Files created:",
    " " = "  {.file _bskyconf.yml} - conference configuration",
    " " = "  {.file app.R} - Shiny Server entry point",
    " " = "  {.file update_posts.R} - cron job script",
    " " = "  {.file data/} - data directory",
    "",
    "i" = "Next steps:",
    " " = "1. Review {.file _bskyconf.yml} and adjust settings",
    " " = "2. Run {.code Rscript update_posts.R} to fetch initial data",
    " " = "3. Deploy {.file app.R} to Shiny Server or run locally"
  ))

  invisible(path)
}
