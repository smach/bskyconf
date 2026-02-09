#' Launch the Bluesky conference posts Shiny app
#'
#' Starts a Shiny app that displays conference posts in a searchable,
#' filterable table with metric cards and download capability.
#'
#' @param conference_name Character. Display name for the conference
#'   (e.g. `"posit::conf(2025)"`). Used in the page title.
#' @param data_file Character. Path to the display-ready RDS file
#'   (the `table_posts.Rds` produced by [update_conference_posts()]).
#' @param conference_slug Character. Short name with no spaces, used
#'   for download filenames (e.g. `"posit_conf_2025"`). If `NULL`,
#'   derived automatically from `conference_name`.
#' @param about_text A [shiny::tags] object or character string. Text
#'   for the "About" section in the FAQ modal. Default provides a
#'   generic description.
#' @param update_text A [shiny::tags] object or character string. Text
#'   for the "How often updated" section in the FAQ modal. Default
#'   provides a generic description.
#' @param refresh_interval Integer. Milliseconds between data file
#'   re-reads. Default: 900000 (15 minutes).
#' @param favicon_url Character. URL for the browser tab icon.
#' @param ... Additional arguments passed to [shiny::shinyApp()].
#'
#' @return A Shiny app object.
#' @export
#'
#' @examples
#' \dontrun{
#' run_conference_app(
#'   conference_name = "posit::conf(2025)",
#'   data_file = "data/table_posts.Rds"
#' )
#' }
run_conference_app <- function(
    conference_name,
    data_file,
    conference_slug = NULL,
    about_text = NULL,
    update_text = NULL,
    refresh_interval = 900000L,
    favicon_url = "https://apps.machlis.com/shiny/images/bsky.ico",
    ...
) {
  if (!file.exists(data_file)) {
    cli::cli_abort(c(
      "Data file not found: {.path {data_file}}",
      "i" = "Run {.fn update_conference_posts} first to create the data file."
    ))
  }

  conference_slug <- conference_slug %||% make_slug(conference_name)

  about_text <- about_text %||% paste(
    "This app displays posts on Bluesky with tags related to",
    conference_name
  )
  update_text <- update_text %||% paste(
    "The data is re-pulled periodically.",
    "During the conference, updates happen every 30 to 60 minutes."
  )

  ui <- conference_ui(
    conference_name = conference_name,
    favicon_url = favicon_url
  )

  server <- conference_server(
    data_file = data_file,
    conference_name = conference_name,
    conference_slug = conference_slug,
    about_text = about_text,
    update_text = update_text,
    refresh_interval = refresh_interval
  )

  shiny::shinyApp(ui = ui, server = server, ...)
}
