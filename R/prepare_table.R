#' Prepare posts for display in the Shiny app
#'
#' Transforms raw post data into the format expected by the DT table,
#' including formatting dates, creating clickable tag links, and adding
#' data.table indexes for fast filtering.
#'
#' @param posts Data frame of raw posts (as stored in the archive file).
#' @param min_date Date or `NULL`. If provided, only include posts on or
#'   after this date.
#'
#' @return A [data.table::data.table] ready for the Shiny app, with columns:
#'   CreatedAt, Text, Author, Likes, Reposts, Replies, Tags, AllTags,
#'   HasExternalURLs, CreatedDate. Indexed on CreatedDate and Author.
#' @keywords internal
prepare_display_table <- function(posts, min_date = NULL) {
  table_posts <- posts

  if (!is.null(min_date)) {
    table_posts <- table_posts |>
      dplyr::filter(as.Date(CreatedAt) >= min_date)
  }

  table_posts <- table_posts |>
    dplyr::mutate(
      CreatedDate = as.Date(CreatedAt),
      HasExternalURLs = !is.na(ExternalURLs) |
        stringr::str_detect(
          Post,
          "https?://\\S+|\\b\\S+\\.(com|org|net|io|gov|edu|cloud)/\\S+"
        ),
      CreatedAt = format(CreatedAt, "%Y-%m-%d %H:%MZ")
    ) |>
    dplyr::select(
      CreatedAt, Text, Author = Name, Likes, Reposts, Replies,
      AllTags = Tags, HasExternalURLs, CreatedDate
    )

  # Convert tag lists to clickable hashtag links
  table_posts$Tags <- vapply(
    table_posts$AllTags,
    tags_to_links,
    character(1)
  )

  # Convert to data.table and add indexes for fast filtering
  data.table::setDT(table_posts)
  data.table::setindex(table_posts, CreatedDate)
  data.table::setindex(table_posts, Author)

  table_posts
}
