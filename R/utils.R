#' Convert a tag list to clickable Bluesky hashtag links
#'
#' @param tag_list Character vector of tags, or `NA`.
#' @return Character string of comma-separated HTML links, or `NA`.
#' @keywords internal
tags_to_links <- function(tag_list) {
  if (length(tag_list) == 1 && is.na(tag_list)) return(NA_character_)
  if (length(tag_list) == 0) return(NA_character_)

  links <- vapply(tag_list, function(tag) {
    sprintf(
      "<a href='https://bsky.app/hashtag/%s' target='_blank'>%s</a>",
      tag, tag
    )
  }, character(1))

  paste(links, collapse = ", ")
}


#' Convert AT Protocol URIs to Bluesky web URLs
#'
#' Converts `at://DID/app.bsky.feed.post/POSTID` to
#' `https://bsky.app/profile/DID/post/POSTID`. Works correctly on
#' character vectors (unlike `bskyr::bs_uri_to_url()` which only
#' converts the first element).
#'
#' @param uri Character vector of AT URIs.
#' @return Character vector of Bluesky web URLs.
#' @keywords internal
at_uri_to_url <- function(uri) {
  gsub(
    "^at://(.*?)/app\\.bsky\\.feed\\.post/(.*)$",
    "https://bsky.app/profile/\\1/post/\\2",
    uri
  )
}


#' Derive a URL-safe slug from a conference name
#'
#' Lowercases, replaces non-alphanumeric characters with underscores,
#' and collapses consecutive underscores.
#'
#' @param conference_name Character string.
#' @return Character string suitable for filenames.
#' @keywords internal
make_slug <- function(conference_name) {
  slug <- tolower(conference_name)
  slug <- gsub("[^a-z0-9]+", "_", slug)
  slug <- gsub("_+", "_", slug)
  slug <- gsub("^_|_$", "", slug)
  slug
}
