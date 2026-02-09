#' Fetch Bluesky posts for a single hashtag
#'
#' Searches Bluesky for posts matching a hashtag and returns a tidy data frame.
#' Uses [bskyr::bs_search_posts()] to query the API, then extracts post text,
#' external URLs, and tags from the nested list columns.
#'
#' @param hashtag Character. The hashtag to search for (with or without `#`).
#' @param limit Integer. Maximum number of posts to retrieve. Default: 250.
#' @param auth Authentication object from [bs_conference_auth()] or
#'   [bskyr::bs_auth()].
#'
#' @return A tibble with columns: Post, By, Name, CreatedAt, Likes, Reposts,
#'   Replies, URI, ExternalURLs, Tags, PostID, URL, Text, TimePulled,
#'   Tags_String.
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- bs_conference_auth()
#' posts <- fetch_hashtag_posts("#rstats", limit = 50, auth = auth)
#' }
fetch_hashtag_posts <- function(hashtag, limit = 250L, auth = bs_conference_auth()) {
  hashtag_clean <- gsub("#", "", hashtag, fixed = TRUE)

  mydata <- bskyr::bs_search_posts(
    query = hashtag,
    limit = limit,
    auth  = auth,
    sort  = "latest"
  )

  if (nrow(mydata) == 0) {
    return(empty_posts_tibble())
  }

  # Resolve bskyr column names, which vary between versions.
  col_names <- names(mydata)
  col_time   <- find_column(col_names, c("indexed_at"))
  col_likes  <- find_column(col_names, c("like_count"))
  col_repost <- find_column(col_names, c("repost_count"))
  col_reply  <- find_column(col_names, c("reply_count"))
  col_uri    <- find_column(col_names, c("uri"))

  # Extract author handle and display name.
  # bskyr may return these as flat columns (author_handle) or as a nested

  # list column ("author") containing tibbles with handle/display_name.
  author_info <- extract_author_info(mydata, col_names)

  # Extract post text from the record list column
  post_text <- purrr::map_chr(mydata$record, ~ {
    txt <- purrr::pluck(.x, "text", .default = NA_character_)
    if (is.null(txt)) NA_character_ else txt
  })

  # Extract external URLs from the embed list column.
  # The embed structure varies: some have external_uri (flat), some have
  # nested external$uri, some are images or NULL. When no posts in the
  # result set have embeds, bskyr omits the column entirely.
  if ("embed" %in% col_names) {
    external_urls <- purrr::map_chr(mydata$embed, extract_external_url)
  } else {
    external_urls <- rep(NA_character_, nrow(mydata))
  }

  # Extract tags from the record list column.
  # Tags may be in record$tags (top-level field) or embedded in
  # record$facets as richtext facet features of type "tag".
  tag_list <- purrr::map(mydata$record, extract_tags)

  # Build Bluesky post URLs from AT URIs.
  # Note: bskyr::bs_uri_to_url() does not vectorize correctly (returns the
  # same URL for all inputs), so we convert manually via regex.
  uri_vec <- mydata[[col_uri]]
  post_urls <- at_uri_to_url(uri_vec)

  # Build output tibble with standardised column names
  mydata_wrangled <- dplyr::tibble(
    Post         = post_text,
    By           = author_info$handle,
    Name         = author_info$display_name,
    CreatedAt    = as.POSIXct(mydata[[col_time]], format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"),
    Likes        = mydata[[col_likes]],
    Reposts      = mydata[[col_repost]],
    Replies      = mydata[[col_reply]],
    URI          = uri_vec,
    ExternalURLs = external_urls,
    Tags         = tag_list,
    PostID       = stringr::str_replace(uri_vec, "^.*/(\\w+)$", "\\1"),
    URL          = post_urls,
    Text         = paste0(
      post_text,
      " <a target='_blank' href='", post_urls, "'> <strong> >> </strong></a>"
    ),
    TimePulled   = Sys.time()
  )

  # Collapse tag lists into comma-separated strings
  mydata_wrangled$Tags_String <- purrr::map_chr(mydata_wrangled$Tags, ~ {
    if (length(.x) == 0) NA_character_ else paste(.x, collapse = ", ")
  })

  mydata_wrangled
}


#' Find a column name from a list of candidates
#'
#' Returns the first candidate that exists in `col_names`. Supports
#' partial matching as a fallback (e.g., "handle" matches "author_handle").
#'
#' @param col_names Character vector of actual column names.
#' @param candidates Character vector of names to try, in priority order.
#' @return The matched column name.
#' @keywords internal
find_column <- function(col_names, candidates) {
  # Exact match first
  for (cand in candidates) {
    if (cand %in% col_names) return(cand)
  }
  # Partial match fallback (candidate appears at end of column name)
  for (cand in candidates) {
    matches <- grep(paste0(cand, "$"), col_names, value = TRUE)
    if (length(matches) > 0) return(matches[1])
  }
  cli::cli_abort(c(
    "Could not find a column matching {.or {.val {candidates}}}.",
    "i" = "Available columns: {.val {col_names}}",
    "i" = "bskyr column names may have changed. Please report this issue."
  ))
}


#' Extract author handle and display name from bskyr results
#'
#' Handles both flat columns (author_handle, author_display_name) and
#' nested list column ("author") containing tibbles.
#'
#' @param mydata The tibble returned by `bskyr::bs_search_posts()`.
#' @param col_names Character vector of column names.
#' @return A list with `handle` and `display_name` character vectors.
#' @keywords internal
extract_author_info <- function(mydata, col_names) {
  # Try flat columns first (older bskyr versions)
  has_flat_handle <- any(c("author_handle", "handle") %in% col_names)
  has_flat_display <- any(c("author_display_name", "display_name") %in% col_names)

  if (has_flat_handle && has_flat_display) {
    col_handle <- find_column(col_names, c("author_handle", "handle"))
    col_display <- find_column(col_names, c("author_display_name", "display_name"))
    return(list(
      handle = mydata[[col_handle]],
      display_name = mydata[[col_display]]
    ))
  }

  # Nested "author" list column (bskyr >= 0.4.0)
  if ("author" %in% col_names && is.list(mydata$author)) {
    handle <- purrr::map_chr(mydata$author, ~ {
      val <- purrr::pluck(.x, "handle", .default = NA_character_)
      if (is.null(val)) NA_character_ else val[1]
    })
    display_name <- purrr::map_chr(mydata$author, ~ {
      val <- purrr::pluck(.x, "display_name", .default = NA_character_)
      if (is.null(val)) NA_character_ else val[1]
    })
    return(list(handle = handle, display_name = display_name))
  }

  cli::cli_abort(c(
    "Could not extract author information from search results.",
    "i" = "Available columns: {.val {col_names}}",
    "i" = "bskyr column names may have changed. Please report this issue."
  ))
}


#' Extract external URL from an embed list element
#'
#' Handles varying embed structures: nested external$uri, flat
#' external_uri, or NULL/image-only embeds.
#'
#' @param embed_item A single element from the embed list column.
#' @return A character string (URL or `NA_character_`).
#' @keywords internal
extract_external_url <- function(embed_item) {
  if (is.null(embed_item)) return(NA_character_)

  # Flat column: external_uri (bskyr >= 0.4.0 with tibble embeds)
  uri_val <- purrr::pluck(embed_item, "external_uri", .default = NULL)
  if (!is.null(uri_val) && !all(is.na(uri_val))) {
    return(paste(uri_val[!is.na(uri_val)], collapse = ", "))
  }

  # Nested list: external$uri (older bskyr or raw list embeds)
  uri_val <- purrr::pluck(embed_item, "external", "uri", .default = NULL)
  if (!is.null(uri_val) && !all(is.na(uri_val))) {
    return(paste(uri_val[!is.na(uri_val)], collapse = ", "))
  }

  NA_character_
}


#' Extract tags from a record list element
#'
#' Tries the top-level `tags` field first, then falls back to extracting
#' tag values from richtext facets.
#'
#' @param record_item A single element from the record list column.
#' @return A character vector of tags (possibly length 0).
#' @keywords internal
extract_tags <- function(record_item) {
  if (is.null(record_item)) return(character(0))

  # Try top-level tags field first
  tag_val <- purrr::pluck(record_item, "tags", .default = NULL)
  if (!is.null(tag_val) && length(tag_val) > 0) {
    return(as.character(unlist(tag_val)))
  }

  # Fall back to extracting from facets
  facets <- purrr::pluck(record_item, "facets", .default = NULL)
  if (is.null(facets) || length(facets) == 0) return(character(0))

  tags <- character(0)
  for (facet_group in facets) {
    # facet_group may be a list of facets or a single facet
    facet_list <- if (is.list(facet_group) && !is.null(facet_group$features)) {
      list(facet_group)
    } else {
      facet_group
    }
    for (facet in facet_list) {
      features <- purrr::pluck(facet, "features", .default = list())
      for (feat in features) {
        feat_type <- purrr::pluck(feat, "$type", .default = "")
        if (grepl("tag$", feat_type)) {
          tag <- purrr::pluck(feat, "tag", .default = NULL)
          if (!is.null(tag)) tags <- c(tags, tag)
        }
      }
    }
  }

  tags
}


#' Create an empty posts tibble with the expected columns
#'
#' @return An empty tibble with the standard column structure.
#' @keywords internal
empty_posts_tibble <- function() {
  dplyr::tibble(
    Post         = character(),
    By           = character(),
    Name         = character(),
    CreatedAt    = as.POSIXct(character()),
    Likes        = integer(),
    Reposts      = integer(),
    Replies      = integer(),
    URI          = character(),
    ExternalURLs = character(),
    Tags         = list(),
    PostID       = character(),
    URL          = character(),
    Text         = character(),
    TimePulled   = as.POSIXct(character()),
    Tags_String  = character()
  )
}
