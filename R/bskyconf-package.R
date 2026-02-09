#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom data.table setDT setindex uniqueN fwrite `:=`
#' @importFrom rlang `%||%`
#' @importFrom stats setNames
## usethis namespace: end
NULL

# Suppress R CMD check notes for data.table NSE column names
utils::globalVariables(c(

  # Column names used in data.table/dplyr expressions
  "CreatedDate", "Author", "HasExternalURLs", "Post", "URI", "TimePulled",
  "CreatedAt", "By", "NoBots", "Protected", "Account",
  "Likes", "Reposts", "Replies", "Text", "Name", "AllTags",
  "Tags", "ExternalURLs", "Tags_String",
  "..cols_to_keep",

  # Column names from bskyr that dplyr::transmute() references
  "post_text", "author_handle", "author_display_name", "indexed_at",
  "like_count", "repost_count", "reply_count", "uri",
  "external_urls", "tags", "post_url"
))
