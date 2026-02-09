#' Authenticate with Bluesky
#'
#' Convenience wrapper around [bskyr::bs_auth()] that reads credentials
#' from environment variables by default. Set `BLUESKY_APP_USER` and
#' `BLUESKY_APP_PASS` in your `.Renviron` file.
#'
#' @param user Character. Bluesky user name. Defaults to the
#'   `BLUESKY_APP_USER` environment variable via [bskyr::get_bluesky_user()].
#' @param pass Character. Bluesky app password. Defaults to the
#'   `BLUESKY_APP_PASS` environment variable via [bskyr::get_bluesky_pass()].
#'
#' @return An authentication object for use with bskyr functions.
#' @export
#'
#' @examples
#' \dontrun{
#' auth <- bs_conference_auth()
#' }
bs_conference_auth <- function(user = bskyr::get_bluesky_user(),
                               pass = bskyr::get_bluesky_pass()) {
  if (is.null(user) || user == "") {
    cli::cli_abort(c(
      "Bluesky user name not found.",
      "i" = "Set {.envvar BLUESKY_APP_USER} in your {.file .Renviron} file."
    ))
  }
  if (is.null(pass) || pass == "") {
    cli::cli_abort(c(
      "Bluesky app password not found.",
      "i" = "Set {.envvar BLUESKY_APP_PASS} in your {.file .Renviron} file.",
      "i" = "Create an app password at {.url https://bsky.app/settings/app-passwords}."
    ))

  }

  bskyr::bs_auth(user = user, pass = pass)
}
