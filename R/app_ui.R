#' Build the conference app UI
#'
#' Creates the Shiny UI with sidebar filters, metric cards, and a
#' DT DataTable for browsing conference posts.
#'
#' @param conference_name Character. Conference display name.
#' @param favicon_url Character. URL for the browser tab icon.
#'
#' @return A Shiny UI object.
#' @keywords internal
conference_ui <- function(conference_name, favicon_url) {
  bslib::page_sidebar(
    theme = conference_theme(),

    shiny::tags$head(
      shiny::tags$link(rel = "shortcut icon", href = favicon_url),
      shiny::tags$link(
        rel = "stylesheet",
        href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.0.0/css/all.min.css"
      ),
      shiny::tags$style(shiny::HTML("
        body {
          overflow-y: hidden;
        }
        .main-container {
          display: flex;
          min-height: 100vh;
        }
        .sidebar {
          height: 100vh;
          overflow-y: auto;
        }
        #posts_table {
          flex-grow: 1;
          overflow-y: auto;
          display: block;
        }
        .card-body {
          flex-grow: 0;
        }
        .page-content {
          padding: 20px;
          flex: 1;
          overflow-y: auto;
        }
        .sidebar-content {
          padding: 20px;
        }
      "))
    ),

    window_title = paste(conference_name, "Bluesky posts"),

    title = shiny::div(
      style = "display: flex; flex-direction: column; justify-content: center; align-items: center; width: 100%; gap: 0.5rem;",
      shiny::div(
        style = "display: flex; justify-content: center; align-items: center; gap: 1rem;",
        shiny::div(
          style = "font-size: 24px; font-family: 'Montserrat', sans-serif; font-weight: 600;",
          shiny::uiOutput("dynamic_title")
        ),
        shiny::div(
          style = "position: absolute; right: 1rem;",
          shiny::actionButton(
            "show_faq", "FAQ",
            icon = shiny::icon("question-circle"),
            class = "btn-primary"
          )
        )
      ),
      shiny::div(
        style = "font-style: italic; font-size: 14px; color: #666; margin-bottom: 0.5rem;",
        shiny::textOutput("last_updated")
      )
    ),

    sidebar = bslib::sidebar(
      class = "sidebar-content",
      shiny::dateRangeInput(
        "date_range",
        "Select Date Range",
        start = NULL,
        end = NULL,
        format = "yyyy-mm-dd"
      ),
      shiny::checkboxInput(
        "has_url",
        "Show only posts with external URLs",
        value = FALSE
      ),
      shiny::hr(),
      shiny::div(
        style = "display: flex; flex-wrap: wrap; justify-content: flex-start; align-items: flex-start; margin-bottom: 1rem;",
        shiny::h5("Filter by Authors", style = "margin: 0 0 0.5rem 0; width: 100%;"),
        shiny::div(
          style = "display: flex; gap: 0.5rem; justify-content: flex-end;",
          shiny::actionButton("select_all", "Select All", class = "btn-sm"),
          shiny::actionButton("clear_all", "Clear All", class = "btn-sm")
        )
      ),
      shiny::div(
        class = "author-checkbox-group",
        shiny::checkboxGroupInput(
          "selected_authors",
          label = NULL,
          choices = NULL,
          selected = NULL
        )
      ),
      shiny::downloadButton(
        "download_data", "Download Current Data",
        class = "btn-primary w-100"
      ),
      shiny::br()
    ),

    shiny::div(
      class = "main-container",
      shiny::div(
        class = "page-content",
        shiny::div(
          style = "display: flex; gap: 1.5rem; margin-bottom: 1.5rem;",
          # Total Posts Card
          shiny::div(
            class = "metric-card",
            style = "background: linear-gradient(135deg, #3498db, #2980b9);",
            shiny::tags$i(class = "fas fa-newspaper metric-icon"),
            shiny::h6("Total Posts", class = "metric-title"),
            shiny::div(class = "metric-value", shiny::textOutput("total_posts"))
          ),
          # Unique Authors Card
          shiny::div(
            class = "metric-card",
            style = "background: linear-gradient(135deg, #2ecc71, #27ae60);",
            shiny::tags$i(class = "fas fa-users metric-icon"),
            shiny::h6("Unique Authors", class = "metric-title"),
            shiny::div(class = "metric-value", shiny::textOutput("unique_authors"))
          ),
          # Total Likes Card
          shiny::div(
            class = "metric-card",
            style = "background: linear-gradient(135deg, #9b59b6, #8e44ad);",
            shiny::tags$i(class = "fas fa-heart metric-icon"),
            shiny::h6("Total Likes", class = "metric-title"),
            shiny::div(class = "metric-value", shiny::textOutput("total_likes"))
          )
        ),
        bslib::card(
          bslib::card_header(
            "Browse and search posts (regular expressions work). Click >> at end of text to view original post on Bluesky."
          ),
          shiny::uiOutput("table_ui")
        )
      )
    )
  )
}
