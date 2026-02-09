#' Conference app server function factory
#'
#' Returns a Shiny server function with configuration baked in via closure.
#'
#' @param data_file Character. Path to the display-ready RDS file.
#' @param conference_name Character. Conference display name.
#' @param conference_slug Character. Slug for download filenames.
#' @param about_text A [shiny::tags] object or character string.
#' @param update_text A [shiny::tags] object or character string.
#' @param refresh_interval Integer. Data re-read interval in milliseconds.
#'
#' @return A Shiny server function.
#' @keywords internal
conference_server <- function(
    data_file,
    conference_name,
    conference_slug,
    about_text,
    update_text,
    refresh_interval
) {
  # Ensure text is wrapped in tags if character
  if (is.character(about_text)) {
    about_text <- shiny::tags$p(about_text, class = "faq-answer")
  }
  if (is.character(update_text)) {
    update_text <- shiny::tags$p(update_text, class = "faq-answer")
  }

  function(input, output, session) {

    table_posts <- shiny::reactiveFileReader(
      intervalMillis = refresh_interval,
      session = session,
      filePath = data_file,
      readFunc = readRDS
    )

    # Author checkbox state
    last_authors <- shiny::reactiveVal(NULL)

    shiny::observe({
      df <- data.table::data.table(table_posts())
      shiny::req(nrow(df) > 0)

      if (!is.null(input$date_range[1])) {
        df <- df[CreatedDate >= input$date_range[1] & CreatedDate <= input$date_range[2]]
      }

      authors <- sort(unique(df$Author))

      if (!identical(authors, last_authors())) {
        last_authors(authors)
        if (is.null(input$selected_authors)) {
          shiny::updateCheckboxGroupInput(
            session, "selected_authors",
            choices = authors,
            selected = authors
          )
        } else {
          shiny::updateCheckboxGroupInput(
            session, "selected_authors",
            choices = authors
          )
        }
      }
    })

    shiny::observeEvent(input$select_all, {
      df <- data.table::data.table(table_posts())
      authors <- df[, sort(unique(Author))]
      shiny::updateCheckboxGroupInput(
        session, "selected_authors",
        selected = authors
      )
    })

    shiny::observeEvent(input$clear_all, {
      shiny::updateCheckboxGroupInput(
        session, "selected_authors",
        selected = character(0)
      )
    })

    # Date range update
    shiny::observe({
      df <- data.table::data.table(table_posts())
      shiny::updateDateRangeInput(
        session, "date_range",
        start = min(df$CreatedAt),
        end = max(df$CreatedAt)
      )
    })

    # Dynamic title
    output$dynamic_title <- shiny::renderUI({
      start_date <- format(input$date_range[1], "%b. %d")
      end_date <- format(input$date_range[2], "%b. %d")
      sprintf(
        "%s Posts on Bluesky from %s to %s",
        conference_name, start_date, end_date
      )
    })

    # Filtered data
    filtered_data <- shiny::reactive({
      shiny::req(table_posts())
      df <- data.table::data.table(table_posts())

      df <- df[CreatedDate >= input$date_range[1] & CreatedDate <= input$date_range[2]]

      if (input$has_url) {
        df <- df[HasExternalURLs == TRUE]
      }

      if (!is.null(input$selected_authors) && length(input$selected_authors) > 0) {
        df <- df[Author %in% input$selected_authors]
      }

      df
    })

    # Metric outputs
    output$total_posts <- shiny::renderText({
      nrow(filtered_data())
    })

    output$unique_authors <- shiny::renderText({
      data.table::uniqueN(filtered_data()$Author)
    })

    output$total_likes <- shiny::renderText({
      sum(filtered_data()$Likes, na.rm = TRUE)
    })

    # Table UI
    output$table_ui <- shiny::renderUI({
      if (is.null(table_posts())) {
        shiny::p("Processing . . . .", class = "processing-message")
      } else {
        DT::dataTableOutput("posts_table")
      }
    })

    # Data table (server-side)
    output$posts_table <- DT::renderDataTable({
      shiny::req(filtered_data())

      hide_cols <- which(
        names(filtered_data()) %in% c("AllTags", "HasExternalURLs", "CreatedDate")
      ) - 1
      created_at_col <- which(names(filtered_data()) == "CreatedAt") - 1

      DT::datatable(
        filtered_data(),
        rownames = FALSE,
        escape = FALSE,
        filter = "top",
        options = list(
          pageLength = 25,
          autoWidth = TRUE,
          search = list(regex = TRUE),
          searchHighlight = TRUE,
          lengthMenu = c(25, 50, 75, 100, 250),
          order = list(list(created_at_col, "desc")),
          columnDefs = list(
            list(targets = hide_cols, visible = FALSE),
            list(targets = 3, orderSequence = c("desc", "asc")),
            list(targets = 4, orderSequence = c("desc", "asc")),
            list(targets = 5, orderSequence = c("desc", "asc"))
          ),
          dom = "lfrtip",
          deferRender = TRUE
        )
      )
    }, server = TRUE)

    # Download handler
    output$download_data <- shiny::downloadHandler(
      filename = function() {
        paste0(conference_slug, "_posts_", format(Sys.Date(), "%Y%m%d"), ".csv")
      },
      content = function(file) {
        df <- filtered_data()
        cols_to_keep <- setdiff(
          names(df),
          c("AllTags", "HasExternalURLs", "CreatedDate")
        )
        df_to_save <- df[, ..cols_to_keep]
        data.table::fwrite(df_to_save, file)
      }
    )

    # FAQ modal
    shiny::observeEvent(input$show_faq, {
      shiny::showModal(shiny::modalDialog(
        title = "Frequently Asked Questions",
        shiny::div(
          shiny::h4("About this App", class = "faq-question"),
          about_text,
          shiny::h4("How often is the data updated?", class = "faq-question"),
          update_text,
          shiny::h4("Can I download the data?", class = "faq-question"),
          shiny::p(
            "Yes! You can download the currently filtered data using the download button in the sidebar.",
            class = "faq-answer"
          ),
          shiny::h4("Can I see the R code?", class = "faq-question"),
          shiny::p(
            "This app was built with the ",
            shiny::tags$a(
              href = "https://github.com/smach/bskyconf",
              "bskyconf R package",
              target = "_blank"
            ),
            ".",
            class = "faq-answer"
          )
        ),
        size = "l",
        easyClose = TRUE,
        footer = shiny::modalButton("Close")
      ))
    })

    # Last updated timestamp
    output$last_updated <- shiny::renderText({
      file_info <- file.info(data_file)
      paste("Last updated", format(file_info$mtime, "%b. %d, %Y %H:%M UTC"))
    })
  }
}
