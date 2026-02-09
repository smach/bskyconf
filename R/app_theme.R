#' Create the bslib theme for the conference app
#'
#' Defines the Bootstrap 5 theme with blue gradient metric cards,
#' responsive layout, and Inter/Montserrat fonts.
#'
#' @return A [bslib::bs_theme] object.
#' @keywords internal
conference_theme <- function() {
  bslib::bs_theme(
    version = 5,
    preset = NULL,
    bg = "#f8f9fa",
    fg = "#333",
    primary = "#2980b9",
    base_font = bslib::font_google("Inter"),
    heading_font = bslib::font_google("Montserrat"),
    font_scale = 0.9
  ) |>
    bslib::bs_add_rules(
      "
      /* Card styling */
      .card {
        border: none;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        transition: box-shadow 0.3s ease;
      }
      .card:hover {
        box-shadow: 0 4px 8px rgba(0,0,0,0.15);
      }
      .card-header {
        background-color: white;
        border-bottom: 2px solid #e9ecef;
        font-family: 'Montserrat', sans-serif;
        font-weight: 600;
      }

      /* Table styling */
      .dataTables_wrapper {
        padding: 1rem;
      }
      .dataTable thead th {
        background-color: #f8f9fa;
        font-weight: 600;
      }
      .dataTable tbody tr:hover {
        background-color: #f1f5f9 !important;
      }

      /* Sidebar styling */
      .sidebar {
        background: linear-gradient(180deg, #ffffff 0%, #f8f9fa 100%);
        border-right: none;
        box-shadow: 2px 0 5px rgba(0,0,0,0.05);
      }

      /* Date range input styling */
      .input-group {
        box-shadow: 0 1px 3px rgba(0,0,0,0.1);
        border-radius: 6px;
      }

      /* Search and filter inputs styling */
      .dataTables_filter input,
      .filterrow input {
        border-radius: 4px !important;
        border: 1px solid #dee2e6 !important;
        padding: 0.375rem 0.75rem !important;
        transition: border-color 0.15s ease-in-out, box-shadow 0.15s ease-in-out;
      }
      .dataTables_filter input:focus,
      .filterrow input:focus {
        border-color: #2980b9 !important;
        box-shadow: 0 0 0 0.2rem rgba(41, 128, 185, 0.25) !important;
      }

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

      .author-checkbox-group {
        padding-right: 10px;
        margin-top: 1rem;
      }

      hr {
        margin-top: 0.5rem;
        margin-bottom: 0.5rem;
      }

      /* Metric card styling */
      .metric-card {
        flex: 1;
        padding: 1.5rem;
        background: linear-gradient(135deg, #3498db, #2980b9);
        border-radius: 0.75rem;
        text-align: center;
        color: white;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
      }
      .metric-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 8px rgba(0, 0, 0, 0.15);
      }
      .metric-title {
        margin: 0;
        color: rgba(255, 255, 255, 0.9);
        font-size: 1rem;
        font-weight: 500;
      }
      .metric-value {
        font-size: 2rem;
        font-weight: bold;
        margin-top: 0.5rem;
      }
      .metric-icon {
        font-size: 1.5rem;
        margin-bottom: 0.5rem;
        opacity: 0.9;
      }

      /* Processing Message Styling */
      .processing-message {
        text-align: center;
        padding: 20px;
        font-style: italic;
        color: #888;
      }
      "
    )
}
