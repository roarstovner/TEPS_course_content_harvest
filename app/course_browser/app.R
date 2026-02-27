library(shiny)
library(bslib)
library(DT)
library(dplyr, warn.conflicts = FALSE)
library(diffobj)

# ── Global ──────────────────────────────────────────────────────────────────

courses <- load_courses()

# Pre-compute filter choices
inst_choices <- sort(unique(courses$institution_short))
names(inst_choices) <- institution_labels[inst_choices]
year_range <- range(courses$Årstall)
semester_choices <- c("Vår", "Høst", "Sommer")
status_choices <- sort(unique(courses$Status))
names(status_choices) <- status_labels[as.character(status_choices)]

# Lighter table for browse display
browse_cols <- c(
  "course_id", "institution_short", "Emnekode_raw", "Emnenavn",
  "Årstall", "Semesternavn", "Status", "url", "fulltext", "plan_content_id"
)

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  title = "Course Plan Browser",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Browse tab ──
  nav_panel(
    "Browse",
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        selectizeInput("browse_inst", "Institution",
          choices = inst_choices, multiple = TRUE,
          options = list(placeholder = "All institutions")
        ),
        sliderInput("browse_year", "Year",
          min = year_range[1], max = year_range[2],
          value = year_range, step = 1, sep = ""
        ),
        selectizeInput("browse_semester", "Semester",
          choices = semester_choices, multiple = TRUE,
          options = list(placeholder = "All semesters")
        ),
        selectizeInput("browse_status", "Status",
          choices = status_choices, multiple = TRUE,
          options = list(placeholder = "All statuses")
        ),
        checkboxInput("browse_has_text", "Only with fulltext", value = FALSE),
        actionButton("browse_clear", "Clear filters", class = "btn-sm btn-outline-secondary")
      ),

      # Main area: side-by-side table + content viewer
      layout_columns(
        col_widths = c(7, 5),
        fill = TRUE,
        DTOutput("browse_table"),
        tags$div(
          class = "browse-viewer",
          radioButtons("browse_view", NULL,
            choices = c("Plaintext" = "text", "Rendered HTML" = "html"),
            inline = TRUE
          ),
          conditionalPanel(
            "input.browse_view === 'text'",
            uiOutput("browse_fulltext")
          ),
          conditionalPanel(
            "input.browse_view === 'html'",
            uiOutput("browse_html_frame")
          )
        )
      )
    )
  ),

  # ── Diff tab ──
  nav_panel(
    "Diff",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("diff_inst", "Institution",
          choices = inst_choices, multiple = FALSE,
          options = list(placeholder = "Pick institution")
        ),
        selectizeInput("diff_code", "Course code",
          choices = NULL, multiple = FALSE,
          options = list(placeholder = "Pick course code")
        ),
        tags$hr(),
        tags$strong("Versions"),
        DTOutput("diff_versions_table", height = "auto"),
        tags$hr(),
        radioButtons("diff_mode", "Diff content",
          choices = c("Normalized text" = "normalized", "Raw fulltext" = "raw"),
          selected = "normalized"
        ),
        radioButtons("diff_layout", "Layout",
          choices = c("Side by side" = "sidebyside", "Unified" = "unified"),
          selected = "sidebyside"
        )
      ),
      uiOutput("diff_output")
    )
  )
)

# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Cache for on-demand HTML loading (one institution at a time)
  html_cache <- reactiveValues(inst = NULL, data = NULL)

  # ── Browse: clear filters ──
  observeEvent(input$browse_clear, {
    updateSelectizeInput(session, "browse_inst", selected = character(0))
    updateSliderInput(session, "browse_year", value = year_range)
    updateSelectizeInput(session, "browse_semester", selected = character(0))
    updateSelectizeInput(session, "browse_status", selected = character(0))
    updateCheckboxInput(session, "browse_has_text", value = FALSE)
  })

  # ── Browse: filtered data ──
  browse_filtered <- reactive({
    df <- courses[, browse_cols]

    if (length(input$browse_inst) > 0)
      df <- df[df$institution_short %in% input$browse_inst, ]

    df <- df[df$Årstall >= input$browse_year[1] & df$Årstall <= input$browse_year[2], ]

    if (length(input$browse_semester) > 0)
      df <- df[df$Semesternavn %in% input$browse_semester, ]

    if (length(input$browse_status) > 0)
      df <- df[df$Status %in% as.integer(input$browse_status), ]

    if (input$browse_has_text)
      df <- df[!is.na(df$fulltext) & nchar(df$fulltext) > 0, ]

    df
  })

  # ── Browse: DataTable ──
  output$browse_table <- renderDT({
    df <- browse_filtered()
    display <- data.frame(
      Institution = institution_labels[df$institution_short],
      Code        = df$Emnekode_raw,
      Name        = df$Emnenavn,
      Year        = df$Årstall,
      Semester    = df$Semesternavn,
      Status      = status_labels[as.character(df$Status)],
      Text        = ifelse(!is.na(df$fulltext) & nchar(df$fulltext) > 0, "Yes", "No"),
      URL         = ifelse(
        is.na(df$url), "",
        paste0('<a href="', htmltools::htmlEscape(df$url),
               '" target="_blank" title="', htmltools::htmlEscape(df$url),
               '">Link</a>')
      ),
      stringsAsFactors = FALSE
    )
    datatable(
      display,
      selection = "single",
      escape = FALSE,  # allow HTML in URL column
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        dom = "lftip",
        columnDefs = list(
          list(width = "60px", targets = c(3, 6)),  # Year, Text
          list(width = "50px", targets = 7)          # URL
        )
      ),
      rownames = FALSE
    )
  }, server = TRUE)

  # ── Browse: selected course ──
  selected_course <- reactive({
    idx <- input$browse_table_rows_selected
    if (is.null(idx) || length(idx) == 0) return(NULL)
    browse_filtered()[idx, ]
  })

  # ── Browse: plaintext display ──
  output$browse_fulltext <- renderUI({
    sel <- selected_course()
    if (is.null(sel)) return(tags$p(class = "text-muted", "Select a row to view course plan text."))
    txt <- sel$fulltext
    if (is.na(txt) || nchar(txt) == 0) return(tags$p(class = "text-muted", "No fulltext available for this course."))
    tags$div(class = "fulltext-display", txt)
  })

  # ── Browse: rendered HTML display ──
  output$browse_html_frame <- renderUI({
    sel <- selected_course()
    if (is.null(sel)) return(tags$p(class = "text-muted", "Select a row to view rendered HTML."))

    raw_html <- load_course_html(
      sel$course_id, sel$institution_short, html_cache
    )
    if (is.null(raw_html) || is.na(raw_html))
      return(tags$p(class = "text-muted", "No HTML available for this course."))

    tags$iframe(
      srcdoc = raw_html,
      sandbox = "allow-same-origin",
      class = "html-frame"
    )
  })

  # ── Diff: cascading course code selector ──
  observe({
    inst <- input$diff_inst
    if (is.null(inst) || inst == "") {
      updateSelectizeInput(session, "diff_code", choices = character(0), server = TRUE)
      return()
    }
    # Only show course codes that have 2+ distinct plan versions
    codes <- courses |>
      filter(institution_short == inst, !is.na(plan_content_id)) |>
      group_by(Emnekode_raw) |>
      filter(n_distinct(plan_content_id) >= 2) |>
      ungroup() |>
      pull(Emnekode_raw) |>
      unique() |>
      sort()
    updateSelectizeInput(session, "diff_code", choices = codes, server = TRUE)
  })

  # ── Diff: version data for selected course ──
  diff_versions <- reactive({
    inst <- input$diff_inst
    code <- input$diff_code
    if (is.null(inst) || inst == "" || is.null(code) || code == "") return(NULL)

    courses |>
      filter(
        institution_short == inst,
        Emnekode_raw == code,
        !is.na(plan_content_id)
      ) |>
      arrange(Årstall, Semesternavn) |>
      group_by(plan_content_id) |>
      summarise(
        year_from = min(Årstall),
        year_to = max(Årstall),
        semesters = n(),
        fulltext = first(fulltext),
        fulltext_normalized = first(fulltext_normalized),
        .groups = "drop"
      ) |>
      arrange(year_from, year_to) |>
      mutate(
        version = paste0("V", row_number()),
        label = paste0(version, " (", year_from,
                       ifelse(year_from == year_to, "", paste0("-", year_to)),
                       ")"),
        hash_short = substr(plan_content_id, 1, 8)
      )
  })

  # ── Diff: version timeline table ──
  output$diff_versions_table <- renderDT({
    vers <- diff_versions()
    if (is.null(vers) || nrow(vers) == 0) return(NULL)
    display <- data.frame(
      Ver = vers$version,
      Years = ifelse(vers$year_from == vers$year_to,
                     as.character(vers$year_from),
                     paste0(vers$year_from, "-", vers$year_to)),
      N = vers$semesters,
      Hash = vers$hash_short,
      stringsAsFactors = FALSE
    )
    datatable(display,
      selection = "none", rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE,
                     columnDefs = list(list(width = "30px", targets = c(0, 2))))
    )
  }, server = FALSE)

  # ── Diff: render all consecutive diffs (newest pair on top) ──
  output$diff_output <- renderUI({
    vers <- diff_versions()
    if (is.null(vers) || nrow(vers) < 2) {
      return(tags$p(class = "text-muted",
        "Select an institution and course code with multiple plan versions."))
    }

    use_normalized <- (input$diff_mode == "normalized")
    n <- nrow(vers)

    # Build list of diff blocks, newest pair first
    diff_blocks <- lapply(seq(n, 2), function(i) {
      row_a <- vers[i - 1, ]
      row_b <- vers[i, ]

      text_a <- if (use_normalized) row_a$fulltext_normalized else row_a$fulltext
      text_b <- if (use_normalized) row_b$fulltext_normalized else row_b$fulltext

      diff_html <- render_diff_html(
        text_a, text_b,
        banner_a = row_a$label,
        banner_b = row_b$label,
        mode = input$diff_layout
      )

      if (is.null(diff_html)) {
        return(tags$div(
          class = "diff-pair",
          tags$h5(paste(row_a$label, "vs", row_b$label)),
          tags$p(class = "text-muted", "Could not compute diff (missing text).")
        ))
      }

      note <- NULL
      if (identical(trimws(text_a), trimws(text_b))) {
        note <- if (use_normalized) {
          tags$div(class = "alert alert-info",
            "Normalized texts are identical. Try 'Raw fulltext' to see what differs.")
        } else {
          tags$div(class = "alert alert-info", "Raw texts are identical.")
        }
      }

      tags$div(
        class = "diff-pair",
        tags$h5(paste(row_a$label, "vs", row_b$label)),
        note,
        tags$div(class = "diff-container", HTML(diff_html))
      )
    })

    do.call(tagList, diff_blocks)
  })
}

shinyApp(ui, server)
