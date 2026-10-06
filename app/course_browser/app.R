# app.R — Course Plan Concordance Browser
#
# Plan-centric: the unit is the unique course plan (~11k), not the offering
# (~33k). 66% of offerings share their plan text with another offering, so an
# offering-level browser makes you read the same paragraph several times.
# Offering coverage is shown on each plan instead.
#
# Run with: shiny::runApp("app/course_browser")

library(shiny)
library(bslib)
library(DT)
library(dplyr, warn.conflicts = FALSE)
library(stringi)
library(ggplot2)
library(diffobj)

# The pipeline's reader of the raw harvest, for the Source HTML view
for (f in c("utils.R", "fetch_html_cols.R", "extract_fulltext.R", "institution_config.R",
            "pipeline.R")) {
  source(file.path("../../R", f))
}

# ── Global ──────────────────────────────────────────────────────────────────

bd <- load_browser_data()
plans <- bd$plans
sections <- bd$sections
coverage <- bd$coverage

index <- build_search_index(plans, sections)
term_set <- load_term_set()
review <- load_review()

# Join key shared by plans and sections: a plan_content_id can recur across
# course codes, so all three key columns are needed to identify a plan.
make_key <- function(df) {
  paste(df$plan_content_id, df$institution, df$Emnekode, sep = "\r")
}
plan_key <- make_key(plans)
section_key <- if (is.null(sections)) NULL else make_key(sections)

inst_choices <- sort(unique(plans$institution))
names(inst_choices) <- institution_labels[inst_choices]
year_range <- range(c(plans$year_from, plans$year_to), na.rm = TRUE)
fag_choices <- sort(unique(na.omit(plans$Fagnavn)))

available_sections <- if (is.null(sections)) character(0) else {
  present <- intersect(names(SECTION_LABELS), unique(sections$section))
  setNames(present, SECTION_LABELS[present])
}
scope_choices <- c("Whole plan" = "plan", available_sections)

# ── UI ──────────────────────────────────────────────────────────────────────

ui <- page_navbar(
  id = "nav",
  title = "Course Plan Concordance",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css")),

  # ── Concordance ──
  nav_panel(
    "Concordance",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,
        textInput("q", "Search", placeholder = "e.g. livsmestring dybdelæring"),
        tags$div(class = "search-hint",
          "Space between terms means AND — every term must be present. ",
          tags$strong("\"quote a phrase\""), " to search it as one term."),
        if (!is.null(term_set)) {
          selectizeInput("term", "…or load a term", choices = c("", names(term_set)),
                         options = list(placeholder = "Pick a defined term"))
        },
        selectInput("scope", "Search in", choices = scope_choices, selected = "plan"),
        tags$div(
          class = "opt-row",
          checkboxInput("regex", "Regex", FALSE),
          checkboxInput("mcase", "Match case", FALSE)
        ),
        uiOutput("hit_summary"),
        tags$hr(),
        selectizeInput("inst", "Institution", choices = inst_choices,
                       multiple = TRUE, options = list(placeholder = "All")),
        sliderInput("years", "Active in years", min = year_range[1],
                    max = year_range[2], value = year_range, step = 1, sep = ""),
        selectizeInput("fag", "Subject (Fagnavn)", choices = fag_choices,
                       multiple = TRUE, options = list(placeholder = "All")),
        actionButton("clear", "Clear", class = "btn-sm btn-outline-secondary")
      ),
      layout_columns(
        col_widths = c(5, 7),
        fill = TRUE,
        DTOutput("results"),
        tags$div(class = "detail", uiOutput("detail"))
      )
    )
  ),

  # ── Trend ──
  nav_panel(
    "Trend",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        tags$p(class = "text-muted small",
          "Occurrence of the current search across plans, using the filters set
           on the Concordance tab."),
        selectInput("trend_facet", "Break down by",
          choices = c("Nothing" = "none", "Institution" = "institution",
                      "Subject" = "Fagnavn")),
        selectInput("trend_y", "Show",
          choices = c("Share of plans" = "proportion", "Number of plans" = "n_with")),
        tags$hr(),
        tags$p(class = "text-muted small",
          tags$strong("Note: "),
          "a plan is counted in its year_from — the year its text first appears.
           A plan unchanged from 2020 to 2025 counts once, in 2020. This measures
           uptake in new or revised plans, not the share of plans in force.")
      ),
      plotOutput("trend_plot", height = "480px"),
      tags$hr(),
      DTOutput("trend_table")
    )
  ),

  # ── Coverage ──
  nav_panel(
    "Coverage",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        tags$p(class = "text-muted small",
          "Share of offerings with extracted text, by institution and year.
           Every proportion elsewhere in this app has a denominator of
           successfully harvested plans, so institutions and years are only
           comparable where coverage is comparable."),
        uiOutput("coverage_summary")
      ),
      plotOutput("coverage_plot", height = "520px"),
      tags$hr(),
      DTOutput("coverage_table")
    )
  ),

  # ── Diff ──
  nav_panel(
    "Diff",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        selectizeInput("diff_inst", "Institution", choices = inst_choices,
                       options = list(placeholder = "Pick institution")),
        selectizeInput("diff_code", "Course code", choices = NULL,
                       options = list(placeholder = "Pick course code")),
        tags$hr(),
        tags$strong("Versions"),
        DTOutput("diff_versions"),
        tags$hr(),
        radioButtons("diff_layout", "Layout",
          choices = c("Side by side" = "sidebyside", "Unified" = "unified"))
      ),
      uiOutput("diff_output")
    )
  ),

  # ── Review ──
  if (length(review)) {
    nav_panel("Review", tags$div(class = "p-3", uiOutput("review_ui")))
  }
)

# ── Server ──────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  html_cache <- reactiveValues(inst = NULL, data = NULL)

  clear_search <- function() {
    updateTextInput(session, "q", value = "")
    updateSelectizeInput(session, "inst", selected = character(0))
    updateSelectizeInput(session, "fag", selected = character(0))
    updateSliderInput(session, "years", value = year_range)
    updateSelectInput(session, "scope", selected = "plan")
    updateCheckboxInput(session, "regex", value = FALSE)
    if (!is.null(term_set)) updateSelectizeInput(session, "term", selected = "")
  }
  observeEvent(input$clear, clear_search())

  # Sets the search from a list with q, regex, mcase, scope and inst, as given
  # by a deep link or a review item.
  apply_search <- function(qs) {
    is_true <- function(x) tolower(x) %in% c("1", "true", "yes")
    if (!is.null(qs$q))     updateTextInput(session, "q", value = qs$q)
    if (!is.null(qs$regex)) updateCheckboxInput(session, "regex", value = is_true(qs$regex))
    if (!is.null(qs$mcase)) updateCheckboxInput(session, "mcase", value = is_true(qs$mcase))
    if (!is.null(qs$scope) && qs$scope %in% scope_choices) {
      updateSelectInput(session, "scope", selected = qs$scope)
    }
    if (!is.null(qs$inst)) {
      picked <- intersect(trimws(strsplit(qs$inst, ",")[[1]]), inst_choices)
      if (length(picked)) updateSelectizeInput(session, "inst", selected = picked)
    }
  }

  # Picking a defined term fills the search box with its regex
  observeEvent(input$term, {
    req(input$term, nzchar(input$term))
    updateTextInput(session, "q", value = unname(term_set[[input$term]]))
    updateCheckboxInput(session, "regex", value = TRUE)
  })

  # Deep links: ?q=livsmestring&scope=learning_outcomes&regex=1&inst=ntnu,uit
  # lets a search be bookmarked, shared, or cited.
  observeEvent(session$clientData$url_search, once = TRUE, {
    apply_search(parseQueryString(session$clientData$url_search))
  })

  # ── Review: open the plans behind a decision in the Concordance tab ──
  output$review_ui <- renderUI({
    tagList(
      tags$p(class = "text-muted",
        "Open decisions that need the plans read. 'Show plans' runs the
         item's search on the Concordance tab; the issue in chainlink has
         the details."),
      lapply(seq_along(review), function(i) {
        r <- review[[i]]
        tags$div(class = "section-block",
          tags$h5(paste0("#", r$issue, " — ", r$title)),
          tags$p(r$question),
          actionButton(paste0("review_", i), "Show plans",
                       class = "btn-sm btn-primary"))
      })
    )
  })
  lapply(seq_along(review), function(i) {
    observeEvent(input[[paste0("review_", i)]], {
      clear_search()
      apply_search(review[[i]])
      nav_select("nav", "Concordance")
    })
  })

  # Land on the top hit so context is visible without an extra click. Fires only
  # when the result set itself changes, so clicking another row is not undone.
  results_proxy <- dataTableProxy("results")
  observeEvent(results(), {
    if (nrow(results()) > 0) selectRows(results_proxy, 1)
  })

  # ── Search ──
  query <- debounce(reactive(input$q), 250)

  query_ok <- reactive({
    q <- trimws(query() %||% "")
    nzchar(q) && query_is_valid(q, isTRUE(input$regex))
  })

  # Plans passing the metadata filters, as row indices into `plans`
  plan_subset <- reactive({
    keep <- plans$year_to >= input$years[1] & plans$year_from <= input$years[2]
    if (length(input$inst) > 0) keep <- keep & plans$institution %in% input$inst
    if (length(input$fag) > 0) keep <- keep & plans$Fagnavn %in% input$fag
    which(keep)
  })

  # One row per matching plan: where it is in `plans`, where the matched text is
  # in the search target, and how many matches it holds.
  results <- reactive({
    subset_rows <- plan_subset()

    if (!query_ok()) {
      return(tibble(
        plan_row = subset_rows,
        target_row = if (identical(input$scope, "plan")) subset_rows else NA_integer_,
        n_match = 0L
      ))
    }

    target <- resolve_scope(input$scope, plans, sections, index)
    found <- search_text(target$text, target$text_lc, trimws(query()),
                         regex = isTRUE(input$regex),
                         match_case = isTRUE(input$mcase))
    if (!length(found$hits)) {
      return(tibble(plan_row = integer(0), target_row = integer(0),
                    n_match = integer(0)))
    }

    target_keys <- if (identical(input$scope, "plan")) {
      plan_key[found$hits]
    } else {
      section_key[which(sections$section == input$scope)][found$hits]
    }
    plan_row <- match(target_keys, plan_key)

    res <- tibble(
      plan_row = plan_row,
      target_row = found$hits,
      n_match = found$n
    ) |>
      filter(!is.na(plan_row), plan_row %in% subset_rows) |>
      arrange(desc(n_match))
    res
  })

  # The texts the current matches were located in, for KWIC rendering
  target_text <- reactive({
    resolve_scope(input$scope, plans, sections, index)$text
  })

  output$hit_summary <- renderUI({
    q <- trimws(query() %||% "")
    if (nzchar(q) && !query_is_valid(q, isTRUE(input$regex))) {
      return(tags$div(class = "hit-summary text-danger", "Invalid regex"))
    }
    res <- results()
    if (!query_ok()) {
      return(tags$div(class = "hit-summary",
        sprintf("%s plans in filter", format(nrow(res), big.mark = " "))))
    }
    terms <- if (isTRUE(input$regex)) trimws(query()) else parse_query(query())
    tags$div(class = "hit-summary",
      tags$strong(format(nrow(res), big.mark = " ")), " plans · ",
      tags$strong(format(sum(res$n_match), big.mark = " ")), " matches",
      if (length(terms) > 1) {
        tags$div(class = "term-legend mt-1",
          lapply(seq_along(terms), function(j) {
            tags$span(class = paste0("chip t", ((j - 1) %% 4) + 1), terms[j])
          }))
      },
      tags$div(class = "text-muted",
        sprintf("of %s plans in filter", format(length(plan_subset()), big.mark = " ")))
    )
  })

  # ── Results table ──
  output$results <- renderDT({
    res <- results()
    p <- plans[res$plan_row, ]
    display <- data.frame(
      Inst = institution_labels[p$institution],
      Code = p$Emnekode,
      Name = p$Emnenavn,
      Years = ifelse(p$year_from == p$year_to, as.character(p$year_from),
                     paste0(p$year_from, "–", p$year_to)),
      Off = p$n_offerings,
      Hits = res$n_match,
      stringsAsFactors = FALSE
    )
    if (!query_ok()) display$Hits <- NULL
    datatable(
      display, selection = "single", rownames = FALSE,
      options = list(
        pageLength = 25, scrollY = "calc(100vh - 260px)", scrollCollapse = TRUE,
        paging = TRUE, dom = "tip",
        columnDefs = list(list(width = "42px", targets = c(0, 4)))
      )
    )
  }, server = TRUE)

  selected <- reactive({
    i <- input$results_rows_selected
    if (is.null(i) || !length(i)) return(NULL)
    results()[i, ]
  })

  # ── Detail panel ──
  output$detail <- renderUI({
    sel <- selected()
    if (is.null(sel)) {
      return(tags$p(class = "text-muted p-3",
        "Select a plan to read its matches in context."))
    }
    p <- plans[sel$plan_row, ]
    navset_tab(
      nav_panel("In context", uiOutput("kwic")),
      nav_panel("Full plan", uiOutput("fulltext")),
      nav_panel("Sections", uiOutput("sections_ui")),
      nav_panel("Offerings", DTOutput("offerings_table")),
      nav_panel("Source HTML", uiOutput("html_frame")),
      header = tags$div(
        class = "detail-head",
        tags$h5(paste0(p$Emnekode, " — ", p$Emnenavn %||% "")),
        tags$div(class = "text-muted small",
          paste0(institution_labels[p$institution], " · ",
                 if (p$year_from == p$year_to) p$year_from
                 else paste0(p$year_from, "–", p$year_to),
                 " · ", p$n_offerings, " offering",
                 if (p$n_offerings == 1) "" else "s",
                 if (!is.na(p$Fagnavn)) paste0(" · ", p$Fagnavn) else "")
        ),
        if (!is.na(p$url)) tags$a(href = p$url, target = "_blank",
                                  class = "small", "Open source page →")
      )
    )
  })

  output$kwic <- renderUI({
    sel <- selected()
    req(sel)
    if (sel$n_match == 0 || is.na(sel$target_row)) {
      return(tags$p(class = "text-muted", "No active search — see Full plan."))
    }
    txt <- target_text()[sel$target_row]
    loc <- locate_matches(txt, trimws(query()),
                          regex = isTRUE(input$regex),
                          match_case = isTRUE(input$mcase))
    if (is.null(loc)) {
      return(tags$p(class = "text-muted", "No matches to show."))
    }
    snippets <- kwic_snippets(txt, loc, width = 160)
    scope_label <- names(scope_choices)[match(input$scope, scope_choices)]
    per_term <- count_terms(txt, trimws(query()), regex = isTRUE(input$regex),
                            match_case = isTRUE(input$mcase))
    tagList(
      tags$div(class = "text-muted small mb-2",
        sprintf("%d match%s in %s", length(snippets),
                if (length(snippets) == 1) "" else "es", scope_label)),
      if (length(per_term) > 1) {
        tags$div(class = "term-legend",
          lapply(seq_along(per_term), function(j) {
            tags$span(class = paste0("chip t", ((j - 1) %% 4) + 1),
                      paste0(names(per_term)[j], ": ", per_term[j]))
          }))
      },
      lapply(seq_along(snippets), function(i) {
        tags$div(class = "snippet",
          tags$span(class = "snippet-n", i), HTML(snippets[i]))
      })
    )
  })

  output$fulltext <- renderUI({
    sel <- selected()
    req(sel)
    txt <- plans$course_plan[sel$plan_row]
    if (is.na(txt) || !nzchar(txt)) {
      return(tags$p(class = "text-muted", "No plan text."))
    }
    # Locate in the plan text regardless of scope, so the whole plan is marked
    loc <- if (query_ok()) {
      locate_matches(txt, trimws(query()), regex = isTRUE(input$regex),
                     match_case = isTRUE(input$mcase))
    } else NULL
    tagList(
      tags$div(class = "plaintext", HTML(mark_matches(txt, loc))),
      tags$script(HTML(
        "(function(){var c=document.querySelector('.plaintext');if(!c)return;
          var m=c.querySelector('mark');if(m&&m.scrollIntoView)
          m.scrollIntoView({block:'center'});})();"))
    )
  })

  output$sections_ui <- renderUI({
    sel <- selected()
    req(sel)
    if (is.null(sections)) return(tags$p(class = "text-muted", "No sections data."))
    p <- plans[sel$plan_row, ]
    rows <- sections[
      sections$plan_content_id == p$plan_content_id &
        sections$institution == p$institution &
        sections$Emnekode == p$Emnekode, , drop = FALSE]
    if (nrow(rows) == 0) {
      return(tags$p(class = "text-muted", "No sections extracted for this plan."))
    }
    ord <- order(match(rows$section, names(SECTION_LABELS)))
    rows <- rows[ord, ]
    tagList(lapply(seq_len(nrow(rows)), function(i) {
      loc <- if (query_ok()) {
        locate_matches(rows$text[i], trimws(query()),
                       regex = isTRUE(input$regex),
                       match_case = isTRUE(input$mcase))
      } else NULL
      tags$div(class = "section-block",
        tags$h6(SECTION_LABELS[rows$section[i]] %||% rows$section[i]),
        tags$div(class = "section-text", HTML(mark_matches(rows$text[i], loc))))
    }))
  })

  output$offerings_table <- renderDT({
    sel <- selected()
    req(sel)
    p <- plans[sel$plan_row, ]
    datatable(
      data.frame(
        Years = p$years, Semesters = p$semesters,
        Offerings = p$n_offerings, Credits = p$Studiepoeng %||% NA,
        Level = p$Nivanavn %||% NA, stringsAsFactors = FALSE
      ),
      selection = "none", rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  })

  output$html_frame <- renderUI({
    sel <- selected()
    req(sel)
    p <- plans[sel$plan_row, ]
    ids <- p$course_ids[[1]]
    if (is.null(ids) || !length(ids)) {
      return(tags$p(class = "text-muted", "No offering to fetch HTML for."))
    }
    raw <- load_course_html(ids[1], p$institution, html_cache)
    if (is.null(raw) || is.na(raw)) {
      return(tags$p(class = "text-muted", "No HTML stored for this offering."))
    }
    tags$iframe(srcdoc = raw, sandbox = "allow-same-origin", class = "html-frame")
  })

  # ── Trend ──
  trend_data <- reactive({
    res <- results()
    facet <- input$trend_facet

    grp <- c("year_from", if (facet != "none") facet)
    denom <- plans[plan_subset(), ] |>
      count(across(all_of(grp)), name = "n_plans")
    numer <- plans[res$plan_row, ] |>
      count(across(all_of(grp)), name = "n_with")

    denom |>
      left_join(numer, by = grp) |>
      mutate(n_with = coalesce(n_with, 0L), proportion = n_with / n_plans) |>
      filter(n_plans > 0)
  })

  output$trend_plot <- renderPlot({
    if (!query_ok()) {
      return(ggplot() + annotate("text", 0, 0, label = "Enter a search to see its trend",
                                 colour = "grey50") + theme_void())
    }
    d <- trend_data()
    facet <- input$trend_facet
    yvar <- input$trend_y

    p <- ggplot(d, aes(x = year_from, y = .data[[yvar]])) +
      geom_vline(xintercept = 2020, linetype = "dashed", colour = "grey40") +
      geom_line(linewidth = 0.6, colour = "#2c7fb8") +
      geom_point(size = 1.4, colour = "#2c7fb8") +
      scale_x_continuous(breaks = scales::breaks_width(2)) +
      labs(x = NULL,
           y = if (yvar == "proportion") "Share of plans" else "Plans with match",
           caption = "Dashed line: LK20 in force (2020). Plans counted in year_from.") +
      theme_minimal(base_size = 12)

    if (yvar == "proportion") {
      p <- p + scale_y_continuous(labels = scales::label_percent(),
                                  limits = c(0, NA))
    } else {
      p <- p + scale_y_continuous(limits = c(0, NA))
    }
    if (facet != "none") {
      p <- p + facet_wrap(vars(.data[[facet]]),
                          labeller = if (facet == "institution")
                            as_labeller(institution_labels) else "label_value")
    }
    p
  })

  output$trend_table <- renderDT({
    req(query_ok())
    d <- trend_data() |> mutate(proportion = round(proportion, 4))
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 15, dom = "tip"))
  })

  # ── Coverage ──
  output$coverage_summary <- renderUI({
    tot <- sum(coverage$n_offerings)
    harv <- sum(coverage$n_harvested)
    tags$div(
      tags$p(tags$strong(sprintf("%.0f%%", 100 * harv / tot)), " of ",
             format(tot, big.mark = " "), " offerings have extracted text."),
      tags$p(class = "text-muted small",
             sprintf("%s offerings with no text.", format(tot - harv, big.mark = " ")))
    )
  })

  output$coverage_plot <- renderPlot({
    ggplot(coverage, aes(x = Årstall, y = institution, fill = pct_harvested)) +
      geom_tile(colour = "white", linewidth = 0.4) +
      scale_fill_viridis_c(labels = scales::label_percent(), limits = c(0, 1),
                           option = "D", name = "Harvested") +
      scale_y_discrete(labels = institution_labels) +
      scale_x_continuous(breaks = scales::breaks_width(2)) +
      labs(x = NULL, y = NULL,
           title = "Share of offerings with extracted text") +
      theme_minimal(base_size = 12) +
      theme(panel.grid = element_blank())
  })

  output$coverage_table <- renderDT({
    d <- coverage |>
      mutate(Institution = institution_labels[institution],
             pct_harvested = round(100 * pct_harvested)) |>
      select(Institution, Year = Årstall, Offerings = n_offerings,
             Harvested = n_harvested, `%` = pct_harvested, Plans = n_plans)
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 20, dom = "tip"))
  })

  # ── Diff ──
  observe({
    inst <- input$diff_inst
    if (is.null(inst) || !nzchar(inst)) {
      updateSelectizeInput(session, "diff_code", choices = character(0), server = TRUE)
      return()
    }
    codes <- plans |>
      filter(institution == inst) |>
      count(Emnekode) |>
      filter(n >= 2) |>
      pull(Emnekode) |>
      sort()
    updateSelectizeInput(session, "diff_code", choices = codes, server = TRUE)
  })

  diff_versions <- reactive({
    req(input$diff_inst, input$diff_code, nzchar(input$diff_code))
    plans |>
      filter(institution == input$diff_inst, Emnekode == input$diff_code) |>
      arrange(year_from, year_to) |>
      mutate(
        version = paste0("V", row_number()),
        label = paste0(version, " (", year_from,
                       ifelse(year_from == year_to, "", paste0("–", year_to)), ")")
      )
  })

  output$diff_versions <- renderDT({
    v <- diff_versions()
    datatable(
      data.frame(Ver = v$version,
                 Years = ifelse(v$year_from == v$year_to, v$year_from,
                                paste0(v$year_from, "–", v$year_to)),
                 Off = v$n_offerings,
                 Hash = substr(v$plan_content_id, 1, 8), stringsAsFactors = FALSE),
      selection = "none", rownames = FALSE,
      options = list(dom = "t", paging = FALSE, ordering = FALSE)
    )
  }, server = FALSE)

  output$diff_output <- renderUI({
    v <- diff_versions()
    if (nrow(v) < 2) {
      return(tags$p(class = "text-muted",
        "Select an institution and a course code with more than one plan version."))
    }
    blocks <- lapply(seq(nrow(v), 2), function(i) {
      a <- v[i - 1, ]; b <- v[i, ]
      html <- render_diff_html(a$course_plan, b$course_plan,
                               a$label, b$label, mode = input$diff_layout)
      tags$div(class = "diff-pair",
        tags$h5(paste(a$label, "vs", b$label)),
        if (is.null(html)) tags$p(class = "text-muted", "Could not compute diff.")
        else tags$div(class = "diff-container", HTML(html)))
    })
    do.call(tagList, blocks)
  })
}

shinyApp(ui, server)
