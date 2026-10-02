# R/institution_config.R
# Single source of truth for all institution configuration.
# Each institution declares its harvesting strategy, CSS selectors,
# pre/post processing functions, and fetch overrides.

institution_configs <- list(

  oslomet = list(
    code = "1175",
    strategy = "standard",
    selector = "#main-content",
    selector_mode = "single",
    year_in_url = TRUE,
    section_strategy = "html_headings",
    section_heading_level = "h2",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  uia = list(
    code = "1171",
    strategy = "standard",
    selector = "#right-main",
    selector_mode = "single",
    year_in_url = TRUE,
    section_strategy = "html_headings",
    section_heading_level = "h2",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  ntnu = list(
    code = "1150",
    strategy = "standard",
    selector = "#content",
    selector_mode = "single",
    year_in_url = TRUE,
    request_delay = 10,  # robots.txt: Crawl-delay: 10 for all user agents
    post_fn = .post_ntnu,
    fetch_fn = fetch_html_cols_single_ntnu,
    section_strategy = "html_headings",
    # h3 sections plus the h2 "Eksamen" block (Vurderingsordning, Karakter);
    # its h4 exam sessions (dates, rooms) are dropped (#245).
    section_heading_selector = "h2, h3",
    section_subheading_selector = "h4"
  ),

  inn = list(
    code = "0264",
    strategy = "standard",
    selector = ".content-inner",
    selector_mode = "single",
    year_in_url = TRUE,
    pre_fn = .add_table_cell_breaks,
    section_strategy = "html_headings",
    # inn marks most section headings with <div class="label"> (and the facts
    # box with <div class="facts-label">); only Læringsutbytte and Pensum are
    # real <h2>. Only 3 h2s existed, which under-segmented the page (#198).
    # Walk h2 plus the class-marked divs so all heading levels are caught.
    section_heading_selector = "h2, div.label, div.facts-label"
  ),

  hivolda = list(
    code = "0236",
    strategy = "url_discovery",
    selector = "article.content-emweb",
    selector_mode = "single",
    year_in_url = TRUE,
    pre_fn = .add_table_cell_breaks,
    # Drupal fields (div.field-<name> + div.label) name each part of the plan;
    # the exam table is the unlabelled field-assessments-row (#214).
    section_strategy = "html_fields",
    section_fields = c(
      "field-course-content"             = "course_content",
      "field-learning-outcome"           = "learning_outcomes",
      "field-learning-outcome-knowledge" = "learning_outcomes",
      "field-learning-outcome-skills"    = "learning_outcomes",
      "field-learning-outcome-qualif"    = "learning_outcomes",
      "field-work-learn-activities"      = "teaching_methods",
      "field-assessment-requirements"    = "coursework_requirements",
      "field-assessments-row"            = "assessment",
      "field-req-preq-knowledge"         = "prerequisites",
      "field-course-codes-refs"          = "prerequisites",
      "field-curriculum"                 = "reading_list"
    )
  ),

  hiof = list(
    code = "0256",
    strategy = "standard",
    selector = "#vrtx-fs-emne-content, main .entry-content, .entry-content",
    selector_mode = "single",
    year_in_url = TRUE,
    user_agent = "browser",
    section_strategy = "html_headings",
    section_heading_level = "h2",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  hvl = list(
    code = "0238",
    strategy = "standard",
    selector = ".l-2-col__main-content",
    selector_mode = "single",
    year_in_url = TRUE,
    fetch_fn = fetch_html_cols_single_hvl,
    section_strategy = "html_headings",
    section_heading_level = "h3",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  mf = list(
    code = "8221",
    strategy = "standard",
    selector = "main",
    selector_mode = "single",
    year_in_url = FALSE,
    # WordPress page: details/summary accordions plus an untitled intro block
    # (only three h2s, none of them plan sections) (#213).
    section_strategy = "details_mf",
    section_intro_selector = "article .content-body > div.wp-block-group",
    section_subheading_selector = "p"
  ),

  nla = list(
    code = "8223",
    strategy = "json_extract",
    year_in_url = FALSE,
    section_strategy = "json_nla"
  ),

  nord = list(
    code = "1174",
    strategy = "standard",
    selector = paste0(
      "#ac-trigger-0, #ac-trigger-1, #ac-trigger-2, #ac-trigger-3, #ac-trigger-4, ",
      "#ac-trigger-5, #ac-trigger-6, #ac-trigger-7, #ac-trigger-8, ",
      ".ac-panel--inner, #ac-panel-2 .field__item, #ac-panel-0 li, p, .placeholder-text"
    ),
    selector_mode = "multi",
    year_in_url = TRUE,
    section_strategy = "accordion_nord",
    # Arbeidskrav/obligatorisk deltakelse are lines inside the vurdering
    # accordion, not a heading of their own (#212).
    section_inline_coursework = TRUE
  ),

  nih = list(
    code = "1260",
    strategy = "standard",
    selector = ".fs-body",
    selector_mode = "single",
    year_in_url = TRUE,
    section_strategy = "html_headings",
    section_heading_level = "h2"
  ),

  uib = list(
    code = "1120",
    strategy = "standard",
    selector = paste0(
      ".accordion, .accordion__main, ",
      ".vertical-reset-children .vertical-reset-children div, ",
      "summary, #main-content li, p, ",
      ".vertical-reset-children .vertical-reset-children .mt-12"
    ),
    selector_mode = "multi",
    year_in_url = TRUE,
    request_delay = 10,  # robots.txt: Crawl-delay: 10 for all user agents
    section_strategy = "details_uib",
    section_heading_level = "h2"
  ),

  uio = list(
    code = "1110",
    strategy = "standard",
    selector = "#vrtx-course-content",
    selector_mode = "single",
    year_in_url = FALSE,
    section_strategy = "html_headings",
    section_heading_level = "h2",
    # h3 carries Obligatoriske/Anbefalte forkunnskaper inside "Opptak til
    # emnet"; <p>Obligatoriske aktiviteter:</p> sits inside "Undervisning".
    section_subheading_selector = "h3, h4, p"
  ),

  uis = list(
    code = "1160",
    strategy = "html_pdf_discovery",
    # Every block of the plan is a direct child of .article__section; leave out
    # the page navigation, the contact footer (staff names) and the facts box,
    # but keep the exam boxes, which share its class (#251)
    selector = paste("#block-page-content .article__section >",
                     ":not(.content-navigation):not(.course-footer):not(.factbox--course),",
                     "#block-page-content .factbox--exam"),
    selector_mode = "multi",
    year_in_url = TRUE,
    # The multi selector's first match is a link, so html_headings found no
    # sections and every page fell back to text_split (#214).
    section_selector = "#block-page-content",
    section_strategy = "html_headings",
    section_heading_level = "h2",
    # PDF plans (text_split fallback) open with a title and a metadata block;
    # the untitled paragraph after it is the course introduction (#243).
    section_text_header = "^(?:Emnekode|Vekting|Semester|Antall semestre|Undervisningsspråk|Tilbys av)\\b[^:]*:"
  ),

  usn = list(
    code = "1176",
    strategy = "shadow_dom",
    year_in_url = TRUE,
    section_strategy = "text_split"
  ),

  uit = list(
    code = "1130",
    strategy = "url_discovery",
    # div.col-md-7.mainContent holds the plan's h2 sections (Om emnet, Hva
    # lærer du, Undervisning og pensum, Eksamen) plus the year picker and the
    # contact block with staff names, which .pre_uit() cuts (#218).
    selector = ".mainContent",
    selector_mode = "single",
    year_in_url = TRUE,
    post_fn = .pre_uit,
    section_strategy = "html_headings",
    section_heading_level = "h2",
    # h3 "Pensum" under "Undervisning og pensum", "Mer info om arbeidskrav"
    section_subheading_selector = "h3"
  ),

  nmbu = list(
    code = "1173",
    strategy = "standard",
    selector = ".layout",
    selector_mode = "single",
    year_in_url = FALSE,
    section_strategy = "html_headings",
    section_heading_level = "h3",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  samas = list(
    code = "0217",
    strategy = "noop",
    year_in_url = FALSE,
    section_strategy = "noop"
  ),

  steiner = list(
    code = "8225",
    strategy = "pdf_split",
    year_in_url = FALSE,
    section_strategy = "text_split"
  )
)

#' Get configuration for a single institution
#'
#' @param inst Character, institution short name (e.g. "oslomet")
#' @return Named list with institution configuration
get_institution_config <- function(inst) {
  config <- institution_configs[[inst]]
  if (is.null(config)) stop("Unknown institution: ", inst)
  config$name <- inst
  config
}

#' Get all institution configurations
#'
#' @return Named list of all institution configs
load_all_configs <- function() institution_configs
