# R/institution_config.R
# Single source of truth for all institution configuration.
# Each institution declares its harvesting strategy, the page part that holds
# the plan (`selector`, minus `exclude`), pre/post processing functions, fetch
# overrides, and how the plan is read into sections (`section_*`; see
# R/blocks.R).

institution_configs <- list(

  oslomet = list(
    code = "1175",
    strategy = "standard",
    selector = "#main-content",
    year_in_url = TRUE,
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  uia = list(
    code = "1171",
    strategy = "standard",
    selector = "#right-main",
    year_in_url = TRUE,
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  ntnu = list(
    code = "1150",
    strategy = "standard",
    selector = "#content",
    year_in_url = TRUE,
    request_delay = 10,  # robots.txt: Crawl-delay: 10 for all user agents
    post_fn = .post_ntnu,
    fetch_fn = fetch_html_cols_single_ntnu,
    section_strategy = "html",
    # h3 sections plus the h2 "Eksamen" block (Vurderingsordning, Karakter);
    # its h4 exam sessions (dates, rooms) are dropped (#245).
    section_heading_selector = "h2, h3",
    section_subheading_selector = "h4"
  ),

  inn = list(
    code = "0264",
    strategy = "standard",
    selector = ".content-inner",
    year_in_url = TRUE,
    pre_fn = .add_table_cell_breaks,
    section_strategy = "html",
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
    # course coordinator and approver (names)
    exclude = "div.field-person-in-charge, div.field-approval-sign",
    year_in_url = TRUE,
    pre_fn = .add_table_cell_breaks,
    # Drupal fields (div.field-<name> + div.label) name each part of the plan;
    # the exam table is the unlabelled field-assessments-row (#214).
    section_strategy = "html",
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
    year_in_url = TRUE,
    user_agent = "browser",
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  hvl = list(
    code = "0238",
    strategy = "standard",
    selector = ".l-2-col__main-content",
    year_in_url = TRUE,
    fetch_fn = fetch_html_cols_single_hvl,
    section_strategy = "html",
    section_heading_selector = "h3",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  mf = list(
    code = "8221",
    strategy = "standard",
    selector = "article .content-body",
    # facts box, contact card (staff names) and banner
    exclude = paste(".template-study-subject__details, .template-study-subject__contact,",
                    ".wp-block-mf-banner, hgroup"),
    year_in_url = FALSE,
    # WordPress page: details/summary accordions plus an untitled intro block
    # (only three h2s, none of them plan sections) (#213). The intro is
    # course content, with paragraph sub-headings ("Arbeidsform og
    # organisering:"); the accordion group's h2 "Om studiet" ends it.
    section_strategy = "html",
    section_heading_selector = "h2, summary",
    section_subheading_selector = "div.wp-block-group p",
    section_scope = "details",
    section_initial = "course_content"
  ),

  nla = list(
    code = "8223",
    strategy = "json_extract",
    year_in_url = FALSE,
    section_strategy = "json"
  ),

  nord = list(
    code = "1174",
    strategy = "standard",
    # Title, course code, the short description and the accordions; each
    # section is an accordion item (div.ac) with a trigger button, minus its
    # "Kopier lenke" label, and a panel. Not the "sist oppdatert" line.
    selector = "div.main-content",
    exclude = ".copy-accordion-anchor, .pre-title",
    year_in_url = TRUE,
    section_strategy = "html",
    section_heading_selector = "button.ac-trigger",
    section_scope = "div.ac",
    # Arbeidskrav/obligatorisk deltakelse are lines inside the vurdering
    # accordion, not a heading of their own (#212).
    section_inline_coursework = TRUE
  ),

  nih = list(
    code = "1260",
    strategy = "standard",
    selector = ".fs-body",
    year_in_url = TRUE,
    section_strategy = "html"
  ),

  uib = list(
    code = "1120",
    strategy = "standard",
    # The main column: h2 sections and the details accordions; not the sidebar
    # (exam dates and rooms).
    selector = "div.grid-span-main",
    year_in_url = TRUE,
    request_delay = 10,  # robots.txt: Crawl-delay: 10 for all user agents
    # h2 sections (Mål og innhald, Læringsutbytte) and details/summary
    # accordions (Krav til forkunnskapar, Vurderingsformer, Litteraturliste).
    section_strategy = "html",
    section_heading_selector = "h2, summary",
    section_scope = "details"
  ),

  uio = list(
    code = "1110",
    strategy = "standard",
    selector = "#vrtx-course-content",
    year_in_url = FALSE,
    section_strategy = "html",
    # h3 carries Obligatoriske/Anbefalte forkunnskaper inside "Opptak til
    # emnet"; <p>Obligatoriske aktiviteter:</p> sits inside "Undervisning".
    section_subheading_selector = "h3, h4, p"
  ),

  uis = list(
    code = "1160",
    strategy = "html_pdf_discovery",
    # The plan, without the page navigation, the contact footer (staff names)
    # and the facts box, but with the exam boxes, which share its class
    # (#251). Pages of withdrawn course versions have no div.article, only a
    # notice ("This course version is no longer available ...").
    selector = "#block-page-content div.article",
    exclude = ".content-navigation, .course-footer, .factbox--course:not(.factbox--exam)",
    year_in_url = TRUE,
    section_strategy = "html",
    # PDF plans (text_split fallback) open with a title and a metadata block;
    # the untitled paragraph after it is the course introduction (#243).
    section_text_header = "^(?:Emnekode|Vekting|Semester|Antall semestre|Undervisningsspråk|Tilbys av)\\b[^:]*:"
  ),

  usn = list(
    code = "1176",
    strategy = "shadow_dom",
    year_in_url = TRUE,
    section_strategy = "text"
  ),

  uit = list(
    code = "1130",
    strategy = "url_discovery",
    # div.col-md-7.mainContent holds the plan's h2 sections (Om emnet, Hva
    # lærer du, Undervisning og pensum, Eksamen) plus the year picker and the
    # contact block with staff names, which .pre_uit() cuts (#218).
    selector = ".mainContent",
    year_in_url = TRUE,
    post_fn = .pre_uit,
    section_strategy = "html",
    # 2008-2011 plans (old FS layout) head each field with a
    # div/span.fsemneoverskrift instead of an h2 (#282)
    section_heading_selector = "h2, .fsemneoverskrift",
    # h3 "Pensum" under "Undervisning og pensum", "Mer info om arbeidskrav"
    section_subheading_selector = "h3"
  ),

  nmbu = list(
    code = "1173",
    strategy = "standard",
    selector = ".layout",
    year_in_url = FALSE,
    section_strategy = "html",
    section_heading_selector = "h3",
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
    section_strategy = "text"
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
