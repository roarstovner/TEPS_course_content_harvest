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
    section_heading_level = "h2"
  ),

  uia = list(
    code = "1171",
    strategy = "standard",
    selector = "#right-main",
    selector_mode = "single",
    year_in_url = TRUE,
    section_strategy = "html_headings",
    section_heading_level = "h2"
  ),

  ntnu = list(
    code = "1150",
    strategy = "standard",
    selector = "#content",
    selector_mode = "single",
    year_in_url = TRUE,
    post_fn = .post_ntnu,
    fetch_fn = fetch_html_cols_single_ntnu,
    section_strategy = "html_headings",
    section_heading_level = "h3"
  ),

  inn = list(
    code = "0264",
    strategy = "standard",
    selector = ".content-inner",
    selector_mode = "single",
    year_in_url = TRUE,
    pre_fn = .add_table_cell_breaks,
    section_strategy = "html_headings",
    section_heading_level = "h2"
  ),

  hivolda = list(
    code = "0236",
    strategy = "url_discovery",
    selector = "article.content-emweb",
    selector_mode = "single",
    year_in_url = TRUE,
    pre_fn = .add_table_cell_breaks,
    section_strategy = "text_split"
  ),

  hiof = list(
    code = "0256",
    strategy = "standard",
    selector = "#vrtx-fs-emne-content, main .entry-content, .entry-content",
    selector_mode = "single",
    year_in_url = TRUE,
    user_agent = "browser",
    section_strategy = "html_headings",
    section_heading_level = "h2"
  ),

  hvl = list(
    code = "0238",
    strategy = "standard",
    selector = ".l-2-col__main-content",
    selector_mode = "single",
    year_in_url = TRUE,
    fetch_fn = fetch_html_cols_single_hvl,
    section_strategy = "html_headings",
    section_heading_level = "h3"
  ),

  mf = list(
    code = "8221",
    strategy = "standard",
    selector = "main",
    selector_mode = "single",
    year_in_url = FALSE,
    section_strategy = "html_headings",
    section_heading_level = "h2"
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
    section_strategy = "accordion_nord"
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
    section_heading_level = "h2"
  ),

  uis = list(
    code = "1160",
    strategy = "html_pdf_discovery",
    selector = "#block-page-content .link--, #block-page-content .paragraph--with-title",
    selector_mode = "multi",
    year_in_url = TRUE,
    section_strategy = "html_headings",
    section_heading_level = "h2"
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
    selector = ".hovedfelt > main > div.col-md-12",
    selector_mode = "multi",
    year_in_url = TRUE,
    post_fn = .pre_uit,
    section_strategy = "html_headings",
    section_heading_level = "h2"
  ),

  nmbu = list(
    code = "1173",
    strategy = "standard",
    selector = ".layout",
    selector_mode = "single",
    year_in_url = FALSE,
    section_strategy = "html_headings",
    section_heading_level = "h3"
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
