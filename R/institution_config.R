# R/institution_config.R
# Single source of truth for all institution configuration.
# Each institution declares its harvesting strategy, the page part that holds
# the plan (`selector`, minus `exclude`), how a plan's year is known
# (`plan_years`), pre/post processing functions, fetch overrides, and how the
# plan is read into sections (`section_*`; see R/blocks.R).
#
# facts: the elements that hold the title, course code and facts box (CSS, or
#        XPath when it starts with "/"; in nla's JSON the parts "title" and
#        "table"), and parts in no section that change from year to year
#        while the plan stays the same (programme and subject lists,
#        admission flags, stock evaluation text). Their text is in
#        course_plan but in no section, not in the plan id (a campus or
#        programme list that changes from year to year does not make a new
#        plan) and not counted by the shell rule (course_plan_spec.md). In pages read as text (usn, steiner, uis
#        PDFs) they are the lines before the plan (.text_header() in
#        R/blocks.R).
#
# plan_years: "url"     the URL holds the year (and semester), so every DBH
#                       year is harvested (nla: the academic year, and the
#                       page's JSON holds that year's plan only);
#             "current" the site shows only the plan in force when the page is
#                       fetched, so a harvest gives the plan of that academic
#                       year only.
#
# page_term: function(html) giving the start semester of the plan version a
#            page shows, where the page says so, as semester_term() (inn: the
#            selected start semester; nord, uis: only an academic year, which
#            counts as its autumn); an offering whose page shows a version
#            that starts after the offering's semester gets no plan (a later
#            version, course_plan_spec.md). inn, nord, uis, hiof.
# page_end_term: function(html) giving the last semester the version a page
#            shows is for; an offering after it gets no plan (an earlier
#            version). hiof: a page for one semester or for two
#            ("Høst 2013–Vår 2014", which serves the spring offering too).
#
# url_year: "academic"  the URL's year is the academic year the page is for
#                       (ntnu, hvl, oslomet: "/2025" is 2025/26; uis: the
#                       semester menu's "2025 - 2026"), so a spring or
#                       summer offering reads the year before, and
#                       read_harvest() gives an offering the page fetched for
#                       its course and academic year, whichever offering it
#                       was fetched for.
#
# request_delay: seconds between requests to the site (default 3-5,
#                DEFAULT_REQUEST_DELAY), at least the Crawl-delay its
#                robots.txt asks of all user agents. The robots.txt of every
#                host is in data/robots_txt.csv (date read, crawl delay,
#                disallowed paths); tests/testthat/test-robots.R checks the
#                two against each other.
#
# english_page: list(notice, from, to) for a course whose plan is only on the
#                English page, while the Norwegian page says so instead
#                (`notice`, a regular expression on the page's HTML). The
#                harvest then fetches the English URL, the Norwegian one with
#                its part `from` replaced by `to` (fetch_html_cols_single());
#                the row keeps the Norwegian URL. A page with the notice in
#                the raw store (fetched before the notice was known) gives
#                no plan (plan_offerings()). hvl, nmbu, nord, ntnu.
#
# robots_permission: the institution's permission to harvest paths its
#                robots.txt disallows for all user agents, recorded as
#                "granted YYYY-MM-DD by <who> (<what>)". Without one, the
#                harvest does not request a URL that a disallow rule in
#                data/robots_txt.csv matches (robots_allows(), README
#                "robots.txt"). A decision of the project's own, taken
#                without asking the institution, is recorded as such
#                ("decided YYYY-MM-DD by ..., not granted by ..."): steiner.

institution_configs <- list(

  oslomet = list(
    code = "1175",
    strategy = "standard",
    selector = "#main-content",
    # the portlet's name ("EPN-V2")
    exclude = "#main-content h2.portlet-title-text",
    # the title and the facts row (English name, credits, year, reading list,
    # timetable, programmes)
    facts = c("#main-content div.header", "#main-content section.section-top"),
    plan_years = "url",
    url_year = "academic",
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p",
    # Teaching methods that only say "Se fagplanen." take the text of the
    # subject's Fagplan block on the same page. The block's coursework and
    # attendance rules join the course's own coursework requirements, up to a
    # label of another part (exam forms, suitability, grading).
    section_pointer = list(
      heading = "Fagplan", lead = "Fagplanen sier:",
      sections = c(teaching_methods = "Fagets arbeids- og undervisningsformer"),
      append = list(coursework_requirements = c(
        "Arbeidskrav", "Arbeidskrav og faglig aktivitet med krav om deltakelse",
        "Faglig aktivitet med krav om deltakelse", "Obligatorisk deltakelse",
        "Deltaking i undervisning")),
      stop = paste0("^(Rettigheter og plikter ved eksamen|Vurderings-/eksamensformer|",
                    "Vurderingskriterier|Vurdering - vurderingskriterier|Skikkethetsvurdering|",
                    "Vurderingsuttrykk|Samlet vurdering|Kvalitativ beskrivelse)")
    )
  ),

  uia = list(
    code = "1171",
    strategy = "standard",
    selector = "#right-main",
    # the breadcrumbs (they end with the title) and the facts' links
    # ("Timeplan", "Pensumliste (Leganto)")
    exclude = "nav#breadcrumbs, #vrtx-fs-emne-content div.connected-links",
    # and the stock student evaluation text and the admission flags (single
    # course, external candidates), each an h2 and the div after it
    facts = c("#vrtx-fs-emne-content div.page-intro", "#vrtx-fs-emne-facts",
              local({
                h2 <- paste0("//h2[contains(@class, 'vrtx-fs-infotype-title')][",
                             paste0("normalize-space() = '", c("Studentevaluering", "Tilbys som enkeltemne",
                                    "Vert tilbydd som enkeltemne", "Tilgang for privatister",
                                    "Tilgang for privatistar"), "'", collapse = " or "), "]")
                paste0(h2, " | ", h2, "/following-sibling::div[1]")
              })),
    plan_years = "url",
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  ntnu = list(
    code = "1150",
    strategy = "standard",
    selector = "#content",
    # Not read: the timetable tab, the tab titles, and in the exam tab
    # (#omEksamen) the link, the rooms, the comments ("Individuelt tidspunkt
    # kommer senere") and each exam part's date and time with their labels
    # (on the English page "Date", "Time"): when and where, not the
    # assessment (course_plan_spec.md)
    exclude = c(
      paste("#timeplan, h1.tabTitle, #omEksamen .exam-link, #omEksamen .exam-rooms, #omEksamen ul.portlet-msg-info,",
            # banners: "Nytt fra studieåret 2025/2026", "Undervises ikke
            # studieåret ...", "Det tilbys ikke lenger undervisning i emnet."
            "p.portlet-msg-alert"),
      local({
        label <- paste0("//div[@id='omEksamen']//span[contains(@class, 'exam-fact-label')]",
                        "[normalize-space() = 'Dato' or normalize-space() = 'Tid' or ",
                        "normalize-space() = 'Sted og rom' or normalize-space() = 'Date' or ",
                        "normalize-space() = 'Time']")
        paste0(label, " | ", label, "/following-sibling::span[1]")
      })),
    # the page title, course code, name and facts (credits, level, start,
    # language, place, assessment form); the admission programme list
    # ("Spesielle vilkår"), the credit-reduction box and the subject areas
    facts = c("#content > h1", "#course-details > span.course-code",
              "#course-details > h1.course-name", "#course-details > div.course-facts",
              "#requirements-toggler", "#credit-reductions-toggler",
              "//div[contains(@class, 'course-card')][.//ul[contains(@class, 'subject-area-list')]]"),
    plan_years = "url",
    url_year = "academic",
    request_delay = 10,  # robots.txt (data/robots_txt.csv): Crawl-delay: 10 for all user agents
    fetch_fn = fetch_html_cols_single_ntnu,
    # A course whose plan is only in English: the Norwegian page's course
    # content, and mostly every other part, says "See English version.", "Se
    # engelsk beskrivelse." or "Se engelsk tekst." (2021-2025); www.ntnu.edu
    # has the plan, the same layout with English headings
    english_page = list(
      notice = paste0('<p class="content-course-content">\\s*(?i:see english version|',
                      'se engelsk (?:beskrivelse|tekst))\\.?\\s*</p>'),
      from = "https://www.ntnu.no/studier/emner/", to = "https://www.ntnu.edu/studies/courses/"),
    # The exam tab lists the exam parts once per exam session; the
    # assessment section and the plan id keep one session
    exam_sessions = TRUE,
    section_strategy = "html",
    # h3 sections plus the exam tab's h2 "Eksamen" (assessment); h4 marks
    # the contact block's sub-headings and the exam sessions. An old-layout
    # "Vurderingsordning: ..." h3 is the tab's text, not a heading.
    section_heading_selector = "h2, h3:not(.exam-assessment-form)",
    section_subheading_selector = "h4",
    # "Mer om vurdering" lists the arbeidskrav under "Obligatoriske
    # arbeidskrav" or "OBLIGATORISKE AKTIVITETER", not a heading of their own
    section_inline_coursework = TRUE
  ),

  inn = list(
    # 1177 since INN became Universitetet i Innlandet (DBH from 2025)
    code = c("0264", "1177"),
    strategy = "standard",
    selector = ".content-inner",
    # the title, the semester picker ("Startsemestre") and the facts list
    # (code, credits, semesters, language, place) but not its prerequisites
    facts = c("section#main-content > header", "fieldset.semesters-top",
              paste0("//ul[@id='facts']//li[div[contains(@class, 'facts-label')]]",
                     "[not(div[contains(@class, 'facts-label')][normalize-space() = 'Forkunnskapskrav' or ",
                     "normalize-space() = 'Required prerequisite knowledge'])]")),
    plan_years = "url",
    # The start semester of the version the page shows (the selected
    # "Startsemestre"), in Norwegian also on the English page: the latest
    # version where the requested semester has none, with or without the
    # notice "Emnebeskrivelsen for valgt semester er ikke publisert enda"
    page_term = function(html) {
      m <- stringr::str_match(html, 'data-readinglist-start-semester="(\\d{4}) (Vår|Spring)?')
      semester_term(m[, 2], ifelse(is.na(m[, 3]), "Høst", "Vår"))
    },
    section_strategy = "html",
    # inn marks most section headings with <div class="label"> (and the facts
    # box with <div class="facts-label">); only Læringsutbytte and Pensum are
    # real <h2>. Only 3 h2s existed, which under-segmented the page.
    # Walk h2 plus the class-marked divs so all heading levels are caught.
    section_heading_selector = "h2, div.label, div.facts-label"
  ),

  hivolda = list(
    code = "0236",
    strategy = "url_discovery",
    selector = "article.content-emweb",
    # course coordinator and approver (names)
    exclude = "div.field-person-in-charge, div.field-approval-sign",
    # the title and the one-line fields (code, duration, credits, level,
    # semesters, language, student numbers), not the reading-list pointer
    facts = c("div.node-cd > h2", "div.node-cd div.label-inline:not(.field-curriculum)",
              # the stock evaluation and examiner-arrangement sentences and
              # the programme list
              "div.node-cd div.field-evaluation", "div.node-cd div.field-sensor-system",
              "div.node-cd div.used-in-programs"),
    plan_years = "url",
    # Drupal fields (div.field-<name> + div.label) name each part of the plan;
    # the exam table is the unlabelled field-assessments-row.
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
    # faculty folders, Norwegian and English, from the site's listings
    # (resolve_urls_hiof_batch())
    strategy = "url_discovery",
    selector = "#vrtx-fs-emne-content, main .entry-content, .entry-content",
    facts = c("#vrtx-fs-emne-content > h1", "#vrtx-fs-emne-facts"),
    plan_years = "url",
    user_agent = "browser",
    # The semesters in the title, "LSKNO22215 ... (Vår 2020)", "(Autumn 2024)"
    # on an English page, "(Høst 2013–Vår 2014)" for a course over two
    # semesters (title_terms()): the folder videre/ has pages whose title
    # names a later semester than the URL's, and a spring offering can take
    # the autumn page of a course over two semesters (hiof_match_urls())
    page_term = function(html) title_terms(html)$first,
    page_end_term = function(html) title_terms(html)$last,
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  hvl = list(
    code = "0238",
    strategy = "standard",
    # The title, the facts aside (credits, campus, semesters, language,
    # programmes) and the plan; not the year stamp and year links, the
    # aside's course coordinators (names), exam schedule and reading-list
    # button
    selector = "div.l-body",
    exclude = c("div.l-body > h2, div.l-body > .link-container, .l-2-col__aside .button",
                paste0("//aside[contains(@class, 'l-2-col__aside')]//li[.//h3 or ",
                       ".//div[contains(@class, 'teaser-icon--person')]]")),
    facts = c("div.l-body > h1", ".l-2-col__aside"),
    plan_years = "url",
    url_year = "academic",
    fetch_fn = fetch_html_cols_single_hvl,
    # "Emnebeskrivelsen finnes kun på engelsk", "Dette emnet er kun beskrevet
    # på engelsk", "Dette emnet er berre skildra på engelsk", "Se engelske
    # nettsider for mer informasjon" (2025): the English page has the same
    # layout
    english_page = list(
      notice = paste0("Emnebeskrivelsen finnes kun på engelsk|emne[tr]? er (?:kun beskrevet|berre skildra) ",
                      "på engelsk|Se engelske nettsider"),
      from = "/studier/studieprogram/emne/", to = "/en/studies-at-hvl/study-programmes/courses/"),
    section_strategy = "html",
    section_heading_selector = "h3",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  mf = list(
    code = "8221",
    strategy = "standard",
    selector = "article .content-body",
    # contact card (staff names), banner, the facts box's buttons ("Vis
    # flere", "Timeplan", "Kontakt studieveileder"), the stock
    # library-access block ("Tilgang til litteratur") and the button to the
    # reading list ("Litteraturlisten for dette emnet"; the list itself is in
    # Leganto); the 2026-04 pages name the classes template-study-program__*,
    # later ones template-study-subject__*
    exclude = c(paste("#block-emnelegantoinfo,",
                    ".template-study-subject__contact, .template-study-program__contact,",
                    ".wp-block-mf-banner, .template-study-subject__details button,",
                    ".template-study-program__details button,",
                    ".template-study-subject__details .wp-block-button,",
                    ".template-study-program__details .wp-block-button"),
                "//div[contains(@class, 'wp-block-button')][.//a[contains(@href, 'leganto')]]"),
    plan_years = "current",
    # WordPress page: details/summary accordions plus an untitled intro block
    # (only three h2s, none of them plan sections). The intro is
    # course content, with paragraph sub-headings ("Arbeidsform og
    # organisering:"); the accordion group's h2 "Om studiet" ends it.
    section_strategy = "html",
    section_heading_selector = "h2, summary",
    section_subheading_selector = "div.wp-block-group p",
    section_scope = "details",
    section_initial = "course_content",
    facts = c("hgroup", ".template-study-subject__details", ".template-study-program__details")
  ),

  nla = list(
    code = "8223",
    strategy = "json_extract",
    plan_years = "url",
    section_strategy = "json",
    # the title and the table (code, name, semester, place, year, language,
    # credits)
    facts = c("title", "table")
  ),

  nord = list(
    code = "1174",
    strategy = "standard",
    # Title, course code, the short description and the accordions; each
    # section is an accordion item (div.ac) with a trigger button, minus its
    # "Kopier lenke" label, and a panel. Then the sidebar's facts (language,
    # credits, faculty, campus). Not the "sist oppdatert" line,
    # the title's mobile copy, the sidebar's semester picker, staff (names)
    # and Leganto link.
    selector = "div.node__content",
    exclude = paste(".copy-accordion-anchor, .pre-title, div.node__content > div.for-mobile,",
                    "section.sidebar .semester-chooser, section.sidebar .field--name-field-p-employees,",
                    "section.sidebar .field--name-field-subject-leganto-link"),
    plan_years = "url",
    # A course taught in English: "Emnet foregår på engelsk. Se engelske
    # nettsider ved å velge engelsk språk." (2025), and the plan is on the
    # English page of the same year and semester
    english_page = list(notice = "Emnet foregår på engelsk\\. Se engelske nettsider",
                        from = "/studier/emner/", to = "/en/studies/courses/"),
    section_strategy = "html",
    section_heading_selector = "button.ac-trigger",
    section_scope = "div.ac",
    # the short description between the course code and the accordions
    # (2019-), not the title and code before it
    section_untitled = c("div.placeholder-text" = "course_content"),
    facts = c("div.main-content > h1", "div.main-content > .field--name-field-subject-code",
              "section.sidebar", "div.ac.type_of_subject"),   # "Type emne": Valgbart, Obligatorisk
    # For a year it has no plan for, Nord shows "Gjeldende emnebeskrivelse
    # (sist oppdatert 2026/27)": the current plan, a later version (the
    # English page: "Current active course description (last updated
    # 2026/27)", "Course description for 2025/26")
    page_term = function(html) semester_term(stringr::str_match(
      html, paste0("(?:Gjeldende emnebeskrivelse \\(sist oppdatert|Emnebeskrivelse for|",
                   "Current active course description \\(last updated|Course description for) (\\d{4})/"))[, 2], "Høst"),
    # Arbeidskrav/obligatorisk deltakelse are lines inside the vurdering
    # accordion, not a heading of their own.
    section_inline_coursework = TRUE
  ),

  nih = list(
    code = "1260",
    strategy = "standard",
    # The title, the facts list and the plan; not the course coordinator
    # (names), the table of contents' heading and the FS fetch stamp
    selector = "#vrtx-fs-emne-content",
    exclude = c("#vrtx-fs-emne-content > h2.vrtx-toc-header, #vrtx-fs-emne-content > .vrtx-date-info",
                paste0("//div[@id='vrtx-fs-emne-facts']//div[contains(@class, 'dg')]",
                       "[starts-with(normalize-space(), 'Emneansvarlig')]")),
    facts = c("#vrtx-fs-emne-content > h1", "#vrtx-fs-emne-facts"),
    plan_years = "url",
    section_strategy = "html"
  ),

  uib = list(
    code = "1120",
    strategy = "standard",
    # The title, level and facts list above the main column, and the main
    # column: h2 sections and the details accordions; not the facts' link to
    # the reading list, and not the sidebar (contact e-mail, the semester's
    # exam schedule, a link to the department)
    selector = "div.region-inner.grid",
    exclude = c("div.region-inner.grid div.sidebar",
                "//dl[contains(@class, 'course-summary')]/div[starts-with(normalize-space(), 'Ressurs')]"),
    facts = "div.region-inner.grid > div.theme-context__area",
    plan_years = "url",
    # 10 s since 2026-09-30; robots.txt (data/robots_txt.csv) asks it only of
    # Scrapy on 2026-10-16, and the pace is kept
    request_delay = 10,
    # h2 sections (Mål og innhald, Læringsutbytte) and details/summary
    # accordions (Krav til forkunnskapar, Vurderingsformer, Litteraturliste).
    section_strategy = "html",
    section_heading_selector = "h2, summary",
    section_scope = "details"
  ),

  uio = list(
    code = "1110",
    strategy = "url_discovery",
    # The title, the plan and the facts box after it ("Fakta om emnet": level,
    # credits, semesters, language, department); not the table of contents,
    # the links to the semester pages and the FS fetch stamp, nor the shared
    # exam-help block ("Mer om eksamen ved UiO", its link list and the
    # closing line), nor "BinaryValueReference{ref = ...}", a rendering
    # defect in the facts box's teaching field (with its label). A page with
    # no #vrtx-content (the Weblogin page saved in place of a closed course
    # page) has no text, so no plan. The banner above the content ("Dette
    # emnet er nedlagt / erstattet av ...") is outside the selector and stays
    # out (course_plan_spec.md: banners).
    selector = "#vrtx-content",
    exclude = c(paste("#vrtx-course-title-toc > :not(h1), #vrtx-course-semesters,",
                      "#vrtx-main-content > .vrtx-date-info"),
                local({
                  h <- paste0("//h3[", paste0("normalize-space() = '", c("Mer om eksamen ved UiO",
                                "Meir om eksamen ved UiO", "More about examinations at UiO"), "'",
                                collapse = " or "), "]")
                  paste0(h, " | ", h, "/following-sibling::ul[1] | ", h, "/following-sibling::p[1]")
                }),
                "//dt[following-sibling::dd[1][starts-with(normalize-space(), 'BinaryValueReference')]]",
                "//dd[starts-with(normalize-space(), 'BinaryValueReference')]"),
    facts = c("#vrtx-course-title-toc", "#vrtx-additional-content"),
    plan_years = "current",
    section_strategy = "html",
    # h3 carries Obligatoriske/Anbefalte forkunnskaper inside "Opptak til
    # emnet"; <p>Obligatoriske aktiviteter:</p> sits inside "Undervisning".
    section_subheading_selector = "h3, h4, p"
  ),

  uis = list(
    code = "1160",
    strategy = "html_pdf_discovery",
    # The plan, without the page navigation, the contact footer (staff names)
    # and the facts box's links (timetable, Leganto search). Pages of
    # withdrawn course versions have no div.article, only a notice ("This
    # course version is no longer available ...").
    selector = "#block-page-content div.article",
    exclude = c(".content-navigation, .course-footer",
                paste0("//div[contains(@class, 'factbox--course') and not(contains(@class, 'factbox--exam'))]",
                       "//p[contains(@class, 'factbox__content-item')][.//a]")),
    facts = c("div.article__top", "div.factbox--course:not(.factbox--exam)"),
    plan_years = "url",
    url_year = "academic",
    # For a year its menu lacks, the course page is fetched: it shows the
    # latest version, the one selected in the menu ("2026 - 2027")
    page_term = function(html) semester_term(stringr::str_match(
      html, '<option value="[^"]*"\\s+selected>\\s*(\\d{4})\\s*-')[, 2], "Høst"),
    section_strategy = "html",
    # PDF plans (text_split fallback) open with a title and a metadata block;
    # the untitled paragraph after it is the course introduction.
    section_text_header = "^(?:Emnekode|Vekting|Semester|Antall semestre|Undervisningsspråk|Tilbys av)\\b[^:]*:"
  ),

  usn = list(
    code = "1176",
    strategy = "shadow_dom",
    plan_years = "url",
    # robots.txt (data/robots_txt.csv): Crawl-delay: 5 for all user agents;
    # the wait after each page shown in Chrome (resolve_urls_usn_batch())
    request_delay = 5,
    section_strategy = "text",
    # assessment holds "Arbeidskrav 2: ..." descriptions and gate lines
    section_inline_coursework = TRUE
  ),

  uit = list(
    code = "1130",
    strategy = "url_discovery",
    # The course code and title, the facts (campus, semester, credits, level)
    # and div.col-md-7.mainContent, which holds the plan's h2 sections (Om
    # emnet, Hva lærer du, Undervisning og pensum, Eksamen) plus the year
    # picker and the contact block with staff names, which .pre_uit() cuts;
    # not the breadcrumbs and the apply button with the application period,
    # nor the link labels to the university's exam page ("Alt du trenger å
    # vite om ... : UiTs samleside om eksamen", "Her finner du mer informasjon
    # om eksamen i nedlagte emner"; on English pages "Everything you need to
    # know about ... the exam ...: UiT Exams homepage")
    selector = "#maincontents",
    exclude = c("#maincontents .hero-container1 .offset-md-1, #maincontents .breadcrumb",
                paste0("//p[a[contains(@href, 'uit.no/eksamen') or contains(@href, 'uit.no/exams')]]",
                       "[normalize-space() = normalize-space(a) or starts-with(normalize-space(), 'Alt du trenger å vite om')",
                       " or starts-with(normalize-space(), 'Everything you need to know about')]")),
    facts = "#maincontents > section",
    plan_years = "url",
    post_fn = .pre_uit,
    section_strategy = "html",
    # 2008-2011 plans (old FS layout) head each field with a
    # div/span.fsemneoverskrift instead of an h2
    section_heading_selector = "h2, .fsemneoverskrift",
    # h3 "Pensum" under "Undervisning og pensum", "Mer info om arbeidskrav";
    # the 2020s exam box labels its gate table <strong class="d-block">
    # Obligatoriske arbeidskrav</strong>
    section_subheading_selector = "h3, strong.d-block",
    # 2012-2019 plans list arbeidskrav inside the Eksamen text
    section_inline_coursework = TRUE
  ),

  nmbu = list(
    code = "1173",
    strategy = "standard",
    selector = ".layout",
    # the title and the facts (credits, faculty, campus, language, frequency,
    # workload, teaching period)
    facts = c(".layout article header", ".layout section.grid-container"),
    plan_years = "current",
    # the link label to the AI categories ("Her finner du KI-kategoriene
    # beskrevet."), a paragraph of nothing else, in every plan
    exclude = "//p[a[contains(@href, '#kikategori')]][normalize-space() = normalize-space(a)]",
    # A course whose plan is only in English: the Norwegian page's "Om dette
    # emnet" (and every accordion) says "Se engelsk emnebeskrivelse" or "See
    # English text" (2025); /en/course/{code} has the plan, the same layout
    # with English headings
    english_page = list(
      notice = paste0('<h3>Om dette emnet</h3><div class="cke">\\s*(?:<em>\\s*)?',
                      '(?i:se engelsk (?:emnebeskrivelse|tekst|beskrivelse)|see english (?:text|version|description))',
                      '\\.?\\s*(?:</em>\\s*)?</div>'),
      from = "/emne/", to = "/en/course/"),
    section_strategy = "html",
    # sections are details accordions; the 2026-04 pages head them with
    # <summary> only, later pages put an <h3> inside it
    section_heading_selector = "h3, summary",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  samas = list(
    code = "0217",
    strategy = "noop",
    plan_years = "current",
    section_strategy = "noop"
  ),

  steiner = list(
    code = "8225",
    strategy = "pdf_split",
    plan_years = "current",
    section_strategy = "text",
    robots_permission = paste(
      "decided 2026-10-19 by the project lead, not granted by Steiner (not asked):",
      "the five subject PDFs under /dokumenter/ are open data, and fetching them",
      "makes negligible traffic"),
    # the PDF opens with the title and a metadata table (code and name,
    # English name, programme, credits, semester, language)
    section_text_header = paste0("^(?:Emnekode og (?:emne)?navn|Emnenavn|Engelsk emnenavn|",
                                 "Studieprogrammet emnet inngår i|Studiepoeng|Semester|",
                                 "Undervisningsspråk)\\s{2,}")
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

#' Institution short name for DBH institution codes
#'
#' A config's `code` can hold several DBH codes, e.g. when an institution got
#' a new code after a change of status (inn: 0264, 1177).
#'
#' @param institution_code Character vector of DBH institution codes
#' @return Character vector of short names, NA for codes no config has
institution_from_code <- function(institution_code) {
  codes <- lapply(institution_configs, `[[`, "code")
  lookup <- setNames(rep(names(codes), lengths(codes)), unlist(codes))
  unname(lookup[institution_code])
}

#' Get all institution configurations
#'
#' @return Named list of all institution configs
load_all_configs <- function() institution_configs
