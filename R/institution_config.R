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
#            version, course_plan_spec.md). inn, nord, uis, hiof, hvl (the
#            semester a course is first taught).
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
# html_fn: function(html, course_id) giving the HTML the block reader reads
#          in place of the stored page, NA for none: the offering's plan
#          where the page holds the plans of several years as data (vid's
#          JSON, vid_plan_html()). Its `selector` and `facts` name elements
#          of that HTML.
#
# robots_permission: the institution's permission to harvest paths its
#                robots.txt disallows for all user agents, recorded as
#                "granted YYYY-MM-DD by <who> (<what>)". Without one, the
#                harvest does not request a URL that a disallow rule in
#                data/robots_txt.csv matches (robots_allows(),
#                docs/harvesting.md "robots.txt"). A decision of the project's
#                own, taken without asking the institution, is recorded as such
#                ("decided YYYY-MM-DD by ..., not granted by ..."): steiner.

institution_configs <- list(

  oslomet = list(
    code = "1175",
    strategy = "standard",
    selector = "#main-content",
    # the portlet's name ("EPN-V2"), and in the facts row the year picker
    # ("Emnehistorikk") and the links to the reading list and timetable
    # ("Pensum", "Timeplan", each with its semesters)
    exclude = c("#main-content h2.portlet-title-text",
                paste0("//section[contains(@class, 'section-top')]//div[dl/dt[normalize-space() = 'Emnehistorikk' or ",
                       "normalize-space() = 'Pensum' or normalize-space() = 'Timeplan']]")),
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
    # content, and mostly every other part, is only a pointer to the English
    # page, some 60 wordings (2019-2025): "Se engelsk versjon", "Emnet
    # undervises på engelsk. Se engelsk versjon for emnebeskrivelse.", "Se
    # emnets engelske nettside - International MSc", "Se beskrivelse på
    # engelsk.", "Cf. English description", "Emnebeskrivelsen finnes kun på
    # engelsk.", "Undervises kun på engelsk" (2026-11-05). A paragraph that
    # goes on with more than a short tail ("Se den engelske beskrivelsen
    # dersom du finner ...") or starts with plan text has a plan of its own.
    # www.ntnu.edu has the plan, the same layout with English headings
    english_page = list(
      notice = paste0(
        '<p class="content-course-content">\\s*(?:<p>\\s*)?(?i:(?:(?:dette )?(?:emnet|faget) undervises ',
        '(?:kun )?på engelsk[.,]\\s*|the subject is taught in english\\.\\s*|vennligst\\s+|',
        'for fullstendig emnebeskrivelse,\\s*|\\.\\s*)?(?:(?:se|see|cf\\.)(?: derfor)?\\s+',
        "(?:the |den |emnets |the course's )?(?:engels|english)|se (?:emne|kurs)?beskrivelsen? på engelsk|",
        'see (?:the description )?in english|emnebeskrivelsen? (?:finnes|er) kun (?:tilgjengelig )?på engelsk|',
        'faglig innhold er beskrevet på engelsk|undervises kun på engelsk|',
        'you will find the course description in the english version))(?:[^<]|<br\\s*/?>){0,60}</p>'),
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
    # not the table of contents' heading (NIH's layout)
    exclude = "#vrtx-fs-emne-content h2.vrtx-toc-header",
    facts = c("#vrtx-fs-emne-content > h1", "#vrtx-fs-emne-facts"),
    plan_years = "url",
    user_agent = "browser",
    # The semesters in the title, "LSKNO22215 ... (Vår 2020)", "(Autumn 2024)"
    # on an English page, "(Høst 2013–Vår 2014)" for a course over two
    # semesters (title_terms()): the folder videre/ has pages whose title
    # names a later semester than the URL's, and a spring offering can take
    # the autumn page of a course over two semesters (vortex_match_urls())
    page_term = function(html) title_terms(html)$first,
    page_end_term = function(html) title_terms(html)$last,
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  himolde = list(
    code = "0232",
    # department folders, Norwegian and English, from the site's listings
    # (resolve_urls_himolde_batch()); the layout is NIH's and HiOF's (FS
    # pages in Vortex)
    strategy = "url_discovery",
    # The title, the facts list and the plan; not the course coordinator
    # (names), the links to the timetable and the reading list, the table of
    # contents' heading and the FS fetch stamp
    selector = "#vrtx-fs-emne-content",
    exclude = c(paste("#vrtx-fs-emne-content div.connected-links, #vrtx-fs-emne-content h2.vrtx-toc-header,",
                      "#vrtx-fs-emne-content .vrtx-date-info"),
                paste0("//div[@id='vrtx-fs-emne-facts']//div[contains(@class, 'dg')]",
                       "[starts-with(normalize-space(), 'Emneansvarlig') or ",
                       "starts-with(normalize-space(), 'Course Leader')]"),
                # nor a guest lecturer in the plan's text: a paragraph opening
                # with a bold "Lecturer", "<strong>Lecturer</strong>:<name>,
                # <university> (<e-mail>)", and, after a bare "Lecturer:", the
                # paragraph of the lecturer's biography under it (seminar
                # courses LOG904-130, IDR804-019; 2026-11-06)
                local({
                  p <- paste0("//div[@id='vrtx-fs-emne-content']//p[starts-with(normalize-space(), 'Lecturer')]",
                              "[*[1][self::strong or self::b][starts-with(normalize-space(), 'Lecturer')]]")
                  paste0(p, " | ", p, "[normalize-space() = 'Lecturer' or normalize-space() = 'Lecturer:']",
                         "/following-sibling::p[1]")
                }),
                # nor the reading-list widget: its label "Pensumoversikt" and
                # the titles of the course's Leganto lists under it (2026-11-06)
                local({
                  w <- paste0("//div[contains(@class, 'vrtx-fs-infotype-content')]/*[self::strong or self::p[strong]]",
                              "[normalize-space() = 'Pensumoversikt']")
                  paste0(w, " | ", w, "/following-sibling::ul[1][li//a[contains(@href, '/leganto/')]]")
                })),
    # and the programmes the course belongs to, which change from year to year
    facts = c("#vrtx-fs-emne-content > h1", "#vrtx-fs-emne-facts",
              local({
                h2 <- paste0("//h2[normalize-space() = 'Emnet er tilknyttet følgende studieprogram' or ",
                             "normalize-space() = 'The course is connected to the following study programs']")
                paste0(h2, " | ", h2, "/following-sibling::ul[1][contains(@class, 'vrtx-fs-programmes')]")
              })),
    plan_years = "url",
    # The semesters in the title, "(Vår 2024)", "(Autumn 2024)", or a course
    # over two semesters, "(Høst 2022–Vår 2023)" (title_terms()), filed under
    # its autumn (vortex_match_urls())
    page_term = function(html) title_terms(html)$first,
    page_end_term = function(html) title_terms(html)$last,
    section_strategy = "html",
    # Paragraph sub-headings ("Arbeidskrav") split sections, as at HiOF.
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
    # A course whose plan is only in English: the first paragraph of the
    # Norwegian page's content is only a pointer to the English page, some 30
    # wordings (2017-2025): "Emnebeskrivelsen finnes kun på engelsk, gå til
    # ...", "Emneomtalen finnest berre på engelsk", "Dette emnet er kun skildra
    # på engelsk. Klikk på "English" ...", "Dette emnet vert undervist på
    # engelsk. For å sjå emneskildringa ...", "Dette emnet kan du lese om på
    # dei engelske nettsidene våre.", "Se engelske nettsider for mer
    # informasjon", "Kursinformasjon finnes kun på engelsk. Se informasjon på
    # https://...", "You can read about this course on our English website.",
    # "This course will be taught in English, please see the English version
    # ..." (2026-11-08). A paragraph with more than a short tail, or followed
    # by more paragraphs (a Norwegian plan after "For English course
    # description, choose ..."), has a plan of its own. The English page has
    # the same layout
    english_page = list(
      notice = paste0(
        '<div class="l-2-col__main-content"[^>]*>\\s*(?:<h3[^>]*>[^<]*</h3>\\s*)?<p[^>]*>(?:<p>|</?a\\b[^>]*>|\\s)*(?i:',
        "(?:(?:emne|studieprogram)?(?:beskrivelsen|planen|omtalen?|skildringa)|kursinformasjon) ",
        "(?:finnes|finnast|finnest|finst|er) (?:kun|kunn|bare|berre) (?:tilgjengelig )?på engelsk|",
        "(?:dette )?emne[tr]? (?:er (?:kun |bare |berre )?(?:beskrevet|skildra) på engelsk|",
        "(?:er|undervises|vert undervist) på engelsk[.,!]? (?:klikk|for å sjå))|",
        "(?:dette emnet kan du lese om|du finn(?:er)? (?:mer )?(?:informasjon om dette emnet|emneplanen)|",
        "informasjon om emnet finn du) på dei? engelske nettsidene|",
        "(?:se|sjå) (?:<a[^>]*>\\s*)?(?:den )?engelske (?:nettsider|sida)|gå til våre enge?lske nettsider|",
        "you can read about this (?:course|topic) on our english website|",
        "this course will be taught in english, please see the english|",
        "to find information about the course you have to change language|",
        "for english course descript\\w*, choose|the course description is available in english only)",
        "(?:[^<]|<br\\s*/?>|</?a\\b[^>]*>|<p><a[^>]*></a>){0,150}</p>(?:\\s*</p>)*\\s*(?:<h3|</div>)"),
      from = "/studier/studieprogram/emne/", to = "/en/studies-at-hvl/study-programmes/courses/"),
    # The facts' teaching semester may say when the course is first taught
    # ("Undervisningssemester: Høst. Undervises første gang høsten 2023.",
    # "Vår. Første gang våren 2021", "Semester of instruction: Autumn
    # Semester - first time autumn 2023"): a page for a year before it shows
    # a later version
    page_term = function(html) {
      m <- stringr::str_match(html, paste0(
        "(?i)(?:Undervisningssemester|Semester of instruction):[^<]*?(?:f(?:ø|y)rste (?:gang|gong|undervisning)|",
        "first time) (?:i )?(høst|haust|vår|autumn|fall|spring)\\w* (\\d{4})"))
      semester_term(m[, 3], ifelse(tolower(m[, 2]) %in% c("høst", "haust", "autumn", "fall"), "Høst", "Vår"))
    },
    section_strategy = "html",
    section_heading_selector = "h3",
    # Paragraph sub-headings ("Arbeidskrav", "Faget i praksis") split sections.
    section_subheading_selector = "p"
  ),

  khio = list(
    code = "6220",
    # a page per course version, /emner/{code}-{version} (DBH's code with
    # its version number); a version no longer taught keeps a page with the
    # headings and no text (fetch_html_cols_single_khio())
    strategy = "standard",
    fetch_fn = fetch_html_cols_single_khio,
    # The title, the facts list and the plan
    selector = "main section.content",
    # the title and the facts list (credits, code, the programmes the course
    # belongs to, which change from year to year)
    facts = c("main section.content h1", "main section.content .programme-meta"),
    # the page shows the version's plan as it is now, without a year
    plan_years = "current",
    # h2s ("Kort beskrivelse av emnet", "Læringsutbytte for emnet",
    # "Undervisning og læringsformer", "Vurdering"); coursework under a
    # paragraph sub-heading ("Arbeidskrav:")
    section_strategy = "html",
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

  nhh = list(
    code = "1240",
    # one page per course, named from its title (/emner/{slug}/), found with
    # the site search (resolve_urls_nhh_batch())
    strategy = "url_discovery",
    # The title, the plan's accordion and the facts aside; not the course
    # responsible box (names) and the link to the next semester's version
    # ("Vår 2027", ?displayNextTerm=true)
    selector = "main#mainContent",
    exclude = "#mainContent .extraContent, #mainContent .expandableList__heading a",
    # the code and title, the semester the plan is for ("Høst 2026") and the
    # facts box (credits, language, teaching semester, the programmes the
    # course belongs to)
    facts = c("#mainContent h1", "#mainContent .expandableList__heading", "#mainContent .aboutStudyBlock"),
    plan_years = "current",
    # Accordion items, each an h2 ("Innhold", "Læringsutbytte", "Topics")
    section_strategy = "html"
  ),

  nmh = list(
    code = "1210",
    # a study handbook per cohort, a page per course named from its title,
    # found in the handbooks' course lists (resolve_urls_nmh_batch())
    strategy = "url_discovery",
    # The title, the facts box and the plan; not the breadcrumb ("Emner") and
    # the publication stamp ("Publisert: 3. apr. 2020 — Oppdatert: ...")
    selector = "main#main",
    exclude = "main#main nav.breadcrumbs__main, main#main > p.typo-13pt",
    # the title and the facts box (code, level, category, credits, cohort)
    facts = c("main#main header", "main#main .metadata__main"),
    # the handbook's cohort, the academic year it starts (resolve_urls_nmh_batch())
    plan_years = "url",
    # h2s ("Kort om emnet", "Læringsmål", "Arbeidskrav", "Avsluttende vurdering")
    section_strategy = "html"
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
    # "Adgangsregulering" mixes admission ("Emnet er kun åpent for studenter
    # på ...") with the courses the course requires ("Bestått 90 stp. i
    # masterfaget og bestått FoU-oppgave."), which go to prerequisites
    section_headings = c("adgangsregulering" = ".admission"),
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

  vid = list(
    code = "8208",
    # one page per course, named from its title (/studier/emner/{slug}-{code},
    # a few with a numeric id after it), found in the sitemap
    # (resolve_urls_vid_batch()); a page without any plan counts as not found
    # (fetch_html_cols_single_vid())
    strategy = "url_discovery",
    fetch_fn = fetch_html_cols_single_vid,
    # the page holds, as JSON, the plan of every year and semester the course
    # is taught in from spring 2024, so every DBH year from then is harvested;
    # the offering's plan is read from it as HTML
    plan_years = "url",
    html_fn = vid_plan_html,
    selector = "main",
    # the title and the facts list (code, credits, language, campus, duration,
    # the programmes the course belongs to)
    facts = c("main h1", "main dl.facts"),
    # an h2 per paragraph and accordion of the plan ("Emnets innhold",
    # "Læringsutbytte", "Vurderingsordning")
    section_strategy = "html"
  ),

  phs = list(
    code = "1301",
    # /studier/emner/{year}/{var|host}/{code}.html from 2022, or the English
    # tree's /english/studies/courses/{year}/{spring|autumn}/; the listings
    # (resolve_urls_phs_batch()) hold every page, a quarter of DBH's
    # offerings. The layout is HiMolde's (FS pages in Vortex)
    strategy = "url_discovery",
    # the English pages have no plan: not found (fetch_html_cols_single_phs())
    fetch_fn = fetch_html_cols_single_phs,
    # The title, the facts list (credits, department, semester, duration) and
    # the plan, which here sit side by side; not the reading-list link, the
    # table of contents' heading and the FS fetch stamp
    selector = "#vrtx-content",
    exclude = "#vrtx-content div.connected-links, #vrtx-content h2.vrtx-toc-header, #vrtx-content .vrtx-date-info",
    # and the programmes the course belongs to (an empty list on most pages)
    facts = c("#vrtx-content > h1", "#vrtx-fs-emne-facts",
              local({
                h2 <- paste0("//h2[normalize-space() = 'Emnet er tilknyttet følgende studieprogram' or ",
                             "normalize-space() = 'The course is connected to the following study programs']")
                paste0(h2, " | ", h2, "/following-sibling::ul[1][contains(@class, 'vrtx-fs-programmes')]")
              })),
    plan_years = "url",
    # "(Høst 2025–Vår 2026)": a course over two semesters is filed under its
    # autumn, as at HiMolde
    page_term = function(html) title_terms(html)$first,
    page_end_term = function(html) title_terms(html)$last,
    # h2s: Innledning, Læringsutbytte, Kunnskap, Ferdigheter, Generell
    # kompetanse, Organisering og læringsaktiviteter, Obligatorisk aktivitet,
    # Vurdering
    section_strategy = "html"
  ),

  fhs = list(
    code = "1303",
    # On Forsvaret's site: /utdanning/emner/{CODE}/{year}-{HØST|VÅR}, a page
    # per version of the plan from autumn 2018, listed on the code's page
    # (resolve_urls_fhs_batch(): the version in force in the offering's
    # semester)
    strategy = "url_discovery",
    # The title, the facts box (credits, academic year, programmes, English
    # title) and the plan; not the "Oppdatert" stamp and the versions menu
    selector = "#main-container article",
    exclude = "#main-container article .article__meta, #main-container article .education-banner__dropdown",
    facts = c("#main-container article h1", "#main-container article .education-banner"),
    plan_years = "url",
    # "Hva lærer du" (bold paragraphs: "Emnets temaer") before the accordions
    # (h3s: Læringsutbytte, Læringsaktiviteter, Pensum, Arbeidskrav og
    # vurdering)
    section_strategy = "html",
    section_heading_selector = "h3.accordion__panel-title",
    section_scope = "li.accordion__panel",
    section_initial = "course_content",
    # Paragraphs switch sections, except those naming a gate ("Arbeidskrav",
    # "Obligatorisk arbeidskrav:") in "Arbeidskrav og vurdering": they and
    # the exam lines after them stay in assessment, where
    # section_inline_coursework moves the gate's lines (up to
    # "Vurderingsform:") to coursework_requirements; and "Emnets oppbygning:"
    # with a colon, which lists the course's modules (MILM4106, 2018-2022),
    # not how it is run, so they stay in course_content
    section_subheading_selector = paste0(
      "//p[not(ancestor::li[contains(@class, 'accordion__panel')]",
      "[.//h3[normalize-space() = 'Arbeidskrav og vurdering']]",
      " and (contains(., 'rbeidskrav') or contains(., 'bligatorisk')))",
      " and normalize-space() != 'Emnets oppbygning:']"),
    # Sub-headings of "Hva lærer du" mapped here, before the heading map:
    # "Opptakskrav" holds the courses the course requires ("Emnet forutsetter
    # bestått eksamen i ... SPR1720 og SPR1721"), "Målgruppe" who may take it;
    # the others (Etterretningsskolen) say how the course is run
    section_headings = c("opptakskrav" = "prerequisites", "målgruppe" = ".drop",
                         "emnets oppbygning" = "teaching_methods", "emnets oppbygging" = "teaching_methods",
                         "arbeidsformer og læringsaktiviteter" = "teaching_methods"),
    section_inline_coursework = TRUE
  ),

  fjellhaug = list(
    code = "8234",
    # a page per course code, /emne/{slug} or /en/subjects/{slug}, the slug
    # sometimes with the title, found in the sitemaps
    # (resolve_urls_fjellhaug_batch())
    strategy = "url_discovery",
    # The title, the facts box and the plan; not the breadcrumb, the menu of
    # the plan's parts, and the mobile copy of the plan (accordions after it)
    selector = "main#main",
    exclude = paste("main#main nav[aria-label='breadcrumb'], main#main nav.sticky-nav,",
                    "main#main section.content-stick-nav-mobile"),
    # the title and the facts box (code, credits, campus, language, start,
    # duration)
    facts = "main#main section.hero",
    # the page shows the plan in force, without a year
    plan_years = "current",
    # h4s ("Emnets innhold", "Læringsmål", "Vurdering og eksamen",
    # "Pensumlitteratur"); bold paragraph sub-headings ("Arbeidskrav")
    section_strategy = "html",
    section_heading_selector = "h4",
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
