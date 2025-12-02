# ========== 1. Load packages ==========
# library(dplyr)
# library(readxl)
# library(purrr)
# library(glue)

# ========== 2. Load data ==========
courses <- read_excel("courses.xlsx")

# ========== 3. Helper: Standardise semester ==========
standardize_semester <- function(semester, style = c("uia", "ntnu", "inn", "url", "hiof", "nih")) {
  style <- match.arg(style)
  sem <- tolower(semester)
  
  case_when(
    style == "uia" & sem %in% c("h??st", "host", "autumn") ~ "host",
    style == "uia" & sem %in% c("v??r", "var", "spring") ~ "var",
    
    style == "ntnu" & sem %in% c("h??st", "host", "autumn") ~ "1",
    style == "ntnu" & sem %in% c("v??r", "var", "spring") ~ "2",
    
    style == "inn" ~ paste0(tolower(gsub("\u00e5", "a", semester))),
    style == "url" ~ URLencode(tolower(semester), reserved = TRUE),
    
    style == "hiof" & sem %in% c("v??r", "spring") ~ "var",
    style == "nih" & sem %in% c("h??st", "host", "autumn") ~ "host",
    
    TRUE ~ semester
  )
}

# ========== 4. Create URL based on institution ==========
create_url <- function(institution, course_code, year, semester) {
  institution <- tolower(institution)
  course_code <- trimws(course_code)
  
  # Common encoded formats
  course_code_enc <- URLencode(course_code, reserved = TRUE)
  semester_url <- standardize_semester(semester, "url")
  
  switch(institution,
         
         "oslomet" = glue("https://student.oslomet.no/studier/-/studieinfo/emne/{course_code}/{year}/{semester_url}"),
         
         "uia" = glue("https://www.uia.no/studier/emner/{year}/{standardize_semester(semester, 'uia')}/{tolower(course_code)}.html"),
         
         "ntnu" = glue("https://www.ntnu.no/studier/emner/{toupper(course_code)}/{year}/{standardize_semester(semester, 'ntnu')}#tab=omEmnet"),
         
         "inn" = glue("https://studiekatalog.edutorium.no/inn/nb/emne/{course_code}/{year}-{standardize_semester(semester, 'uia')}"),
         
         "hivolda" = glue("https://www.hivolda.no/emne/{course_code}"),
         
         "hiof" = glue("https://www.hiof.no/studier/emner/lusp/lusp/{year}/{standardize_semester(semester, 'hiof')}/{tolower(course_code)}.html"),
         
         "hvl" = glue("https://www.hvl.no/studier/studieprogram/emne/{course_code}"),
         
         "mf" = glue("https://mf.no/studier/emner/{tolower(course_code)}"),
         
         "nla" = glue("https://www.nla.no/studietilbud/emner/{year}/{tolower(course_code)}/"),
         
         "nord" = glue("https://www.nord.no/studier/emner/{tolower(course_code)}"),
         
         "nih" = glue("https://www.nih.no/studier/emner/{year}/{standardize_semester(semester, 'nih')}/{tolower(course_code)}.html"),
         
         "uib" = glue("https://www4.uib.no/studier/emner/{course_code}"),
         
         "uio" = glue("https://www.uio.no/studier/emner/uv/ils/{course_code}/index.html"),
         
         "uis" = glue("https://www.uis.no/nb/course/{course_code}"),
         
         "usn" = glue("https://www.usn.no/studier/studie-og-emneplaner/#/emne/{course_code}_{year}_{semester_url}"),
         
         "uit" = glue("https://uit.no/utdanning/aktivt/emne/{course_code}"),
         
         "samas" = NA_character_,
         
         "steiner" = NA_character_,
         
         NA_character_  # fallback
  )
}

# ========== 5. Apply function to all rows ==========
courses <- courses %>%
  mutate(
    url = pmap_chr(
      list(institution_short, course_code, year, semester),
      ~ create_url(..1, ..2, ..3, ..4)
    )
  )

# ========== 6. View and save ==========
View(courses)

write.xlsx(courses, "course_urls_generated.xlsx", overwrite = TRUE)





