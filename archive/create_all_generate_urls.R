# 1) Finn alle generate_urls_*.R filer i R/-mappene
files <- list.files("R", pattern = "generate_urls_.*\\.R$", 
                    recursive = TRUE, full.names = TRUE)

# 2) Les inn og slå sammen linjene
all_code <- unlist(lapply(files, readLines, warn = FALSE))

# 3) Skriv til en ny fil i R-mappa
out_file <- file.path("R", "all_generate_urls.R")
writeLines(all_code, out_file, useBytes = TRUE)

cat("Samlet kode skrevet til", out_file, "\n")
