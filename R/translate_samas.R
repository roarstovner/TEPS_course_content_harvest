# R/translate_samas.R
#
# Translate Samas (Sámi University) course plans from Sami to Norwegian.
# Uses ellmer::batch_chat_text() with Anthropic for 50% cost reduction.
#
# Usage:
#   source("R/translate_samas.R")
#   # Reads data/html_samas.RDS, adds fulltext_no column, saves back.

library(ellmer)

translate_samas <- function(
    input_path = "data/html_samas.RDS",
    batch_path = "data/checkpoint/samas_translation_batch.json",
    model = "claude-sonnet-4-6"
) {
  df <- readRDS(input_path)

  needs_translation <- !is.na(df$fulltext)

  if (sum(needs_translation) == 0) {
    message("No fulltext to translate.")
    return(df)
  }

  message("Translating ", sum(needs_translation), " course plans...")

  chat <- chat_anthropic(
    system_prompt = paste(
      "You are a professional translator specializing in Sami languages (North Sami, Lule Sami, South Sami) and Norwegian. Translate the following course plan text to Norwegian (Bokmål).

      Preserve the document structure (headings, lists, paragraphs).

      If parts of the text are already in Norwegian, keep them as-is.
      Output only the translated text, no commentary."
    ),
    model = model,
    echo = "none"
  )

  prompts <- as.list(df$fulltext[needs_translation])

  translations <- batch_chat_text(
    chat = chat,
    prompts = prompts,
    path = batch_path
  )

  df$fulltext_no <- NA_character_
  df$fulltext_no[needs_translation] <- translations

  saveRDS(df, input_path)
  message("Done. Saved ", sum(!is.na(df$fulltext_no)), " translations to ", input_path)

  invisible(df)
}

if (sys.nframe() == 0) {
  translate_samas()
}
