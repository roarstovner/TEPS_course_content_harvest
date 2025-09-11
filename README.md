# TEPS Scraper Mini

Liten, selvstendig prosjekt-mal som viser hvordan du kan lagre CSS-selectors i YAML
og bruke dem i R-skript for scraping. Resultatet lagres som `.txt` for enkel manuell sjekk.

## Struktur
```
teps-scraper-mini/
├── R/
│   ├── utils_selectors.R
│   ├── scrape_course_page.R
│   ├── scrape_pdf.R
│   └── validate_selectors.R
├── config/
│   └── selectors.yaml
├── scripts/
│   └── demo_oslomet.R
├── tests/
│   └── test-validate.R
├── out/            # genererte .txt-filer
└── examples/       # evt. sample-URL-lister senere
```

## Avhengigheter
Installer ved behov:
```r
install.packages(c("yaml", "rvest", "xml2", "stringr", "pdftools"))
# (valgfritt) install.packages("testthat")
```

## Kjør eksempel
Fra prosjektroten:
```r
source("scripts/demo_oslomet.R")
```
Dette validerer selectors for `oslomet`, skraper `sample_url` fra YAML,
og skriver ut `out/oslomet_sample.txt` for manuell sjekk i PR.

## PDF-kilder (Steiner)
Bruk `scrape_pdf(url, out_txt_path)` for å lese tekst fra PDF.
For `steiner` er `pdf: true` satt i `selectors.yaml`, og HTML-validering hoppes over.

## Tips
- Hold selectors så stabile som mulig (id, data-*, aria-*).
- Unngå for dype CSS-kjeder; vurder XPath hvis struktur varierer.
- Kjør validering i CI (GitHub Actions) for tidlig varsling om brudd.
```

