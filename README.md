
# 📚 TEPS Course Content Harvest

Et R-basert rammeverk for å hente og strukturere emnebeskrivelser fra norske lærerutdanningsinstitusjoner.\
Pipelinen er **konfigurerbar via YAML** og produserer per institusjon **URL-lister, HTML og fulltekst**.

------------------------------------------------------------------------

## ✨ Kort oppsummert

-   **Input:** institusjon, kurskode, år, semester (H/V)\
-   **Output:** URL-er, HTML, renset fulltekst og QA-status\
-   **Dekning:** 17 institusjoner (per oktober 2025)\
-   **Design:** alt styres fra YAML-filer – ingen hardkoding i R-koden\
-   **Motor:** Chromote + `rvest` + `xml2` + YAML

------------------------------------------------------------------------

## 🛠️ Input

Hovedinput finnes i:

-   `data/input/courses.RDS`\
-   `data-raw/courses.xlsx`

**Eksempel (R):**

``` r
head(courses)
#   institution_short course_code year semester
# 1           oslomet    MGVM4100 2024     H
# 2               uia      NO-155 2024     V
```

**Institusjoner dekket foreløpig (17):**

``` text
hiof, hivolda, hvl, inn, mf, nih, nla, nmbu, nord,
ntnu, oslomet, uia, uib, uio, uis, uit, usn
```

------------------------------------------------------------------------

## 📤 Output

For hver institusjon opprettes en egen mappe i `data/output/` med:

-   **Tidsstemplete filer**\
    `course_urls_<inst>_YYYYMMDD-HHMM.csv` og `.txt`
-   **Latest-alias**\
    `course_urls_latest.csv` og `.txt` (peker alltid til nyeste batch)

**Eksempel (`data/output/uio`):**

``` text
course_urls_uio_20250912-1114.csv
course_urls_uio_20250912-1114.txt
course_urls_latest.csv
course_urls_latest.txt
```

> **Merk:** Enkelte institusjoner kan ha alternative prefiks/filnavn (f.eks. `candidates_*`) iht.
> konfigurasjon.

------------------------------------------------------------------------

## 🔄 Pipeline

| Steg | Skript | Beskrivelse |
|:----------------:|:--------------------|:---------------------------------|
| 1️⃣ | `scripts/01_prepare_input.R` | Standardiserer input (institusjon, kurskode, år, semester) |
| 2️⃣ | `scripts/02_generate_urls.R` | Genererer URL-er fra YAML-mønstre |
| 3️⃣ | `scripts/03_scrape_chromote_only.R` | Rendre og lagre HTML med Chromote |
| 4️⃣ | `scripts/04_parse_html.R` | Parse HTML → renset tekst (Markdown-lignende format) |
| 6️⃣ | `scripts/06_arbeidskrav_manually.R` | Ekstraherer *Arbeidskrav*-seksjoner |
| 7️⃣ | `scripts/05_qacheck.R` | QA-sjekk av seksjoner og tekstlengde |

Alle parametere styres gjennom miljøvariabler i `00_run_all.R`.

------------------------------------------------------------------------

## 🕸️ Steg 3 – Scraping med Chromote

Chromote brukes for å gjengi nettsider som laster innhold dynamisk via JavaScript.\
HTML-filene lagres i `data/output/<inst>/html_raw/`.

### 💻 Miljøvariabler (settes i `00_run_all.R`)

``` r
Sys.setenv(
  TEPS_RUN_SCRAPE   = "TRUE",
  TEPS_CHROMOTE_ONLY = "TRUE",
  TEPS_SAVE_HTML    = "TRUE",
  TEPS_SAVE_TXT     = "TRUE"
)
```

Dette sikrer enhetlig rendring for alle institusjoner, uavhengig av CMS (Vortex, Drupal, Liferay, CorePublish, SPA).

------------------------------------------------------------------------

## 🧱 Steg 4 – Parsing og rensing

**Fil:** `R/parse_html_generic.R`

Parseren: - Velger største hovedcontainer automatisk (`.pick_main()`).
- Fjerner navigasjon, topp-/bunnmenyer, breadcrumbs, script- og style-elementer.
- Ekstraherer avsnitt, punktlister og overskrifter.
- Normaliserer whitespace og duplikater.
- Promoterer seksjonsoverskrifter (f.eks. *Læringsutbytte* → `## Læringsutbytte`).
- Bruker `config/selectors.yaml` og `config/default.yaml` for alle regler.
- Har fallback-logikk for korte eller manglende tekster.

### 📁 Output

Renset tekst lagres som `.txt`-filer i `data/output/<inst>/txt_clean/`\
og som en samlet CSV per institusjon (`courses_clean.csv`).

Eksempel:

``` text
data/output/uio/
├── html_raw/
├── txt_clean/
│   ├── ENG47901_2024__chromote.txt
│   ├── PSY1001_2025_H_chromote.txt
├── courses_clean.csv
```

------------------------------------------------------------------------

## ⚙️ Konfigurasjon

### `config/institutions.yaml`

URL-mønstre og semester-stil per institusjon:

``` yaml
url_pattern: "https://www.uio.no/studier/emner/{year}/{semester_url}/{course_code}/index.html"
semester_style: "host_vaar"
```

### `config/selectors.yaml`

CSS-selectors for å hente hovedinnhold:

``` yaml
uio:
  selector_main: "#vrtx-main-content, #vrtx-content, #vrtx-course-content, main, article"
  selector_exclude:
    - "header"
    - "footer"
    - "nav"
    - "aside"
    - ".breadcrumb"
    - ".menu"
    - ".sidebar"
    - "script"
    - "style"
  fallback_main: "main, article, #content"
```

> Hver institusjon har egne regler, uten YAML-alias for maksimal kompatibilitet.

### `config/default.yaml`

Globale parsing-regler:

``` yaml
min_chars: 400
keep_after:
  - "(?i)om emnet"
  - "(?i)emnebeskrivelse"
stop_before:
  - "(?i)kontakt"
  - "(?i)personvern"
sections:
  Læringsutbytte:
    - "(?i)^\s*læringsutbytte"
  Vurdering:
    - "(?i)^\s*vurdering"
  Pensum:
    - "(?i)^\s*pensum"
```

------------------------------------------------------------------------

## 🧾 Outputstruktur

``` text
data/output/
├── <inst>/
│   ├── html_raw/          # Originale Chromote-filer
│   ├── txt_clean/         # Renset tekst
│   ├── courses_clean.csv  # Samlet tekst per kurs
│   └── course_urls_latest.csv
└── _aggregated/
    ├── courses_clean_all.csv
    └── qa_section_check.csv
```

------------------------------------------------------------------------

## 🔍 Steg 5 – QA: Seksjonssjekk

**Fil:** `scripts/05_qacheck.R`

Etter parsing kjøres QA-skriptet som sjekker at teksten inneholder minst én av følgende hovedseksjoner:

```         
Læringsutbytte, Arbeidskrav, Vurdering, Undervisning, Pensum
```

Resultatet lagres som CSV:

```         
data/output/_aggregated/qa_section_check.csv
```

**Eksempel (kort utdrag):**

```         
institution | status                   | n_files
-------------|--------------------------|---------
uis          | Mangler hovedseksjoner   | 75
uio          | OK                       | 47
hiof         | OK                       | 56
```

------------------------------------------------------------------------

## 🧩 Steg 6 – Arbeidskrav-ekstraksjon

**Fil:** `scripts/06_arbeidskrav_manually.R`

Etter at kurs­tekstene er renset i steg 4, trekkes ut egne deltekster for
seksjonen **Arbeidskrav** (hvis den finnes).  
Skriptet bruker regulære uttrykk for å finne teksten mellom overskriften
`## Arbeidskrav` og neste seksjon.

### 📘 Metode

- søker etter linjer som matcher `(?i)^##\\s*arbeidskrav`
- lagrer alt frem til neste `##`-overskrift
- fjerner overflødig whitespace, HTML-rester og punktmerking
- legger resultatet i ny kolonne `arbeidskrav` i `courses_clean.csv`

### 📁 Output

Hver institusjons `courses_clean.csv` får nå kolonner:

| Kolonne | Beskrivelse |
|----------|--------------|
| `institution` | institusjonsforkortelse |
| `course_code` | kurskode hentet fra filnavn |
| `url` | original lenke til emnesiden |
| `fulltekst_renset` | hele rensede emneteksten |
| `arbeidskrav` | utdrag mellom *## Arbeidskrav* og neste seksjon |
| `status_code` | (valgfritt) HTTP-status fra scraping |

### 💾 Filplassering

```text
data/output/<inst>/courses_clean.csv
```

Hvis `Arbeidskrav` ikke finnes i teksten, blir kolonnen tom, men
beholdes for strukturens skyld.

------------------------------------------------------------------------

## 🔍 Steg 7 – Kvalitetskontroll (QA)

**Fil:** `scripts/05_qacheck.R`

Skriptet kontrollerer at hver renset kurs­tekst inneholder
hovedseksjoner som *Læringsutbytte*, *Arbeidskrav*, *Vurdering*,
*Undervisning* eller *Pensum*, og at teksten ikke er for kort.

### 📊 Hva som sjekkes

| Parameter | Forklaring |
|------------|-------------|
| `found_sections` | antall forekomster av nøkkelord |
| `n_chars` | antall tegn i teksten |
| `status` | OK / Mangler hovedseksjoner / For kort |

### 📁 Output

To CSV-rapporter lagres i `_aggregated`-mappen:

```text
data/output/_aggregated/qa_section_check_detailed.csv
data/output/_aggregated/qa_section_summary.csv
```

**Eksempel på sammendrag:**

```text
institution | status                    | n_files
-------------|---------------------------|---------
uis          | Mangler hovedseksjoner    | 75
uio          | OK                        | 47
hiof         | For kort / mulig feil sel | 4
```

------------------------------------------------------------------------

## ⚙️ Miljøvariabler for nye steg

I `scripts/00_run_all.R` kan du aktivere eller deaktivere disse delene
av pipelinen:

```r
Sys.setenv(
  TEPS_RUN_ARBEIDSKRAV = "TRUE",  # kjør arbeidskrav-ekstraksjon
  TEPS_RUN_QA_CHECK     = "TRUE"  # kjør QA-sjekk
)
```

Begge kjører automatisk etter parsing-steget dersom variablene står til
`TRUE`.

------------------------------------------------------------------------

## ▶️ Hvordan kjøre

### Én-klikks master-runner

``` r
source("scripts/00_run_all.R")
```

Velg hvilke institusjoner du vil kjøre:

``` r
TEPS_INST <- ""       # tom streng = alle, f.eks. "uio,usn" for utvalg
DEFAULT_RUN_SCRAPE <- TRUE
TEPS_CHROMOTE_ONLY <- TRUE
```

### Manuell kjøring

``` r
source("scripts/01_prepare_input.R")
source("scripts/02_generate_urls.R")
source("scripts/03_scrape_chromote_only.R")
source("scripts/04_parse_html.R")
source("scripts/05_qacheck.R")
```

------------------------------------------------------------------------

## 📑 Modes og tokens

### 🔧 MODE

`MODE` bestemmer hvilke år og semestre som genereres for en institusjon:

-   `hv` → generer både høst (`YEAR_H`) og vår (`YEAR_V`)\
-   `single` → generer kun for ett bestemt år (`SINGLE_YEAR`)\
-   `next` → generer kun neste semester (automatisk, basert på dato)\
-   `both` → generer både nåværende og neste semester

**Eksempel (`oslomet` med MODE = single):**

``` r
inst_short  <- "oslomet"
MODE        <- "single"
SINGLE_YEAR <- 2025
```

→ Genererer bare kurs-URL-er for 2025.

------------------------------------------------------------------------

### 🔡 Tokens for kurskoder

Mange institusjoner bruker ulike varianter av kurskoder (`MGVM4100`, `NO-155`, `PSY-1010`).
For å få konsistente URL-er brukes flere “tokens” i YAML-mønstrene:

-   `{course_code}` → original kurskode fra input\
-   `{course_code_norm}` → standardisert kurskode (uten whitespace/feil)\
-   `{code_upper}` → versjon i UPPERCASE\
-   `{code_upper_nodash1}` → samme som over, men uten første bindestrek\
-   `{code_base}` → baseversjon (uten suffix eller årstall)

**Eksempel:**

``` yaml
url_pattern: "https://www.uib.no/emne/{code_upper_nodash1}"
```

------------------------------------------------------------------------

### 📤 Hvordan dette påvirker output

Når `02_generate_urls.R` kjøres, kombineres:

-   **MODE** → styrer hvilke år/semestre som inkluderes\
-   **Tokens** → fyller inn placeholders i URL-mønstrene fra YAML

Resultatet lagres som tidsstemplete filer per institusjon i `data/output/<inst>/`, samt `course_urls_latest.*` som peker på den nyeste batchen.

------------------------------------------------------------------------

## 📂 Prosjektstruktur

``` text
├── config/                  # YAML-konfigurasjon (URL-mønstre + CSS-selectors)
├── data/
│   ├── input/               # Input (Excel/RDS)
│   ├── cache/               # Standardisert cache (RDS)
│   └── output/              # Output per institusjon (17 mapper)
├── data-raw/                # Opprinnelige Excel/selector-utkast
├── R/                       # Fellesfunksjoner + institusjonsspesifikke moduler
├── scripts/                 # Hovedpipeline (01–03)
├── tests/                   # Tester/eksperimenter
├── README.md                # Denne filen (vises på GitHub)
├── README.qmd               # (valgfritt) Quarto med kjørbar kode → kan rendre til MD
└── TEPS_course_content_harvest.Rproj
```

------------------------------------------------------------------------

## 📌 Notater

-   **Tegnkoding:** UTF-8 for all I/O\
-   **Formater:** CSV (output), YAML (config), RDS (cache/mellomlagring)\
-   **Tidsstempel:** `format(Sys.time(), "%Y%m%d-%H%M")` i filnavn\
-   **`latest`-filer:** peker alltid til nyeste batch\
-   **Utvidelser:** nye institusjoner legges til via YAML (ingen endring i R-kode nødvendig)

------------------------------------------------------------------------
