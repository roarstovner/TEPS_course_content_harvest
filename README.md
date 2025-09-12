# 📚 TEPS Course Content Harvest

Et R-basert rammeverk for å hente og strukturere emnebeskrivelser fra norske lærerutdanningsinstitusjoner.  
Pipelinen er **konfigurerbar via YAML** og produserer per institusjon **URL-lister, HTML og fulltekst**.

---

## ✨ Kort oppsummert

- **Input:** institusjon, kurskode, år, semester (H/V)  
- **Output:** URL-er, HTML, fulltekst  
- **Dekning:** 17 institusjoner (per september 2025)  
- **Design:** konfigurasjon i YAML → ingen hardkoding i R-koden

---

## 🛠️ Input

Hovedinput finnes i:

- `data/input/courses.RDS`  
- `data-raw/courses.xlsx`

**Eksempel (R):**
```r
head(courses)
#   institution_short course_code year semester
# 1           oslomet    MGVM4100 2024     H
# 2               uia      NO-155 2024     V
```

**Institusjoner dekket foreløpig (17):**
```text
hiof, hivolda, hvl, inn, mf, nih, nla, nmbu, nord,
ntnu, oslomet, uia, uib, uio, uis, uit, usn
```

---

## 📤 Output

For hver institusjon opprettes en egen mappe i `data/output/` med:

- **Tidsstemplete filer**  
  `course_urls_<inst>_YYYYMMDD-HHMM.csv` og `.txt`
- **Latest-alias**  
  `course_urls_latest.csv` og `.txt` (peker alltid til nyeste batch)

**Eksempel (`data/output/uio`):**
```text
course_urls_uio_20250912-1114.csv
course_urls_uio_20250912-1114.txt
course_urls_latest.csv
course_urls_latest.txt
```

> **Merk:** Enkelte institusjoner kan ha alternative prefiks/filnavn (f.eks. `candidates_*`) iht. konfigurasjon.

---

## 🔄 Pipeline

### 1) Prepare input — `scripts/01_prepare_input.R`
- Standardiserer kolonner (institusjon, kurskode, år, semester)
- Lager hjelpe-tokens ved behov
- Skriver `data/cache/courses_std.RDS`

### 2) Generate URLs — `scripts/02_generate_urls.R`
- Leser URL-mønstre fra `config/institutions.yaml`
- Erstatter tokens (`{course_code}`, `{year}`, `{semester_url}`) robust
- Håndterer institusjonsspesifikke semester-stiler (`H/V`, `host/vaar`, `1/2`, …)
- Eksporterer tidsstemplete + `latest`-filer per institusjon

### 3) Scrape fulltekst — `scripts/03_scrape.R`
- Leser CSS-selectors fra `config/selectors.yaml`
- Parser HTML → `fulltext`
- Rydder whitespace/linjeskift
- Skriver strukturerte resultater til `data/output/`

**Valgfritt:** `R/verify.R` kan sjekke HTTP-status (200/404) og logge resultater.

---

## ⚙️ Konfigurasjon

**`config/institutions.yaml`** — URL-mønstre og semester-stil per institusjon:
```yaml
url_pattern: "https://www.uio.no/studier/emner/{year}/{semester_url}/{course_code}/index.html"
semester_style: "host_vaar"   # alternativer: plain (H/V), host_vaar, ntnu (1/2), m.fl.
```

**`config/selectors.yaml`** — CSS-selector for å hente fulltekst per institusjon:
```yaml
fulltext: ".oslomet-margin-wrapper-top"
```

---

## 📑 Modes og tokens

### 🔧 MODE
`MODE` bestemmer hvilke år og semestre som genereres for en institusjon. Dette gjør det enkelt å styre om vi skal hente både høst og vår, eller bare én spesiell årgang.

- `hv` → generer både høst (`YEAR_H`) og vår (`YEAR_V`)  
- `single` → generer kun for ett bestemt år (`SINGLE_YEAR`)  
- `next` → generer kun neste semester (automatisk, basert på dato)  
- `both` → generer både nåværende og neste semester  

**Eksempel (`oslomet` med MODE = single):**
```r
inst_short  <- "oslomet"
MODE        <- "single"
SINGLE_YEAR <- 2025
```
→ Genererer bare kurs-URL-er for 2025.

---

### 🔡 Tokens for kurskoder
Mange institusjoner bruker ulike varianter av kurskoder (`MGVM4100`, `NO-155`, `PSY-1010`). For å få konsistente URL-er lager vi flere “tokens” som kan brukes i YAML-mønstrene:

- `{course_code}` → original kurskode fra input  
- `{course_code_norm}` → standardisert kurskode (uten whitespace/feil)  
- `{code_upper}` → versjon i UPPERCASE  
- `{code_upper_nodash1}` → samme som over, men uten første bindestrek  
- `{code_base}` → baseversjon (uten suffix eller årstall)  

Dette gjør at YAML-konfigurasjon kan se slik ut:
```yaml
url_pattern: "https://www.uib.no/emne/{code_upper_nodash1}"
```
i stedet for å hardkode regler for hver variant i R-koden.

---

### 📤 Hvordan dette påvirker output
Når `02_generate_urls.R` kjøres, kombineres:

- **MODE** → styrer *hvilke år/semestre* som inkluderes  
- **Tokens** → fyller inn placeholders i URL-mønstrene fra YAML  

Resultatet lagres som tidsstemplete filer per institusjon i `data/output/<inst>/`, samt `course_urls_latest.*` som peker på den nyeste batchen.


## 📂 Prosjektstruktur

```text
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

---

## ▶️ Bruk

**Typisk kjøring i R:**
```r
# 1) Forbered input
source("scripts/01_prepare_input.R")

# 2) Generer URL-er
source("scripts/02_generate_urls.R")

# 3) Scrape fulltekst
source("scripts/03_scrape.R")
```

---

## 📌 Notater

- **Tegnkoding:** UTF-8 for all I/O  
- **Formatpolicy:** CSV (output), YAML (config), RDS (cache/mellomlagring)  
- **Tidsstempel:** `format(Sys.time(), "%Y%m%d-%H%M")` i filnavn  
- **`latest`-filer:** peker alltid til nyeste batch  
- **Utvidelser:** nye institusjoner legges til via YAML (ingen endring i R-kode nødvendig)

---

## 🗺️ Roadmap

- Scraping kommer for hver istitusjon enkeltvis i R/`institution_short` og automatisert i 03_scrape
- Emnebeskrivelser og sider for emner på engelsk skal legges inn i tokens og yaml filene.
- God helg! 
