nm <- function(x) tolower(stringi::stri_trans_general(enc2utf8(ifelse(is.na(x),"",x)), "Latin-ASCII"))

apply_adapters <- function(df, cfg) {
  df$uio_faculty_slug <- NA_character_
  df$uio_inst_slug    <- NA_character_
  
  is_uio <- !is.na(df$institution_short) & df$institution_short == "uio"
  if (!any(is_uio)) return(df)
  
  uio_fac_map <- c(
    "det humanistiske fakultet"="hf","det juridiske fakultet"="jus",
    "det matematisk-naturvitenskapelige fakultet"="matnat","det medisinske fakultet"="med",
    "det odontologiske fakultet"="odont","det samfunnsvitenskapelige fakultet"="sv",
    "det teologiske fakultet"="teologi","det utdanningsvitenskapelige fakultet"="uv"
  )
  uio_inst_map <- list(
    "institutt for arkeologi, konservering og historiske studier"=c("hf","iakh"),
    "institutt for kulturstudier og orientalske sprak"=c("hf","ikos"),
    "institutt for filosofi, ide- og kunsthistorie og klassiske sprak"=c("hf","ifikk"),
    "institutt for litteratur, omradestudier og europeiske sprak"=c("hf","ilos"),
    "institutt for lingvistiske og nordiske studier"=c("hf","iln"),
    "institutt for medier og kommunikasjon"=c("hf","imk"),
    "institutt for sosiologi og samfunnsgeografi"=c("sv","iss"),
    "institutt for statsvitenskap"=c("sv","statsvitenskap"),
    "psykologisk institutt"=c("sv","psi"),
    "sosialantropologisk institutt"=c("sv","sai"),
    "okonomisk institutt"=c("sv","oekonomi"),
    "institutt for informatikk"=c("matnat","ifi"),
    "institutt for geofag"=c("matnat","geofag"),
    "kjemisk institutt"=c("matnat","kjemi"),
    "fysisk institutt"=c("matnat","fys"),
    "matematisk institutt"=c("matnat","math"),
    "institutt for biovitenskap"=c("matnat","ibv"),
    "farmasoytisk institutt"=c("matnat","farmasi"),
    "institutt for teoretisk astrofysikk"=c("matnat","astro"),
    "institutt for teknologisystemer"=c("matnat","its"),
    "institutt for helse og samfunn"=c("med","helsam"),
    "institutt for medisinske basalfag"=c("med","imb"),
    "institutt for klinisk medisin"=c("med","klinmed"),
    "institutt for oral biologi"=c("odont","iob"),
    "institutt for klinisk odontologi"=c("odont","iko"),
    "institutt for larerutdanning og skoleforskning"=c("uv","ils"),
    "institutt for spesialpedagogikk"=c("uv","isp"),
    "institutt for pedagogikk"=c("uv","iped"),
    "cemo - centre for educational measurement"=c("uv","cemo"),
    "det samfunnsvitenskapelige fakultet"=c("sv","sv"),
    "det teologiske fakultet"=c("teologi","tf")
  )
  
  idx <- which(is_uio); n <- length(idx)
  facnm <- if ("faculty_name" %in% names(df)) df$faculty_name[idx] else rep(NA_character_, n)
  facnm_norm <- nm(facnm)
  
  hit <- match(facnm_norm, names(uio_inst_map))
  has <- !is.na(hit)
  if (any(has)) {
    pair <- do.call(rbind, uio_inst_map[hit[has]])
    df$uio_faculty_slug[idx[has]] <- pair[,1]
    df$uio_inst_slug[idx[has]]    <- pair[,2]
  }
  
  # fallback p?? fakultet
  fac_only <- is.na(df$uio_faculty_slug[idx]) & !is.na(facnm)
  if (any(fac_only)) {
    fac_guess <- uio_fac_map[ nm(facnm[fac_only]) ]
    ok <- !is.na(fac_guess)
    df$uio_faculty_slug[idx[fac_only][ok]] <- fac_guess[ok]
  }
  df
}
