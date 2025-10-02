library(httr2)

sample_fixed_total <- function(df, N = 300,
                               min_per_inst = 8L, max_per_inst = 40L,
                               seed = 42L, by_vars = c("year","semester_name")) {
  stopifnot(all(c("url","institution_short") %in% names(df)))
  df <- subset(df, !is.na(url) & nzchar(url) & !is.na(institution_short))
  set.seed(seed)
  
  inst_tab <- sort(table(df$institution_short), decreasing = TRUE)
  K <- length(inst_tab); if (K == 0L) return(df[0, , drop = FALSE])
  
  if (N < K * min_per_inst) min_per_inst <- max(1L, floor(N / K))
  prop <- inst_tab / sum(inst_tab)
  raw_quota <- round(as.numeric(prop) * N); names(raw_quota) <- names(inst_tab)
  raw_quota <- setNames(pmax(min_per_inst, pmin(max_per_inst, raw_quota)), names(inst_tab))
  
  total <- sum(raw_quota)
  if (total > N) {
    sf <- N / total
    raw_quota <- setNames(pmax(min_per_inst, pmin(max_per_inst, round(raw_quota * sf))), names(inst_tab))
    while (sum(raw_quota) > N) {
      i <- which.max(raw_quota)
      if (raw_quota[i] > min_per_inst) raw_quota[i] <- raw_quota[i] - 1L else break
    }
  } else if (total < N) {
    while (sum(raw_quota) < N) {
      i <- which.min(raw_quota)
      if (raw_quota[i] < max_per_inst) raw_quota[i] <- raw_quota[i] + 1L else break
    }
  }
  
  out <- list()
  for (inst in names(inst_tab)) {
    dfi <- df[df$institution_short == inst, , drop = FALSE]
    idx <- match(inst, names(raw_quota))
    k <- min(if (!is.na(idx)) raw_quota[[idx]] else min_per_inst, nrow(dfi))
    if (is.na(k) || k <= 0L) next
    
    strata_key <- if (length(by_vars)) do.call(paste, c(dfi[by_vars], sep = "|")) else rep("all", nrow(dfi))
    strata <- split(dfi, strata_key)
    m <- length(strata); base_q <- if (m > 0) floor(k / m) else k
    rem <- k - base_q * m
    
    pick <- lapply(strata, function(s) {
      if (!nrow(s) || base_q <= 0) return(s[0, , drop = FALSE])
      s[sample(seq_len(nrow(s)), size = min(base_q, nrow(s))), , drop = FALSE]
    })
    if (rem > 0 && m > 0) {
      for (ek in sample(names(strata), size = rem, replace = (rem > m))) {
        s <- strata[[ek]]; if (!nrow(s)) next
        already <- pick[[ek]]
        rem_idx <- setdiff(seq_len(nrow(s)), match(rownames(already), rownames(s)))
        if (length(rem_idx)) pick[[ek]] <- rbind(already, s[sample(rem_idx, 1L), , drop = FALSE])
      }
    }
    out[[length(out)+1L]] <- do.call(rbind, pick)
  }
  ans <- if (length(out)) do.call(rbind, out) else df[0, , drop = FALSE]
  rownames(ans) <- NULL
  if (nrow(ans) > N) ans <- ans[sample(seq_len(nrow(ans)), size = N), , drop = FALSE]
  ans
}

verify_status <- function(df, sleep = 0.25, timeout = 12, max_tries = 2) {
  stopifnot("url" %in% names(df))
  df <- subset(df, !is.na(url) & nzchar(url))
  UA <- "TEPS-url-verifier/1.0 (+github.com/teps)"
  
  cols <- c("url","final_url","status","ok","content_type","bytes","error")
  res  <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    u <- df$url[i]
    make_req <- function(method = c("HEAD","GET")) {
      method <- match.arg(method)
      request(u) |>
        req_user_agent(UA) |>
        req_timeout(timeout) |>
        req_error(is_error = ~ FALSE) |>
        req_retry(max_tries = max_tries, backoff = ~ min(6, 1.5 ^ attempt)) |>
        req_headers(`Accept-Language` = "nb,no;q=0.9,en;q=0.8") |>
        req_method(method)
    }
    resp <- tryCatch(req_perform(make_req("HEAD")), error = function(e) e)
    if (!inherits(resp, "response") || is.na(resp_status(resp)) || resp_status(resp) %in% c(0, 405, 403)) {
      resp <- tryCatch(req_perform(make_req("GET")), error = function(e) e)
    }
    if (inherits(resp, "response")) {
      row <- data.frame(
        url          = u,
        final_url    = resp_url(resp),
        status       = resp_status(resp),
        ok           = resp_status(resp) >= 200 & resp_status(resp) < 400,
        content_type = resp_content_type(resp),
        bytes        = length(resp_body_raw(resp)),
        error        = NA_character_,
        stringsAsFactors = FALSE
      )
    } else {
      emsg <- tryCatch(conditionMessage(resp), error = function(e) "request failed")
      row <- data.frame(
        url          = u,
        final_url    = NA_character_,
        status       = NA_integer_,
        ok           = FALSE,
        content_type = NA_character_,
        bytes        = NA_integer_,
        error        = as.character(emsg),
        stringsAsFactors = FALSE
      )
    }
    row <- row[, cols]
    res[[i]] <- row
    if (sleep > 0) Sys.sleep(sleep + runif(1, 0, 0.15))
    if (i %% 50 == 0) cat("...sjekket", i, "av", nrow(df), "URLer\n")
  }
  out <- do.call(rbind, res); rownames(out) <- NULL; out
}
