# MF (lowercase + nodashnum)
local({
  inst_short <- "mf"
  get_courses_std <- function(path="data/cache/courses_std.RDS"){ need<-c("institution_short","course_code_norm","code_upper","code_base","year","semester_hv"); x<-if (exists("courses")) courses else readRDS(path); miss<-setdiff(need,names(x)); if(length(miss)) stop("Cache mangler: ",paste(miss,collapse=", ")); x }
  df0 <- get_courses_std(); df <- subset(df0, institution_short==inst_short & nzchar(course_code_norm)); if(!nrow(df)) stop("Ingen rader for ",inst_short)
  tpl_sub <- function(p,e){ out<-p; for(k in names(e)) out<-gsub(paste0("\\{",k,"\\}"), ifelse(is.na(e[[k]]),"",e[[k]]), out, perl=TRUE); out }
  safe_read_yaml <- function(path){ tr<-try({con<-file(path,"r",encoding="UTF-8"); on.exit(close(con),add=TRUE); yaml::read_yaml(con)},TRUE); if(!inherits(tr,"try-error") && !is.null(tr)) return(tr); raw<-readBin(path,"raw",file.info(path)$size); txt<-rawToChar(raw); Encoding(txt)<-"unknown"; txt<-sub("^\ufeff","",txt); txt<-gsub("\r\n?","\n",txt); txt<-gsub("\t","  ",txt, TRUE); yaml::yaml.load(txt) }
  pattern <- as.character(safe_read_yaml("config/institutions.yaml")$institutions[[inst_short]]$url_pattern)[1]; if(!nzchar(pattern)) stop("Mangler url_pattern")
  urls <- character(nrow(df))
  for(i in seq_len(nrow(df))){
    urls[i] <- tpl_sub(pattern, list(
      year="", semester="", semester_url="",
      course_code=df$course_code_norm[i],
      code_lower_nodashnum=tolower(df$code_base[i]),
      code_upper=df$code_upper[i],
      code_lower=tolower(df$course_code_norm[i]),
      code_upper_base=df$code_base[i],
      code_lower_base=tolower(df$code_base[i]),
      code_upper_nodashnum=df$code_base[i],
      code_upper_nodash1=df$code_base[i],
      code_upper_nosfx1A=sub("-(1|A)$","", df$code_upper[i])
    ))
  }
  out <- data.frame(course_code_norm=df$course_code_norm, year=NA_integer_, hv=NA_character_, url=urls, stringsAsFactors=FALSE)
  out <- out[!duplicated(out$url), , drop=FALSE]; if(any(grepl("\\{[^}]+\\}", out$url))) warning("Uerstattede tokens ??? sjekk YAML. Eksempel: ", grep("\\{[^}]+\\}", out$url, value=TRUE)[1])
  ts<-format(Sys.time(),"%Y%m%d-%H%M"); outd<-file.path("data","output",inst_short); dir.create(outd,TRUE,TRUE)
  csv_ts<-file.path(outd,sprintf("course_urls_%s_%s.csv",inst_short,ts)); txt_ts<-file.path(outd,sprintf("course_urls_%s_%s.txt",inst_short,ts))
  utils::write.csv(out[,c("course_code_norm","year","hv","url")], csv_ts, row.names=FALSE, fileEncoding="UTF-8"); writeLines(out$url[nchar(out$url)>0], txt_ts, useBytes=TRUE)
  file.copy(csv_ts, file.path(outd,"course_urls_latest.csv"), TRUE); file.copy(txt_ts, file.path(outd,"course_urls_latest.txt"), TRUE)
  cat("Skrev filer for ",inst_short," (",nrow(out)," rader)\n", sep="")
})
