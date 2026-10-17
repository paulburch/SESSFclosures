## =============================================================================
## Closures review, step 19: for every source text, the dates it was in effect
## and the rule each schedule carries.
##   start  = commencement (stated in the text; else the Register's date)
##   end    = first day NOT in effect: the earliest of the Register's
##            "no longer in force" date, a stated cessation / repeal date, or
##            the end of a stated period of application
## Rules: the sentences of the operative clauses that name each schedule;
## gear is classified from their words.
## Output: closures/output/source_meta.csv, source_rules.csv
## Written 17 October 2026 with the assistance of Claude (Anthropic).
## =============================================================================
source("closures/R/fun_closures.R")
cat_df <- read.csv("closures/catalogue/instrument_catalogue.csv", stringsAsFactors = FALSE)
vers <- read.csv("closures/catalogue/instrument_versions.csv", stringsAsFactors = FALSE)
MON <- c(january = 1, february = 2, march = 3, april = 4, may = 5, june = 6, july = 7,
         august = 8, september = 9, october = 10, november = 11, december = 12)
dre <- "([0-9]{1,2})(?:st|nd|rd|th)?\\s+(?:of\\s+)?(January|February|March|April|May|June|July|August|September|October|November|December)\\s+([0-9]{4})"
pdate <- function(s) {
  m <- regmatches(s, regexec(dre, s, perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(m)) return(as.Date(NA))
  as.Date(sprintf("%s-%02d-%02d", m[4], MON[[tolower(m[3])]], as.integer(m[2])))
}
grab <- function(fl, pat) { m <- regmatches(fl, regexpr(pat, fl, perl = TRUE, ignore.case = TRUE)); if (length(m)) m else NA_character_ }

## sources: as-made texts of titles carrying closures, then their compilations
skip <- c("F2011L02125", "F2012L00473", "F2016L00842", "F2017L00515", "F2019L00650", "F2019L01200",
          "F2019L01609", "F2018L01077", "F2022L00116", "F2023L00439", "F2024L00546", "F2024L01344", "F2025L00736")
src <- data.frame(key = cat_df$id, title = cat_df$id, name = cat_df$name, reg = as.Date(cat_df$registered),
                  reg_end = as.Date(cat_df$no_longer_in_force_from), kind = "as made", stringsAsFactors = FALSE)
src <- src[!src$key %in% skip, ]
cv <- vers[grepl("C[0-9]{5}$", vers$registerId), ]
src <- rbind(src, data.frame(key = cv$registerId, title = cv$titleId, name = cat_df$name[match(cv$titleId, cat_df$id)],
                             reg = as.Date(cv$start), reg_end = as.Date(cv$end), kind = "compilation"))
meta <- do.call(rbind, lapply(seq_len(nrow(src)), function(i) {
  k <- src$key[i]; tx <- read_instrument(k); fl <- gsub("\\s+", " ", paste(tx, collapse = " "))
  com <- grab(fl, "commences?\\s+(?:on|at)[^.;]{0,90}|commences seven days after[^.;]{0,40}")
  ## the 2021 direction gives its commencement in a table:
  ## "1. The whole of the instrument | 1 May 2021."
  if (is.na(com) || is.na(pdate(com))) {
    w <- grab(fl, "whole of (?:this|the) instrument\\.?\\s*[|]?\\s*[0-9]{1,2}\\s+[A-Z][a-z]+\\s+[0-9]{4}")
    if (!is.na(w)) com <- w
  }
  start <- src$reg[i]
  if (src$kind[i] == "as made" && !is.na(com)) {
    if (grepl("day after", com, TRUE)) start <- src$reg[i] + 1
    else if (grepl("seven days after", com, TRUE)) start <- src$reg[i] + 7
    else if (!is.na(pdate(com))) start <- pdate(com)
  }
  ## stated ends: "ceases (as if it was revoked) on X", "revoked/repealed on X" -> X;
  ## "ends at the end of X", "ending at 2400 hours ... on X" -> X + 1
  e1 <- grab(fl, "(?:ceases|is repealed|revoked|repealed|expires)\\s+(?:as if it was revoked\\s+)?on\\s+[^.;]{0,40}")
  e2 <- grab(fl, "(?:ends|ending)\\s+at\\s+(?:the\\s+end\\s+of|2400\\s+hours[^.;]{0,20}on(?:\\s+the)?)\\s+[^.;]{0,40}")
  e3 <- grab(fl, "period of ([0-9]+) months from the commencement")
  ## 2005 "applies for a period ... ending on 31 December 2006"; 2007 "ceases to have effect after 1 January 2012"
  e4 <- grab(fl, "(?:ending on|ceases to have effect after|until the end of)\\s+(?:the\\s+)?[^.;]{0,40}")
  ends <- c(src$reg_end[i], pdate(e1), pdate(e2) + 1, pdate(e4) + 1)
  if (!is.na(e3)) ends <- c(ends, seq(start, by = paste(sub("period of ([0-9]+).*", "\\1", e3), "months"), length.out = 2)[2])
  end <- suppressWarnings(min(ends, na.rm = TRUE)); if (!is.finite(end)) end <- as.Date(NA)
  data.frame(key = k, title = src$title[i], kind = src$kind[i], name = src$name[i],
             start = start, end = end, register_end = src$reg_end[i],
             commencement_text = com, end_text = paste(na.omit(c(e1, e2, e3, e4)), collapse = " | "))
}))
## a direction revoked "with effect from the commencement of" its successor: the
## Register's end date is the day after the successor commenced; end it on the
## successor's commencement so the two do not overlap
eb <- sub("\\s.*", "", cat_df$ended_by[match(meta$title, cat_df$id)])
for (j in which(meta$kind == "as made" & !is.na(eb) & eb %in% meta$key)) {
  s <- meta$start[meta$key == eb[j]][1]
  if (!is.na(s) && !is.na(meta$end[j]) && meta$end[j] > s && meta$end[j] - s <= 7) meta$end[j] <- s
}
## a compilation takes over from the as-made (or earlier compilation) of its title
for (t in unique(meta$title[meta$kind == "compilation"])) {
  m <- meta[meta$title == t, ]; m <- m[order(m$start), ]
  for (j in seq_len(nrow(m) - 1)) meta$end[meta$key == m$key[j]] <- min(meta$end[meta$key == m$key[j]], m$start[j + 1], na.rm = TRUE)
}
write.csv(meta, "closures/output/source_meta.csv", row.names = FALSE, na = "")

## rules: operative sentences naming each schedule
rules <- do.call(rbind, lapply(meta$key, function(k) {
  tx <- read_instrument(k); body <- paste(vapply(tx, clean_page, ""), collapse = "\n")
  ## operative text = everything except the schedules' own text (a contents
  ## page can put "Schedule 1" before the operative clauses, 2010 and 2021)
  sch <- tryCatch(split_schedules(tx), error = function(e) list())
  for (s in sch) body <- sub(s$text, " ", body, fixed = TRUE)
  op <- gsub("\\s+", " ", body)
  sen <- unlist(strsplit(op, "(?<=[.;])\\s+(?=\\(?[a-z0-9]{1,3}\\)|[A-Z0-9])", perl = TRUE))
  sen <- sen[grepl("not to be engaged|not to engage|prohibited|must not|is closed|not fish|do not apply|does not apply|permitted", sen, TRUE)]
  ## not operative: contents pages, the Register header line, headings
  sen <- sen[!grepl("^\\s*(Contents|SOURCE:)|\\.{5,}|Schedule\\s+[0-9]+\\s*-\\s*[A-Z].*Schedule\\s+[0-9]+\\s*-\\s*[A-Z]|^\\s*(DIRECTION NOT TO ENGAGE|Direction not to engage in fishing\\s*[0-9]*\\.?\\s*$)", sen)]
  if (!length(sen)) return(NULL)
  do.call(rbind, lapply(sen, function(s) {
    ## every schedule named: "Schedule 1, Schedule 2 and Schedule 3", "Schedules 6, 7 and 8", "Schedules 22 to 26"
    ref <- regmatches(s, gregexpr("Schedules?\\s+[0-9]+(?:\\s*(?:,\\s*and|,\\s*or|,|and|or|to|-|/)\\s*[0-9]+)*", s, perl = TRUE))[[1]]
    ref <- gsub("\\s+to\\s+", "-", paste(ref, collapse = " "))
    nums <- if (length(ref) && nzchar(ref)) {
      x <- gsub("\\s*-\\s*", "-", gsub("[^0-9-]+", " ", ref))   # "27 - 35" is a range
      parts <- strsplit(trimws(x), "\\s+")[[1]]
      v <- unlist(lapply(parts, function(p) {
        r <- suppressWarnings(as.integer(strsplit(p, "-")[[1]])); r <- r[!is.na(r)]
        if (length(r) == 2 && r[2] >= r[1] && r[2] - r[1] < 60) seq(r[1], r[2]) else r }))
      if (length(v)) v else 0L
    } else 0L
    data.frame(key = k, schedule = nums, sentence = substr(s, 1, 600))
  }))
}))
gear_of <- function(s) {
  s <- tolower(s)
  if (grepl("does not apply|do not apply|permitted|despite", s)) return("exemption")
  g <- c(if (grepl("demersal otter trawl", s)) "demersal otter trawl" else if (grepl("trawl", s)) "trawl",
         if (grepl("gillnet|gill net", s)) "gillnet",
         if (grepl("automatic", s)) "automatic longline" else if (grepl("\\bhook|longlin|dropline|line methods", s)) "hook",   # not "bounded by a line"
         if (grepl("danish seine", s)) "Danish seine")
  if (!length(g)) "all methods" else paste(unique(g), collapse = " + ")
}
rules$gear <- vapply(rules$sentence, gear_of, "")
rules$species_specific <- grepl("for the species|for [A-Z][a-z]+ \\(|snapper|school shark|pink ling", rules$sentence)
## conditional closures: catch triggers ("takes more than three", "ten or more ...
## dogfish", tonnes) and move-on rules ("finds any evidence of benthic impacts")
rules$trigger <- grepl("takes (more than )?(three|[0-9])|or more |if an aggregated|if either|evidence of|tonnes|kilograms", rules$sentence, ignore.case = TRUE)
write.csv(rules, "closures/output/source_rules.csv", row.names = FALSE)
cat(nrow(meta), "sources;", nrow(rules), "rule rows\n")
print(meta[, c("key", "kind", "start", "end", "register_end")], row.names = FALSE)
