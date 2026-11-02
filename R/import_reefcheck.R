# =============================================================================
# import_reefcheck.R -- Reef Restoration Prioritisation Tool (v2.6.0)
#
# Reading Reef Check tropical substrate data and converting them to the tool's
# transect format, so that teams using the Reef Check protocol can use the tool
# without retyping their data.
#
# Reef Check records the substrate at 40 points in each of four 20 m segments
# along a 100 m line, in ten categories (HC, SC, RKC, NIA, SP, RC, RB, SD, SI,
# OT), and counts hard-coral points with bleaching or disease ("HC/B", "HC/D").
# Each segment becomes one row ("transect") for the tool. Three layouts are
# accepted:
#   1. Reef Check's own substrate data sheet (Line-Transect-Substrate, .xlsx or
#      .xls; one file per site, depth and date; several files may be uploaded);
#   2. a segment table: one row per segment with the point counts (or
#      percentages) of each category;
#   3. a point table: one row per point with its category code.
#
# What is lost (tested on the Isla del Cano data; see the design record for
# v2.6.0): Reef Check does not separate turf or crustose coralline algae (both
# are recorded as rock, RC), does not record cyanobacteria as a category and
# does not count colonies by taxon, so turf, CCA, diversity and the
# algal-dominance axis are unavailable. Rock is never read as turf. Where a
# programme splits rock into rock with turf (RCTA) and rock with coralline algae
# (RCCA), as Reef Check Australia does, these are used as turf and CCA.
# =============================================================================

#' Reef Check tropical substrate codes and their meaning.
REEFCHECK_CODES <- c(HC = "hard coral", SC = "soft coral", RKC = "recently killed coral",
                     NIA = "nutrient indicator algae", SP = "sponge", RC = "rock",
                     RB = "rubble", SD = "sand", SI = "silt/clay", OT = "other")
# Sub-categories used by some programmes (e.g. Reef Check Australia), mapped to
# the main code for totals and, where informative, to the tool's variables.
REEFCHECK_SUBCODES <- c(RCTA = "RC", RCCA = "RC", RKCTA = "RKC", RKCNIA = "RKC")

#' Note shown with every analysis of Reef Check data.
REEFCHECK_NOTE <- paste(
  "Reef Check data: turf algae, crustose coralline algae (CCA), cyanobacteria and coral diversity are not recorded",
  "by this protocol, so they are not used, and the algal-dominance axis cannot be calculated. Classes rest mainly on",
  "coral cover, macroalgae, rubble, bleaching and disease. Turf-dominated sites may be rated too favourably, and class",
  "support can look higher than with fuller data without being more accurate (see the user guide).")

#' Split Reef Check point entries into a base code and bleaching/disease flags.
#'
#' @param x Character vector of entries such as "HC", "hc/b", "HC/D", "RC ".
#' @return Data frame with `code` (main code, or `NA` if not recognised), `bleached` and `diseased` (logical).
reefcheck_parse_codes <- function(x) {
  x <- toupper(gsub("\\s", "", clean_text(x)))
  x[x %in% c("", "NA")] <- NA
  base <- sub("/.*$", "", x)
  flags <- ifelse(grepl("/", x), sub("^[^/]*/", "", x), "")
  main <- ifelse(base %in% names(REEFCHECK_SUBCODES), REEFCHECK_SUBCODES[base], base)
  ok <- main %in% names(REEFCHECK_CODES)
  data.frame(entry = x, sub = ifelse(ok, base, NA), code = ifelse(ok, main, NA),
             bleached = ok & main == "HC" & grepl("B", flags),
             diseased = ok & main == "HC" & grepl("D", flags),
             stringsAsFactors = FALSE)
}

#' Convert counts of Reef Check categories in each segment to the tool's variables.
#'
#' @param counts Data frame with identifying columns (`site`, and optionally `date`, `depth`, `transect`) and one
#'   numeric column per Reef Check code (point counts or percentages; missing codes are taken as 0). Optional columns
#'   `RCTA`, `RCCA` (rock with turf / with coralline algae), `hc_bleached` and `hc_disease` (numbers of hard-coral
#'   points with bleaching or disease).
#' @return Data frame in the tool's transect format (proportions), with columns `site`, `date`, `depth`, `transect`,
#'   `method`, `coral`, `macroalgae`, `rubble`, `sand`, `bleached`, `disease`, and, where recorded, `turf` and `cca`.
#'   Attribute `messages` lists any checks that failed.
reefcheck_counts_to_transects <- function(counts) {
  msgs <- character(0)
  get <- function(code) if (code %in% names(counts)) { v <- to_number(counts[[code]]); v[is.na(v)] <- 0; v } else rep(0, nrow(counts))
  main <- sapply(names(REEFCHECK_CODES), get)
  if (is.null(dim(main))) main <- matrix(main, nrow = 1, dimnames = list(NULL, names(REEFCHECK_CODES)))
  # rock sub-categories add to rock unless rock already includes them
  rcta <- get("RCTA"); rcca <- get("RCCA")
  if (any(rcta + rcca > 0) && all(main[, "RC"] == 0)) main[, "RC"] <- rcta + rcca
  total <- rowSums(main)
  if (any(total == 0)) msgs <- c(msgs, sprintf("%d segment(s) had no substrate points and were left out.", sum(total == 0)))
  # Reef Check segments have 40 points; other totals are allowed (percentages, other designs) but reported
  pts <- total[total > 0]
  if (all(abs(pts - 100) < 1.5)) {
    unit <- "percentages"
  } else {
    unit <- "point counts"
    off <- sum(abs(pts - 40) > 0.5)
    if (off) msgs <- c(msgs, sprintf("%d segment(s) do not have 40 points (Reef Check standard); cover was calculated from the points recorded.", off))
  }
  keep <- total > 0
  p <- function(v) (v / total)[keep]
  out <- data.frame(site = clean_text(counts$site)[keep], stringsAsFactors = FALSE)
  out$date <- if ("date" %in% names(counts)) as.character(counts$date)[keep] else NA_character_
  out$depth <- if ("depth" %in% names(counts)) to_number(sub("[^0-9.,]+$", "", clean_text(counts$depth)))[keep] else NA_real_
  out$transect <- if ("transect" %in% names(counts)) as.character(counts$transect)[keep] else seq_len(sum(keep))
  out$method <- "reef check"
  out$coral      <- p(main[, "HC"])
  out$macroalgae <- p(main[, "NIA"])
  out$rubble     <- p(main[, "RB"])
  out$sand       <- p(main[, "SD"])
  # Rock (RC) is never read as turf: it also includes bare rock and coralline algae.
  if (any(rcta > 0 | rcca > 0)) {
    out$turf <- p(rcta); out$cca <- p(rcca)
    msgs <- c(msgs, "Rock with turf (RCTA) and rock with coralline algae (RCCA) were used as turf and CCA.")
  }
  # bleaching and disease: share of hard-coral points affected
  hc <- main[keep, "HC"]
  for (v in c("hc_bleached", "hc_disease")) {
    if (v %in% names(counts)) {
      n <- to_number(counts[[v]])[keep]
      x <- ifelse(hc > 0, n / hc, NA_real_)
      if (unit == "percentages" && all(n <= 100, na.rm = TRUE) && any(n > hc, na.rm = TRUE)) x <- n / 100
      out[[if (v == "hc_bleached") "bleached" else "disease"]] <- pmin(x, 1)
    }
  }
  attr(out, "messages") <- msgs
  out
}

#' Is a table in a Reef Check layout (segment or point table) rather than the tool's own format?
#'
#' @param df Data frame as read from a CSV file.
#' @return `"segments"`, `"points"` or `NA`.
reefcheck_table_type <- function(df) {
  low <- toupper(clean_text(names(df)))
  if ("CORAL" %in% low) return(NA_character_)     # the tool's own format
  if (sum(low %in% c(names(REEFCHECK_CODES), names(REEFCHECK_SUBCODES))) >= 3 && "SITE" %in% low) return("segments")
  if (all(c("SITE", "CODE") %in% low)) return("points")
  NA_character_
}

#' Convert a Reef Check segment or point table to the tool's transect format.
#'
#' @param df Data frame read from a CSV file.
#' @param type `"segments"` or `"points"` (from `reefcheck_table_type()`).
#' @return Data frame in the tool's format, with attribute `messages`.
reefcheck_table_to_transects <- function(df, type) {
  names(df) <- clean_text(names(df))
  up <- toupper(names(df))
  std <- c(SITE = "site", DATE = "date", DEPTH = "depth", SEGMENT = "transect", TRANSECT = "transect",
           HC_BLEACHED = "hc_bleached", HC_DISEASE = "hc_disease", CODE = "code")
  for (k in names(std)) { hit <- which(up == k); if (length(hit) && !std[[k]] %in% names(df)) names(df)[hit[1]] <- std[[k]] }
  up <- toupper(names(df))
  for (k in c(names(REEFCHECK_CODES), names(REEFCHECK_SUBCODES))) { hit <- which(up == k); if (length(hit)) names(df)[hit[1]] <- k }
  if (type == "points") {
    pc <- reefcheck_parse_codes(df$code)
    bad <- sum(is.na(pc$code) & !is.na(pc$entry))
    id_cols <- intersect(c("site", "date", "depth", "transect"), names(df))
    key <- do.call(paste, c(df[id_cols], sep = "\r"))
    ids <- df[!duplicated(key), id_cols, drop = FALSE]
    k <- key[!duplicated(key)]
    tab <- function(cond) as.numeric(tapply(cond, factor(key, levels = k), sum, na.rm = TRUE))
    for (code in names(REEFCHECK_CODES)) ids[[code]] <- tab(pc$code %in% code)
    for (s in names(REEFCHECK_SUBCODES)) if (any(pc$sub %in% s)) ids[[s]] <- tab(pc$sub %in% s)
    ids$hc_bleached <- tab(pc$bleached); ids$hc_disease <- tab(pc$diseased)
    out <- reefcheck_counts_to_transects(ids)
    if (bad) attr(out, "messages") <- c(sprintf("%d point(s) had codes that are not Reef Check categories and were left out.", bad),
                                        attr(out, "messages"))
    return(out)
  }
  reefcheck_counts_to_transects(df)
}

#' Read one Reef Check line-transect substrate data sheet (Excel).
#'
#' Finds the site, date and depth beside their labels and the point codes in the four segment blocks, wherever the
#' sheet places them, so small layout differences between versions of the sheet do not matter.
#'
#' @param path Path to the .xlsx or .xls file.
#' @param file_name Original file name, used for the site, date and depth when the sheet leaves them blank (Reef Check
#'   names files "Site dd-mm-yy line s").
#' @return Data frame in the tool's format (one row per segment), with attribute `messages`.
read_reefcheck_sheet <- function(path, file_name = basename(path)) {
  if (!requireNamespace("readxl", quietly = TRUE)) stop("Reading Excel files needs the 'readxl' package. Save the sheet as CSV instead, or install readxl.")
  sheets <- tryCatch(readxl::excel_sheets(path), error = function(e)
    stop(sprintf("'%s' could not be opened. Reef Check sheets are protected; open the file in Excel and save it as .xlsx (File > Save As), then upload it again.", file_name)))
  sh <- if ("DATA" %in% toupper(sheets)) sheets[toupper(sheets) == "DATA"][1] else sheets[1]
  x <- as.matrix(readxl::read_excel(path, sheet = sh, col_names = FALSE, col_types = "text", .name_repair = "minimal"))
  x[is.na(x)] <- ""
  x <- matrix(clean_text(x), nrow = nrow(x))
  # value next to a label: first non-empty cell to its right that is not another label
  value_of <- function(pattern) {
    hit <- which(matrix(grepl(pattern, x, ignore.case = TRUE), nrow = nrow(x)), arr.ind = TRUE)
    if (!length(hit)) return(NA_character_)
    r <- hit[1, 1]; c0 <- hit[1, 2]
    if (c0 < ncol(x)) for (c in (c0 + 1):ncol(x)) {
      v <- x[r, c]
      if (v != "") return(if (grepl(":$", v)) NA_character_ else v)
    }
    NA_character_
  }
  site <- value_of("^Site Name"); date <- value_of("^Date"); depth <- value_of("^Transect Depth")
  # Excel dates arrive as day numbers when read as text
  if (!is.na(date) && grepl("^[0-9]{5}(\\.0+)?$", date)) date <- format(as.Date(as.numeric(date), origin = "1899-12-30"))
  # fall back on Reef Check's file-naming convention: "Paradise 13-12-02 line s"
  stem <- sub("[.][^.]*$", "", file_name)
  m <- regmatches(stem, regexec("^(.*?)\\s+([0-9]{1,2}-[0-9]{1,2}-[0-9]{2,4})(.*)$", stem))[[1]]
  if (length(m)) {
    if (is.na(site) || site == "") site <- clean_text(m[2])
    if (is.na(date) || date == "") { d <- as.Date(m[3], "%d-%m-%y"); if (nchar(sub(".*-", "", m[3])) == 4) d <- as.Date(m[3], "%d-%m-%Y"); date <- format(d) }
    if (is.na(depth) || depth == "") depth <- c(s = "shallow", m = "medium")[tolower(trimws(sub(".*\\s", "", m[4])))]
  }
  if (is.na(site) || site == "") stop(sprintf("No site name in '%s'. Fill in 'Site Name' on the DATA sheet.", file_name))
  # point rows: the first column holds 0.0 ... 9.5; code columns sit to the right of each distance column
  first <- suppressWarnings(as.numeric(x[, 1]))
  rows <- which(first %in% seq(0, 9.5, by = 0.5))
  rows <- rows[rows <= (min(rows) + 19)]
  if (length(rows) < 20) stop(sprintf("'%s' does not look like a Reef Check substrate data sheet (point rows not found).", file_name))
  dist <- suppressWarnings(matrix(as.numeric(x[rows, ]), nrow = length(rows)))
  start_cols <- which(dist[1, ] %in% c(0, 10, 25, 35, 50, 60, 75, 85))
  if (length(start_cols) != 8) stop(sprintf("'%s': the four segment blocks were not found where expected.", file_name))
  seg_of <- c(1, 1, 2, 2, 3, 3, 4, 4)
  codes <- data.frame(transect = rep(seg_of, each = length(rows)),
                      code = as.vector(x[rows, start_cols + 1, drop = FALSE]), stringsAsFactors = FALSE)
  codes <- codes[codes$code != "", , drop = FALSE]
  if (!nrow(codes)) stop(sprintf("'%s' contains no substrate codes.", file_name))
  codes$site <- site; codes$date <- date; codes$depth <- depth
  out <- reefcheck_table_to_transects(codes, "points")
  # bleaching and disease counts typed in the S1-S4 rows, if the points were not marked HC/B or HC/D
  count_row <- function(pattern) {
    r <- which(grepl(pattern, x[, 1], ignore.case = TRUE))[1]
    lab <- which(apply(x, 1, function(row) all(c("S1", "S2", "S3", "S4") %in% row)))
    if (is.na(r) || !length(lab)) return(NULL)
    cols <- match(c("S1", "S2", "S3", "S4"), x[lab[1], ])
    to_number(x[r, cols])
  }
  hc_pts <- as.numeric(tapply(reefcheck_parse_codes(codes$code)$code %in% "HC", factor(codes$transect, levels = 1:4), sum))
  for (v in list(c("bleached", "^# of HC with bleaching"), c("disease", "^# of HC with disease"))) {
    n <- count_row(v[2])
    if (!is.null(n) && any(!is.na(n) & n > 0) && all(is.na(out[[v[1]]]) | out[[v[1]]] == 0)) {
      seg <- as.integer(out$transect)
      out[[v[1]]] <- ifelse(hc_pts[seg] > 0, pmin(n[seg] / hc_pts[seg], 1), NA_real_)
    }
  }
  out$transect <- paste0(site, " ", out$date, " ", depth, " S", out$transect)
  out
}

#' Read one or more uploaded files in the tool's own format or in a Reef Check layout.
#'
#' @param paths Paths of the uploaded files.
#' @param names Original file names (same order).
#' @return A list: `df` (combined data in the tool's input format, ready for `prepare_data()`), `messages` (character)
#'   and `reefcheck` (logical: at least one file was Reef Check data).
import_transect_files <- function(paths, names = basename(paths)) {
  parts <- list(); msgs <- character(0); rc <- FALSE
  for (i in seq_along(paths)) {
    ext <- tolower(sub("^.*[.]", "", names[i]))
    if (ext %in% c("xlsx", "xls")) {
      d <- read_reefcheck_sheet(paths[i], names[i]); rc <- TRUE
    } else {
      d <- read_transect_file(paths[i])
      type <- reefcheck_table_type(d)
      if (!is.na(type)) { d <- reefcheck_table_to_transects(d, type); rc <- TRUE }
    }
    m <- attr(d, "messages")
    if (length(m)) msgs <- c(msgs, if (length(paths) > 1) paste0(names[i], ": ", m) else m)
    attr(d, "messages") <- NULL
    parts[[i]] <- d
  }
  if (length(parts) > 1) {
    cols <- unique(unlist(lapply(parts, names)))
    parts <- lapply(parts, function(d) { for (cl in setdiff(cols, names(d))) d[[cl]] <- NA; d[cols] })
    msgs <- c(sprintf("%d files were combined.", length(parts)), msgs)
  }
  df <- do.call(rbind, parts)
  if (rc) {
    msgs <- c(sprintf("Reef Check data were converted: each 20 m segment is treated as one transect (%d segments).", nrow(df)),
              msgs, REEFCHECK_NOTE)
    if ("depth" %in% names(df) && any(tapply(df$depth, df$site, function(z) length(unique(z[!is.na(z)]))) > 1))
      msgs <- c(msgs, "Some sites were surveyed at more than one depth; depths are pooled within each site. To compare depths, give each depth its own site name.")
  }
  list(df = df, messages = msgs, reefcheck = rc)
}
