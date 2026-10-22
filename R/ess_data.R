#' @include emss-package.r
NULL

# Common input format ---------------------------------------------------------
#
# Every input source (an Excel workbook, CSV files, data frames) is converted
# into one set of tables with fixed column names. Everything downstream reads
# only these:
#
#   ratings   grade, round, table, panelist, item_id, level (integer rank)
#   items     grade, item_id, domain, order, plus every other input column
#             (location columns such as RP67 are chosen at analysis time)
#   levels    grade, level (integer rank, 1 = lowest), label
#   grades    grade, sd, ec
#   examinees grade, score, freq: long score-frequency table (zero rows when
#             not provided)

# Input column names recognised by default, per table. Matching ignores case,
# spaces, underscores and dots; the first candidate found is used.
.ess_default_columns <- list(
  ratings   = list(grade = c("GCA", "Grade"), round = "Round", table = "Table",
                   panelist = c("Panelist", "User"), item_id = "Item_ID",
                   level = "ALD"),
  items     = list(grade = c("GCA", "Grade"), item_id = "Item_ID",
                   domain = "Domain", order = "Order_of_Difficulty"),
  setup     = list(grade = c("GCA", "Grade"), levels = "Level Options",
                   sd = "SD", ec = c("EC", "EC Delta")),
  examinees = list(grade = c("GCA", "Grade"), score = "Score", freq = "Freq")
)

# Columns each table must have (the others are optional).
.ess_required_columns <- list(
  ratings   = c("grade", "panelist", "item_id", "level"),
  items     = c("grade", "item_id"),
  setup     = c("grade"),
  examinees = c("grade", "score", "freq")
)

#' Prepare Embedded Standard Setting input data
#'
#' Builds the input for [emstans()] from data frames or files, in any column
#' layout. Ratings and items are required; setup information (level labels,
#' SD, EC) can come from a setup table, from arguments, or both, with
#' arguments taking precedence.
#'
#' @param ratings Panelists' item alignments: a data frame or a path to a
#'   `.csv` file. One row per panelist, item and round.
#' @param items Item information, including the location column(s) such as
#'   `RP67`: a data frame or a path to a `.csv` file.
#' @param setup Optional setup table (one row per grade), a data frame or a
#'   path to a `.csv` file. Its `Level Options` column lists the level labels
#'   from lowest to highest, separated by commas.
#' @param examinees Optional score frequencies for impact data: a data frame
#'   or a path to a `.csv` file in long format, one row per grade and score,
#'   with three columns: grade (`GCA` or `Grade`), `Score` (on the same scale
#'   as the item locations) and `Freq` (number of examinees with that score).
#'   Wide tables with repeated Score/Freq/Grade blocks side by side are not
#'   accepted.
#' @param levels Level labels from lowest to highest. A character vector
#'   applies to every grade; a named list gives labels per grade, for example
#'   `list(R1 = c("E", "M", "L"), R2 = c("B", "P", "A"))`. Overrides the setup
#'   table.
#' @param sd,ec Scaling SD and essential-consistency threshold. A single
#'   number applies to every grade; a named vector gives values per grade.
#'   Override the setup table. Defaults are SD = 1 and EC = 0.
#' @param columns Optional column mapping for tables whose column names
#'   differ from the defaults. A named list per table, each a named list or
#'   vector of input column names or positions, for example
#'   `list(ratings = list(item_id = "ItemID", level = 8))`. The standard names
#'   are: ratings `grade`, `round`, `table`, `panelist`, `item_id`, `level`;
#'   items `grade`, `item_id`, `domain`, `order`; setup `grade`, `levels`,
#'   `sd`, `ec`; examinees `grade`, `score`, `freq`.
#' @param quiet If `FALSE`, report the column mapping that was used.
#'
#' @return An object of class `"ess_data"`.
#'
#' @examples
#' \dontrun{
#' d <- ess_data(
#'   ratings = "ratings.csv",
#'   items   = "items.csv",
#'   levels  = c("Basic", "Intermediate", "Advanced"),
#'   columns = list(ratings = list(panelist = "Rater", level = "Judgment"))
#' )
#' res <- emstans(data = d, loc = "RP67")
#' }
#' @export
ess_data <- function(ratings, items, setup = NULL, examinees = NULL,
                     levels = NULL, sd = NULL, ec = NULL, columns = NULL,
                     quiet = TRUE) {

  unknown <- setdiff(names(columns), names(.ess_default_columns))
  if (length(unknown)) {
    stop("`columns` has unknown table name(s): ", paste(unknown, collapse = ", "),
         ". Use ratings, items, setup or examinees.", call. = FALSE)
  }

  raw <- list(ratings = .ess_read(ratings, "ratings"),
              items   = .ess_read(items, "items"),
              setup   = if (!is.null(setup)) .ess_read(setup, "setup"),
              examinees = if (!is.null(examinees)) .ess_read(examinees, "examinees"))

  mapping <- list()
  for (tbl in names(raw)) {
    if (is.null(raw[[tbl]])) next
    if (tbl == "examinees") raw[[tbl]] <- .ess_check_examinees(raw[[tbl]])
    if (tbl == "examinees" && nrow(raw[[tbl]]) == 0) { raw[tbl] <- list(NULL); next }
    mapping[[tbl]] <- .ess_resolve_columns(raw[[tbl]], tbl, columns[[tbl]])
  }
  if (!quiet) .ess_report_mapping(mapping)

  # Ratings ---------------------------------------------------------------
  r <- raw$ratings; m <- mapping$ratings
  ratings_df <- data.frame(
    grade    = .ess_chr(r[[m[["grade"]]]]),
    round    = if (!is.na(m[["round"]])) .ess_chr(r[[m[["round"]]]]) else "1",
    table    = if (!is.na(m[["table"]])) .ess_chr(r[[m[["table"]]]]) else "1",
    panelist = .ess_chr(r[[m[["panelist"]]]]),
    item_id  = .ess_chr(r[[m[["item_id"]]]]),
    label    = .ess_chr(r[[m[["level"]]]]),
    stringsAsFactors = FALSE
  )
  if (anyNA(ratings_df[c("grade", "panelist", "item_id")])) {
    stop("Ratings have missing grade, panelist or item ID in row(s): ",
         .ess_show(which(!stats::complete.cases(ratings_df[c("grade", "panelist", "item_id")]))),
         ".", call. = FALSE)
  }

  # Items -----------------------------------------------------------------
  it <- raw$items; m <- mapping$items
  std <- c("grade", "item_id", "domain", "order")
  items_df <- data.frame(
    grade   = .ess_chr(it[[m[["grade"]]]]),
    item_id = .ess_chr(it[[m[["item_id"]]]]),
    domain  = if (!is.na(m[["domain"]])) .ess_chr(it[[m[["domain"]]]]) else NA_character_,
    order   = if (!is.na(m[["order"]])) suppressWarnings(as.numeric(it[[m[["order"]]]])) else NA_real_,
    stringsAsFactors = FALSE
  )
  items_df$row <- seq_len(nrow(items_df))   # input order, the last tie-breaker
  extra <- setdiff(names(it), stats::na.omit(unname(m)))
  extra <- extra[!toupper(extra) %in% toupper(c(std, "row"))]
  for (nm in extra) items_df[[nm]] <- it[[nm]]

  # Grades, levels, SD, EC ------------------------------------------------
  grades <- unique(c(ratings_df$grade))
  setup_df <- NULL
  if (!is.null(raw$setup)) {
    s <- raw$setup; m <- mapping$setup
    setup_df <- data.frame(grade = .ess_chr(s[[m[["grade"]]]]), stringsAsFactors = FALSE)
    setup_df$levels <- if (!is.na(m[["levels"]])) .ess_chr(s[[m[["levels"]]]]) else NA_character_
    setup_df$sd <- if (!is.na(m[["sd"]])) s[[m[["sd"]]]] else NA
    setup_df$ec <- if (!is.na(m[["ec"]])) s[[m[["ec"]]]] else NA
    grades <- unique(c(setup_df$grade[setup_df$grade %in% grades], grades))
  }

  levels_df <- .ess_levels(grades, setup_df, levels)
  grades_df <- data.frame(
    grade = grades,
    sd = .ess_per_grade(grades, sd, setup_df, "sd", default = 1),
    ec = .ess_per_grade(grades, ec, setup_df, "ec", default = 0),
    stringsAsFactors = FALSE
  )

  # Level labels -> integer ranks (exact match after removing white space,
  # as before; labels are case sensitive).
  key <- paste(ratings_df$grade, .ess_squish(ratings_df$label), sep = "\r")
  lk  <- paste(levels_df$grade, .ess_squish(levels_df$label), sep = "\r")
  ratings_df$level <- levels_df$level[match(key, lk)]
  if (anyNA(ratings_df$level)) {
    bad <- unique(ratings_df[is.na(ratings_df$level), c("grade", "label")])
    allowed <- levels_df$label[levels_df$grade == bad$grade[1]]
    stop("ALD label(s) not found in the grade's level labels: ",
         .ess_show(paste0(bad$grade, ": \"", bad$label, "\"")),
         ". Allowed for ", bad$grade[1], ": ", paste(allowed, collapse = ", "), ".",
         call. = FALSE)
  }

  # Examinees -------------------------------------------------------------
  if (!is.null(raw$examinees)) {
    e <- raw$examinees; m <- mapping$examinees
    examinees_df <- data.frame(
      grade = .ess_chr(e[[m[["grade"]]]]),
      score = suppressWarnings(as.numeric(e[[m[["score"]]]])),
      freq  = suppressWarnings(as.numeric(e[[m[["freq"]]]])),
      stringsAsFactors = FALSE
    )
    bad <- which(is.na(examinees_df$grade) | is.na(examinees_df$score) |
                   is.na(examinees_df$freq) | examinees_df$freq < 0 |
                   examinees_df$freq != round(examinees_df$freq))
    if (length(bad)) {
      stop("Examinee scores need a grade, a numeric Score and a whole, non-negative ",
           "Freq in every row. Problem in row(s): ", .ess_show(bad), ".", call. = FALSE)
    }
  } else {
    examinees_df <- data.frame(grade = character(0), score = numeric(0), freq = numeric(0))
  }

  out <- structure(list(
    ratings   = ratings_df[c("grade", "round", "table", "panelist", "item_id", "level", "label")],
    items     = items_df,
    levels    = levels_df,
    grades    = grades_df,
    examinees = examinees_df,
    columns   = mapping
  ), class = "ess_data")

  validate_ess_data(out)
}

#' Read an emss Excel workbook
#'
#' Reads the five-sheet emss workbook (Setup, Panelists, Ratings,
#' ItemMetaData, ExamineeMetaData) by sheet name, falling back to sheet
#' position when a name is not found, and returns [ess_data()].
#'
#' @param path Path to the `.xlsx` file.
#' @param sheets Optional sheet names, a named character vector with any of
#'   `setup`, `ratings`, `items`, `examinees`.
#' @inheritParams ess_data
#' @return An object of class `"ess_data"`.
#' @export
ess_read_excel <- function(path, sheets = NULL, levels = NULL, sd = NULL,
                           ec = NULL, columns = NULL, quiet = TRUE) {
  available <- readxl::excel_sheets(path)
  defaults <- c(setup = "Setup", ratings = "Ratings", items = "ItemMetaData",
                examinees = "ExamineeMetaData")
  position <- c(setup = 1, ratings = 3, items = 4, examinees = 5)
  want <- defaults; want[names(sheets)] <- sheets

  pick <- function(tbl) {
    hit <- available[.ess_norm(available) == .ess_norm(want[[tbl]])]
    if (length(hit)) return(hit[1])
    if (!tbl %in% names(sheets) && length(available) >= position[[tbl]]) {
      message("Sheet `", want[[tbl]], "` not found; using sheet ", position[[tbl]],
              " (`", available[position[[tbl]]], "`) as ", tbl, ".")
      return(available[position[[tbl]]])
    }
    if (tbl %in% c("setup", "examinees")) return(NA_character_)
    stop("Sheet `", want[[tbl]], "` not found in ", basename(path), ".", call. = FALSE)
  }
  read <- function(tbl, ...) {
    sh <- pick(tbl)
    if (is.na(sh)) return(NULL)
    suppressMessages(readxl::read_excel(path, sheet = sh, ...))
  }

  ess_data(
    ratings   = read("ratings", col_types = "text"),
    items     = read("items"),
    setup     = read("setup"),
    examinees = read("examinees"),
    levels = levels, sd = sd, ec = ec, columns = columns, quiet = quiet
  )
}

#' @export
print.ess_data <- function(x, ...) {
  r <- x$ratings
  cat("emss input data\n")
  cat(sprintf("  %d grade(s), %d round(s), %d panelist(s), %d item(s), %d rating(s)\n",
              length(unique(r$grade)), length(unique(r$round)),
              nrow(unique(r[c("grade", "round", "panelist")])),
              nrow(x$items), nrow(r)))
  for (g in unique(x$levels$grade)) {
    cat(sprintf("  %-8s levels: %s\n", g,
                paste(x$levels$label[x$levels$grade == g], collapse = " < ")))
  }
  cat(sprintf("  examinee scores: %s\n",
              if (nrow(x$examinees)) paste(sum(x$examinees$freq), "examinees") else "not provided"))
  invisible(x)
}

#' Check prepared input data
#'
#' Stops with a clear message when the data cannot be analysed: item IDs that
#' are not unique, SD or EC that are not numbers, ratings for items that are
#' not in the item data, or a panelist who did not rate every item of the
#' grade exactly once within a round.
#'
#' @param x An object of class `"ess_data"`.
#' @return `x`, invisibly.
#' @keywords internal
validate_ess_data <- function(x) {
  items <- x$items; ratings <- x$ratings

  dup <- items$item_id[duplicated(items$item_id)]
  if (length(dup)) {
    stop("Item IDs must be unique in the item data. Duplicated: ", .ess_show(dup), ".",
         call. = FALSE)
  }

  for (col in c("sd", "ec")) {
    v <- x$grades[[col]]
    if (!is.numeric(v) || anyNA(v)) {
      bad <- x$grades$grade[is.na(suppressWarnings(as.numeric(v)))]
      stop("`", toupper(col), "` is not a number for grade(s): ", .ess_show(bad), ".",
           call. = FALSE)
    }
  }

  no_items <- setdiff(unique(ratings$grade), unique(items$grade))
  if (length(no_items)) {
    stop("Grade(s) with ratings but no items: ", .ess_show(no_items), ".", call. = FALSE)
  }

  key <- paste(ratings$grade, ratings$round, ratings$panelist, sep = "\r")
  for (k in unique(key)) {
    r <- ratings[key == k, , drop = FALSE]
    g <- r$grade[1]
    grade_items <- items$item_id[items$grade == g]
    missing <- setdiff(grade_items, r$item_id)
    extra   <- setdiff(r$item_id, grade_items)
    twice   <- r$item_id[duplicated(r$item_id)]
    if (length(missing) + length(extra) + length(twice) > 0) {
      stop("Ratings for panelist `", r$panelist[1], "` in grade `", g, "`",
           if (length(unique(ratings$round)) > 1) paste0(", round ", r$round[1]),
           " do not match the item data",
           if (length(missing)) paste0("; not rated: ", .ess_show(missing)),
           if (length(extra))   paste0("; not in item data: ", .ess_show(extra)),
           if (length(twice))   paste0("; rated more than once: ", .ess_show(twice)),
           ". Every panelist must rate every item in the grade exactly once per round.",
           call. = FALSE)
    }
  }
  invisible(x)
}

# Convert the older list-of-data-frames input (setup, panelist, rating,
# item_data, examinee_data, by position) into ess_data.
as_ess_data <- function(x, ...) {
  if (inherits(x, "ess_data")) return(x)
  if (!is.list(x) || length(x) < 4) {
    stop("`data` must be an ess_data object or a list of data frames ",
         "(setup, panelist, rating, item data, examinee data).", call. = FALSE)
  }
  ess_data(ratings = x[[3]], items = x[[4]], setup = x[[1]],
           examinees = if (length(x) >= 5) x[[5]], ...)
}

# Helpers ---------------------------------------------------------------------

.ess_read <- function(x, what) {
  if (is.data.frame(x)) return(as.data.frame(x, stringsAsFactors = FALSE))
  if (is.character(x) && length(x) == 1) {
    if (!file.exists(x)) stop("File not found for ", what, ": ", x, call. = FALSE)
    ext <- tolower(sub(".*\\.([^.]+)$", "\\1", basename(x)))
    if (ext %in% c("csv", "txt", "tsv")) {
      return(as.data.frame(data.table::fread(x, colClasses = "character",
                                             encoding = "UTF-8", na.strings = c("", "NA")),
                           stringsAsFactors = FALSE) |> .ess_type_convert())
    }
    if (ext %in% c("xlsx", "xls")) {
      return(as.data.frame(suppressMessages(readxl::read_excel(x))))
    }
    stop("Unsupported file type for ", what, ": .", ext, call. = FALSE)
  }
  stop("`", what, "` must be a data frame or a file path.", call. = FALSE)
}

# CSV columns are read as text so IDs keep leading zeros; convert columns that
# are entirely numbers back to numeric (ID columns are converted to character
# again when the standard tables are built).
.ess_type_convert <- function(df) {
  for (nm in names(df)) {
    v <- df[[nm]]
    num <- suppressWarnings(as.numeric(v))
    if (all(is.na(v) == is.na(num))) df[[nm]] <- num
  }
  df
}

.ess_norm <- function(x) toupper(gsub("[[:space:]_.]+", "", x))
.ess_squish <- function(x) stringi::stri_replace_all_charclass(x, "\\p{WHITE_SPACE}", "")
.ess_chr <- function(x) {
  if (is.numeric(x)) x <- format(x, scientific = FALSE, trim = TRUE, drop0trailing = TRUE)
  x <- as.character(x)
  x[x %in% c("NA", "")] <- NA
  trimws(x)
}
.ess_show <- function(x, n = 10) {
  x <- unique(as.character(x))
  paste0(paste(utils::head(x, n), collapse = ", "),
         if (length(x) > n) paste0(", ... (", length(x), " in total)") else "")
}

# Resolve the standard columns of one table to input column names.
.ess_resolve_columns <- function(df, tbl, user) {
  defaults <- .ess_default_columns[[tbl]]
  unknown <- setdiff(names(user), names(defaults))
  if (length(unknown)) {
    stop("Unknown column role(s) for ", tbl, ": ", paste(unknown, collapse = ", "),
         ". Available: ", paste(names(defaults), collapse = ", "), ".", call. = FALSE)
  }
  out <- stats::setNames(rep(NA_character_, length(defaults)), names(defaults))
  for (role in names(defaults)) {
    spec <- if (!is.null(user[[role]])) user[[role]] else NULL
    if (!is.null(spec)) {
      spec <- spec[[1]]
      if (is.numeric(spec)) {
        if (spec < 1 || spec > ncol(df)) {
          stop(tbl, " column position ", spec, " for `", role, "` is out of range (1-",
               ncol(df), ").", call. = FALSE)
        }
        out[[role]] <- names(df)[spec]
      } else {
        hit <- names(df)[.ess_norm(names(df)) == .ess_norm(spec)]
        if (!length(hit)) {
          stop("Column `", spec, "` (", role, ") not found in ", tbl, ". Columns: ",
               .ess_show(names(df), 30), ".", call. = FALSE)
        }
        out[[role]] <- hit[1]
      }
    } else {
      for (cand in defaults[[role]]) {
        hit <- names(df)[.ess_norm(names(df)) == .ess_norm(cand)]
        if (length(hit)) { out[[role]] <- hit[1]; break }
      }
    }
  }
  missing <- setdiff(.ess_required_columns[[tbl]], names(out)[!is.na(out)])
  if (length(missing)) {
    stop("Required column(s) not found in ", tbl, ": ",
         paste0(missing, " (default name: ",
                vapply(defaults[missing], `[`, character(1), 1), ")", collapse = ", "),
         ". Columns present: ", .ess_show(names(df), 30),
         ". Use `columns = list(", tbl, " = list(", missing[1], " = ...))` to map them.",
         call. = FALSE)
  }
  out
}

.ess_report_mapping <- function(mapping) {
  for (tbl in names(mapping)) {
    m <- mapping[[tbl]][!is.na(mapping[[tbl]])]
    message(tbl, ": ", paste0(names(m), " = `", m, "`", collapse = ", "))
  }
}

# Examinee scores must be one long score-frequency table (grade, Score,
# Freq). Empty rows and columns are dropped; an empty table means no
# examinee data. A wide table, with Score/Freq/Grade blocks repeated side by
# side (one block per grade), is refused: blocks of unequal length and the
# empty separator columns between them make it easy to misread.
.ess_check_examinees <- function(df) {
  if (is.null(df) || nrow(df) == 0 || ncol(df) == 0) return(data.frame())
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  if (nrow(df) == 0 || ncol(df) == 0) return(data.frame())
  
  # Excel readers suffix repeated header names ("Score...1", "Score...5").
  base <- .ess_norm(sub("\\.\\.\\.[0-9]+$", "", names(df)))
  repeated <- intersect(base[duplicated(base)], c("SCORE", "FREQ", "GRADE", "GCA"))
  if (length(repeated)) {
    stop("Examinee scores must be one long table with one row per grade and score, ",
         "and three columns: GCA (or Grade), Score and Freq (the number of examinees ",
         "with that score). This table repeats ",
         paste0("`", tolower(repeated), "`", collapse = ", "),
         " in several column blocks (a wide layout), which is not accepted. ",
         "Stack the blocks into the three columns, for example:\n",
         "  GCA  Score  Freq\n  R3   280    12\n  R3   281    15\n  R4   290     9",
         call. = FALSE)
  }
  # One block left (e.g. a sheet whose other blocks are empty): it is a long
  # table, so drop the readers' "...N" suffixes to match the column names.
  names(df) <- sub("\\.\\.\\.[0-9]+$", "", names(df))
  df
}

.ess_levels <- function(grades, setup_df, levels) {
  per_grade <- lapply(grades, function(g) {
    lab <- NULL
    if (!is.null(levels)) {
      if (is.list(levels)) lab <- levels[[g]] else lab <- levels
    }
    if (is.null(lab) && !is.null(setup_df)) {
      opt <- setup_df$levels[setup_df$grade == g]
      if (length(opt) && !is.na(opt[1])) lab <- strsplit(opt[1], ",", fixed = TRUE)[[1]]
    }
    if (is.null(lab)) {
      stop("No level labels for grade `", g, "`. Give them in the setup table's ",
           "Level Options column or with `levels = c(...)`.", call. = FALSE)
    }
    lab <- trimws(as.character(lab))
    lab <- lab[lab != ""]
    if (length(lab) < 2) stop("Grade `", g, "` needs at least two levels.", call. = FALSE)
    if (anyDuplicated(.ess_squish(lab))) {
      stop("Level labels for grade `", g, "` are not unique: ", paste(lab, collapse = ", "),
           ".", call. = FALSE)
    }
    data.frame(grade = g, level = seq_along(lab), label = lab, stringsAsFactors = FALSE)
  })
  do.call(rbind, per_grade)
}

.ess_per_grade <- function(grades, arg, setup_df, col, default) {
  val <- rep(NA_real_, length(grades))
  if (!is.null(setup_df)) {
    v <- setup_df[[col]][match(grades, setup_df$grade)]
    num <- suppressWarnings(as.numeric(v))
    bad <- !is.na(v) & is.na(num)
    if (any(bad)) {
      stop("`", toupper(col), "` in the setup data is not a number for grade(s): ",
           .ess_show(grades[bad]), ".", call. = FALSE)
    }
    val <- num
  }
  if (!is.null(arg)) {
    if (!is.numeric(arg)) stop("`", col, "` must be numeric.", call. = FALSE)
    if (is.null(names(arg))) {
      if (length(arg) != 1) stop("Unnamed `", col, "` must be a single number.", call. = FALSE)
      val[] <- arg
    } else {
      hit <- match(grades, names(arg))
      val[!is.na(hit)] <- arg[hit[!is.na(hit)]]
    }
  }
  val[is.na(val)] <- default
  val
}
