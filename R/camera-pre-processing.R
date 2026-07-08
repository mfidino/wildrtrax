#' Locate image files within a directory
#'
#' Recursively searches a directory for image files and returns their file
#' paths. By default, the function searches for JPEG images and returns
#' full file paths.
#'
#' @param path A character string specifying the root directory containing
#'   image files.
#' @param pattern A regular expression used to identify image files. The
#'   default searches for files containing `"jpg"` or `"JPEG"` in their
#'   names.
#' @param ignore.case Logical. Should pattern matching ignore letter case?
#'   Defaults to `TRUE`.
#'
#' @details
#' This function is intended as the first step in preparing image data for
#' WildTrax workflows. The returned file paths can be supplied to functions
#' that read image metadata or modify EXIF information.
#'
#' @return
#' A character vector containing the file paths of all images matching
#' `pattern`.
#'
#' @examples
#' \dontrun{
#'
#' # Locate all JPEG images beneath a project directory
#' imgs <- wt_image_paths(
#'   path = "my_sample"
#' )
#'
#'
#' }
#'
#' @export

wt_image_paths <- function(
    path,
    pattern = "jpg|JPEG",
    ignore.case = TRUE
){
  # checks for the various arguments
  if (!is.character(path) || length(path) != 1) {
    stop(
      "`path` must be a single character string.",
      call. = FALSE
    )
  }

  if (!dir.exists(path)) {
    stop(
      "`path` does not exist.",
      call. = FALSE
    )
  }

  if (!is.character(pattern) || length(pattern) != 1) {
    stop(
      "`pattern` must be a single character string.",
      call. = FALSE
    )
  }

  if (!is.logical(ignore.case) || length(ignore.case) != 1) {
    stop(
      "`ignore.case` must be TRUE or FALSE.",
      call. = FALSE
    )
  }
  to_return <- list.files(
    path = path,
    pattern = pattern,
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = ignore.case
  )
  return(to_return)
}
#' Summarize the folder hierarchy of file paths
#'
#' Summarizes the folder hierarchy represented by a set of image file paths.
#' The function prints the folder levels common to all paths, helping users
#' identify which directory level corresponds to grouping variables (e.g.,
#' sites) prior to downstream processing.
#'
#' @param paths A character vector of file paths, typically produced by
#'   [wt_image_paths()].
#' @param path_split Character string used to split file paths into their
#'   component folders. If `NULL`, defaults to the operating system's file
#'   separator.
#'
#' @details
#' This function is intended to help users understand the directory structure
#' of image datasets before extracting grouping variables with
#' [wt_extract_folder_level()].
#'
#' If all file paths have the same folder depth, the complete folder hierarchy
#' is printed using an example path. If folder depths differ, a warning is
#' issued reporting the observed depths, and only the folder levels common to
#' all paths (i.e., up to the minimum folder depth) are displayed. This
#' accommodates datasets where some images are stored in additional
#' subdirectories while preserving the common hierarchy shared across all
#' images.
#'
#' @return
#' No value is returned.
#'
#' @examples
#' \dontrun{
#'
#' imgs <- wt_image_paths("my_sample")
#'
#' wt_path_summary(imgs)
#'
#' }
#'
#' @export
wt_path_summary <- function(
    paths,
    path_split = NULL
){

  if (!is.character(paths)) {
    stop(
      "`paths` must be a character vector.",
      call. = FALSE
    )
  }

  if(anyNA(paths)){
    stop(
      "`paths` cannot contain missing values.",
      call. = FALSE
    )
  }

  if(length(paths) == 0){
    stop(
      "`paths` contains no file paths.",
      call. = FALSE
    )
  }

  if(is.null(path_split)){
    path_split <- .Platform$file.sep
  } else {

    if(!is.character(path_split) || length(path_split) != 1){
      stop(
        "`path_split` must be NULL or a single character string.",
        call. = FALSE
      )
    }

  }

  path_summary <- strsplit(
    dirname(paths),
    path_split,
    fixed = TRUE
  )

  path_lengths <- lengths(path_summary)

  min_depth <- min(path_lengths)
  max_depth <- max(path_lengths)

  if(min_depth != max_depth){

    warning(
      "Not all files have the same folder depth.\n",
      "Depths observed: ",
      paste(
        sort(unique(path_lengths)),
        collapse = ", "
      ),
      ".\n",
      "The hierarchy below is shown only to the minimum folder depth (",
      min_depth,
      " levels). Additional folder levels may occur after those shown.",
      call. = FALSE
    )

  } else {

    cat(
      sprintf(
        "All files occur at the same depth (%i levels).\n\n",
        min_depth
      )
    )

  }

  example_path <- which.min(path_lengths)

  cat("Folder hierarchy (example path):\n\n")

  for(i in seq_len(min_depth)){
    cat(
      sprintf(
        "Level %i: %s\n",
        i,
        path_summary[[example_path]][i]
      )
    )
  }

  invisible(NULL)

}

#' Extract a folder level from file paths
#'
#' Extracts the folder name at a specified level of a file path hierarchy.
#' This is useful for creating grouping variables (e.g., site identifiers)
#' from image directory structures.
#'
#' @param paths A character vector of file paths, typically produced by
#'   [wt_image_paths()].
#' @param level A positive integer specifying the folder level to extract.
#'   Folder levels can be identified using [wt_path_summary()].
#' @param path_split Character string used to split file paths into their
#'   component folders. If `NULL`, defaults to the operating system's file
#'   separator.
#'
#' @details
#' File paths are first stripped of their filenames before being split into
#' folder components. The requested folder level is then extracted from each
#' path.
#'
#' This function is primarily intended to create grouping variables for
#' downstream functions such as [wt_image_datetime()].
#'
#' @return
#' A character vector containing the folder name at the requested level for
#' each input file path.
#'
#' @examples
#' \dontrun{
#'
#' imgs <- wt_image_paths("my_sample")
#'
#' # Extract the first folder level (e.g., site)
#' site <- wt_extract_folder_level(
#'   imgs,
#'   level = 1
#' )
#'
#' # Extract the second folder level
#' camera <- wt_extract_folder_level(
#'   imgs,
#'   level = 2
#' )
#'
#' }
#'
#' @export

wt_extract_folder_level <- function(
    paths,
    level,
    path_split = NULL
  ){

    if (!is.character(paths)) {
      stop(
        "`paths` must be a character vector.",
        call. = FALSE
      )
    }
    if(anyNA(paths)){
      stop(
        "`paths` cannot contain missing values.",
        call. = FALSE
      )
    }
    if (length(paths) == 0) {
      stop(
        "`paths` contains no file paths.",
        call. = FALSE
      )
    }

    if (!is.numeric(level)|| level < 1 || length(level) != 1){
      stop(
        "`level` must be a positive integer.",
        call. = FALSE
      )
    }

    if(is.null(path_split)){
      path_split <- .Platform$file.sep
    } else {
      if ( (!is.character(path_split) || length(path_split) != 1)) {
        stop(
          "`path_split` must be NULL or a single character string.",
          call. = FALSE
        )
      }
    }

    folder_level <- strsplit(
      dirname(paths),
      path_split,
      fixed = TRUE
    )
    folder_lengths <- lengths(
      folder_level
    )
    if(!all(folder_lengths >= level)){
      stop(
        "Some paths have less sub-folders than the level specified.",
        call. = FALSE
      )
    }
    folder_level <- sapply(
      folder_level,
      "[[",
      level
    )

    return(folder_level)
}



#' Extract image datetimes from EXIF metadata
#'
#' Uses \pkg{exifr} to extract the `DateTimeOriginal` field from image EXIF
#' metadata. Images can either be processed exactly or sampled to provide a
#' faster estimate of the date range represented within a dataset.
#'
#' @param paths A character vector of image file paths, typically produced by
#'   [wt_image_paths()].
#' @param method Character string specifying the extraction method. `"exact"`
#'   extracts EXIF metadata from every image. `"fast"` extracts metadata from
#'   the first and last `n` images, optionally within groups.
#' @param group An optional vector used to group images (e.g., site), typically
#'   produced by [wt_extract_folder_level()]. Must have the same length as
#'   `paths`. When provided, the `"fast"` method extracts metadata from the
#'   first and last `n` images within each group.
#' @param n Number of images to sample from the beginning and end of each
#'   group when `method = "fast"`. Defaults to 5.
#' @param tz Time zone used when converting `DateTimeOriginal` to a
#'   `POSIXct` object. Must be one of the values returned by
#'   [OlsonNames()]. If `NULL` (default), datetimes are returned as
#'   character strings.
#'
#' @details
#' The `"fast"` method is intended for quickly assessing the datetimes
#' of a large image dataset without reading EXIF metadata from every file.
#' Images that are not sampled will have `NA` values for their extracted
#' datetime. This is a fast way to quickly check if there are any datetime
#' errors in your image set.
#'
#' The `"fast"` method assumes that `paths` are ordered chronologically within
#' groups. Users should ensure that file paths are sorted appropriately before
#' running this function.
#'
#' @return
#' A data frame with one row per input image containing:
#' \itemize{
#'   \item `path`: image file path
#'   \item `group`: grouping variable, if supplied
#'   \item `DateTimeOriginal`: the extracted datetime. This is returned
#'   as a `POSIXct` object when `tz` is supplied and as a character vector
#'   otherwise.
#' }
#'
#' @examples
#' \dontrun{
#'
#' imgs <- wt_image_paths("my_sample")
#'
#' # Extract metadata from all images
#' wt_image_datetime(
#'   imgs,
#'   method = "exact"
#' )
#'
#' # Quickly estimate date range by site
#' wt_image_datetime(
#'   imgs,
#'   method = "fast",
#'   group = site,
#'   tz = "US/Central"
#' )
#'
#' }
#' @importFrom exifr read_exif
#' @export
wt_image_datetime <- function(
    paths,
    method = c("fast", "exact"),
    group = NULL,
    n = 5,
    tz = NULL
){

  if(!is.character(paths)){
    stop(
      "`paths` must be a character vector.",
      call. = FALSE
    )
  }

  if(length(paths) == 0){
    stop(
      "`paths` contains no file paths.",
      call. = FALSE
    )
  }

  if(anyNA(paths)){
    stop(
      "`paths` cannot contain missing values.",
      call. = FALSE
    )
  }

  method <- match.arg(
    method
  )

  if(!is.null(group)){
    if(length(group) != length(paths)){
      stop(
        "`group` must have the same length as `paths`.",
        call. = FALSE
      )
    }

    if(!is.character(group) && !is.factor(group)){
      stop(
        "`group` must be either a character vector or factor.",
        call. = FALSE
      )
    }

    group <- as.factor(group)

  }
  if(!is.null(tz)){
    if(!is.character(tz)){
      stop(
        "`tz` must be a characeter object",
        call. = FALSE
      )
    }
    if(!tz %in% OlsonNames()){
      stop(
        "`tz` must be a timezone represented in `OlsonNames()`",
        call. = FALSE
      )
    }
  }

  if(!is.numeric(n) || length(n) != 1 || n < 1 || n %% 1 != 0){
    stop(
      "`n` must be a single positive integer.",
      call. = FALSE
    )
  }


  # The output to return,
  #  creating because if method = "fast"
  #  then  we want to have the NA
  #  values for DateTimeOriginal
  out <- data.frame(
    path = paths,
    DateTimeOriginal = NA_character_
  )

  if(!is.null(group)){
    out$group <- group
  }

  # determine images to query
  if(method == "exact"){

    idx <- seq_along(paths)

  } else {
    if(is.null(group)){
      idx <- unique(
        c(
          seq_len(min(n, length(paths))),
          tail(seq_along(paths), n)
        )
      )
    } else {
      idx <- unlist(
        lapply(
          split(seq_along(paths), group),
          function(x) {
            unique(
              c(
                head(x, n),
                tail(x, n)
              )
            )
          }
        )
      )
    }
  }

  # extract EXIF information
  exif <- exifr::read_exif(
    paths[idx],
    tags = "DateTimeOriginal"
  )
  # parse the datetimes a bit so they
  #  are not all split by colons
  exif$DateTimeOriginal <- sub(
    "^([0-9]{4}):([0-9]{2}):([0-9]{2})",
    "\\1-\\2-\\3",
    exif$DateTimeOriginal
  )
  if(!is.null(tz)){
    exif$DateTimeOriginal <- as.POSIXct(
      exif$DateTimeOriginal,
      tz = tz
    )
  } else {
    warning(
      "`tz` was NULL so DateTimeOriginal is a character object.",
      call. = FALSE
    )
  }

  match_idx <- match(
    tolower(
      normalizePath(
        paths[idx]
      )
    ),
    tolower(
      normalizePath(
        exif$SourceFile
      )
    )
  )
  if(!is.null(tz)){
    out$DateTimeOriginal <- as.POSIXct(
      rep(NA_character_, nrow(out)),
      tz = tz
    )
  }

  out$DateTimeOriginal[idx] <-
    exif$DateTimeOriginal[match_idx]


  return(out)

}


#' Check image datetimes against expected years
#'
#' Internal helper function used by [wt_image_check()] to identify groups
#' containing image datetimes outside the expected years.
#'
#' @keywords internal
wt_check_datetime_year <- function(
    x,
    years = NULL
){

  if(is.null(years)){
    return(
      list(
        passed = TRUE,
        flagged_groups = character(0)
      )
    )
  }

  yrs <- as.integer(format(x$DateTimeOriginal, "%Y"))

  bad <- tapply(
    yrs,
    x$group,
    function(z){
      any(!is.na(z) & !z %in% years)
    }
  )

  flagged <- names(bad)[bad]

  list(
    passed = length(flagged) == 0,
    flagged_groups = flagged
  )

}


#' Check image datetimes against expected months
#'
#' Internal helper function used by [wt_image_check()] to identify groups
#' containing image datetimes outside the expected months.
#'
#' @keywords internal
wt_check_datetime_month <- function(
    x,
    months = NULL
){

  if(is.null(months)){
    return(
      list(
        passed = TRUE,
        flagged_groups = character(0)
      )
    )
  }

  mth <- as.integer(format(x$DateTimeOriginal, "%m"))

  bad <- tapply(
    mth,
    x$group,
    function(z){
      any(!is.na(z) & !z %in% months)
    }
  )

  flagged <- names(bad)[bad]

  list(
    passed = length(flagged) == 0,
    flagged_groups = flagged
  )

}

#' Identify image datetimes occurring on January 1
#'
#' Internal helper function used by [wt_image_check()] to identify groups
#' containing image timestamps on January 1, which can indicate incorrectly
#' initialized camera clocks.
#'
#' @keywords internal
wt_check_datetime_jan1 <- function(
    x
){

  first_dates <- tapply(
    x$DateTimeOriginal,
    x$group,
    function(z){

      z <- z[!is.na(z)]

      if(length(z) == 0){
        return(NA)
      }

      min(z)

    }
  )

  first_dates <- as.POSIXct(first_dates)

  jan1 <- !is.na(first_dates) &
    as.integer(format(first_dates, "%m")) == 1 &
    as.integer(format(first_dates, "%d")) == 1

  flagged <- names(first_dates)[jan1]

  list(
    passed = length(flagged) == 0,
    flagged_groups = flagged
  )

}


#' Identify invalid image datetimes
#'
#' Internal helper function used by [wt_image_check()] to identify groups with
#' image timestamps occurring before a minimum year or in the future.
#'
#' @param min_year Minimum acceptable year for image timestamps.
#'
#' @keywords internal
wt_check_invalid_datetime <- function(
    x,
    min_year = 1995
){

  yr <- as.integer(
    format(
      x$DateTimeOriginal,
      "%Y"
    )
  )

  future <- x$DateTimeOriginal > Sys.time()
  early <- yr < min_year

  early_groups <- tapply(
    early,
    x$group,
    function(z){
      any(z, na.rm = TRUE)
    }
  )

  future_groups <- tapply(
    future,
    x$group,
    function(z){
      any(z, na.rm = TRUE)
    }
  )

  early_groups <- names(early_groups)[early_groups]
  future_groups <- names(future_groups)[future_groups]

  list(
    passed = length(early_groups) == 0 &&
      length(future_groups) == 0,
    early_groups = early_groups,
    future_groups = future_groups
  )

}

#' Print image datetime QA/QC results
#'
#' Internal helper function used by [wt_image_check()] to format and print
#' datetime quality control results.
#'
#' @keywords internal
wt_print_datetime_report <- function(
    results,
    verbose = FALSE
){

  cat("\nImage datetime QA/QC\n")
  cat("====================\n\n")

  checks <- c(
    "Year check" = "year",
    "Month check" = "month",
    "January 1 check" = "jan1",
    "Invalid datetime check" = "invalid"
  )

  for(i in seq_along(checks)){

    name <- names(checks)[i]
    res <- results[[checks[i]]]

    if(res$passed){

      status <- "✓ Passed"

    } else {

      if(checks[i] == "invalid"){

        n_early <- length(res$early_groups)
        n_future <- length(res$future_groups)

        problems <- character(0)

        if(n_early > 0){
          problems <- c(
            problems,
            paste0(
              n_early,
              " early"
            )
          )
        }

        if(n_future > 0){
          problems <- c(
            problems,
            paste0(
              n_future,
              " future"
            )
          )
        }

        status <- paste0(
          "✗ Failed (",
          paste(
            problems,
            collapse = ", "
          ),
          ")"
        )

      } else {

        n <- length(res$flagged_groups)

        status <- paste0(
          "✗ Failed (",
          n,
          ifelse(
            n == 1,
            " group)",
            " groups)"
          )
        )
      }
    }

    cat(
      sprintf(
        "%-28s%s\n",
        name,
        status
      )
    )

    if(verbose && !res$passed){

      cat("\n")

      if(checks[i] == "invalid"){

        if(length(res$early_groups) > 0){

          cat("Groups with early dates:\n")
          cat(
            paste(
              res$early_groups,
              collapse = "\n"
            )
          )
          cat("\n\n")

        }

        if(length(res$future_groups) > 0){

          cat("Groups with future dates:\n")
          cat(
            paste(
              res$future_groups,
              collapse = "\n"
            )
          )
          cat("\n\n")

        }

      } else {

        cat("Groups:\n")
        cat(
          paste(
            res$flagged_groups,
            collapse = "\n"
          )
        )
        cat("\n\n")

      }
    }
  }

  invisible(NULL)

}


#' Check image datetimes for common quality issues
#'
#' Performs quality assurance checks on image timestamps extracted using
#' [wt_image_datetime()]. The function checks for timestamps outside expected
#' years or months, default camera timestamps occurring on January 1, groups
#' with constant timestamps, and invalid timestamps occurring before a
#' specified minimum year or in the future.
#'
#' @param x A data frame containing image datetime information, typically
#'   produced by [wt_image_datetime()]. The data frame must contain a
#'   `DateTimeOriginal` column stored as a `POSIXct` or `POSIXlt` object and
#'   a `group` column identifying image groups (e.g., sites).
#' @param years Optional numeric vector specifying acceptable years.
#'   Groups containing timestamps outside these years will be flagged.
#' @param months Optional numeric vector specifying acceptable months
#'   (1--12). Groups containing timestamps outside these months will be
#'   flagged.
#' @param min_year Minimum acceptable year for image timestamps. Defaults to
#'   1995, corresponding approximately to the introduction of Exif metadata.
#' @param verbose Logical. Should the report include the names of groups with
#'   detected issues? Defaults to `FALSE`, which prints only a compact summary
#'   of checks and the number of groups with potential issues. When `TRUE`,
#'   flagged groups are printed below each failed check. Flagged groups can
#'   also be accessed from the returned object.
#'
#' @details
#' This function summarizes several common camera timestamp issues that can
#' occur during wildlife camera deployments, including incorrectly set camera
#' clocks, uninitialized cameras, and timestamps outside the expected sampling
#' period.
#'
#' The function prints a summary of the quality checks and invisibly returns
#' the complete results. The returned object can be supplied directly to
#' [wt_datetime_followup()] to extract exact datetimes and further investigate
#' groups that failed one or more checks.
#'
#' @return
#' Invisibly returns a named list. Each quality check contains the check
#' result (`passed`) and the groups flagged by that check. The returned object
#' also contains a `check_arguments` element recording the values of
#' `years`, `months`, `min_year`, and the maximum allowable datetime used when
#' the checks were performed. These are retained so that
#' [wt_datetime_followup()] can reproduce the same quality checks on the full
#' set of image datetimes.
#'
#' @examples
#' \dontrun{
#'
#' imgs <- wt_image_paths("my_sample")
#'
#' datetime <- wt_image_datetime(
#'   imgs,
#'   group = site,
#'   tz = "US/Central"
#' )
#'
#' checks <- wt_image_check(
#'   datetime,
#'   years = 2024,
#'   months = 5:10
#' )
#'
#' }
#'
#' @export
wt_image_check <- function(
    x,
    years = NULL,
    months = NULL,
    min_year = 1995,
    verbose = FALSE
){

  if(!inherits(x$DateTimeOriginal, "POSIXt")){
    stop(
      "`x$DateTimeOriginal` must be a datetime object.",
      call. = FALSE
    )
  }

  if(!"group" %in% names(x)){
    stop(
      "`x` must contain a `group` column (see ?wt_image_datetime).",
      call. = FALSE
    )
  }
  if(!any(complete.cases(x))){
    cat("NA values present in x, removing rows with NA values\n")
    x <- x[complete.cases(x),]
  }


  # checks on x

  results <- list(
    year = wt_check_datetime_year(
      x,
      years
    ),
    month = wt_check_datetime_month(
      x,
      months
    ),
    jan1 = wt_check_datetime_jan1(
      x
    ),
    invalid = wt_check_invalid_datetime(
      x,
      min_year = min_year
    ),
    check_arguments = list(
      years = years,
      months = months,
      min_year = min_year,
      max_year = Sys.time()
    )
  )

  wt_print_datetime_report(
    results,
    verbose = verbose
  )

  invisible(results)

}



#' Investigate groups with datetime quality issues
#'
#' Extracts EXIF datetimes for groups flagged by [wt_image_check()] and
#' summarizes the extent of datetime issues within each group. This function
#' helps distinguish between deployment-level datetime errors, where all images
#' in a group are affected, and image-level errors that require inspection of
#' individual photographs.
#'
#' @param check The output from [wt_image_check()].
#' @param paths A character vector of image file paths, typically produced by
#'   [wt_image_paths()].
#' @param group A character vector or factor identifying image groups (e.g.,
#'   sites), typically produced by [wt_extract_folder_level()]. Must have the
#'   same length as `paths`.
#' @param tz Character string specifying the timezone used when converting EXIF
#'   timestamps to datetime objects. Must be one of the timezones returned by
#'   [OlsonNames()].
#'
#' @details
#' This function is intended to be run after [wt_image_check()] identifies one
#' or more groups with potential datetime problems. Exact EXIF datetimes are
#' extracted for all images within flagged groups and re-evaluated using the
#' same quality-control criteria supplied to [wt_image_check()].
#'
#' The returned summary reports the number of images in each flagged group,
#' the earliest and latest timestamps, the deployment duration in days, the
#' number of images with one or more datetime errors, and whether every image
#' in the group contains a datetime error. Groups where every image is flagged
#' often indicate a deployment-level camera clock issue, whereas groups with
#' only a subset of flagged images typically require manual inspection of
#' individual photographs.
#'
#' @return
#' A list containing:
#' \itemize{
#'   \item `summary`: A data frame with one row per flagged group containing
#'   the group identifier, earliest and latest image datetime, deployment
#'   duration (days), number of images, number of images with datetime errors,
#'   and whether all images in the group were flagged.
#'   \item `datetime`: The output from [wt_image_datetime()] for all images in
#'   the flagged groups.
#' }
#'
#' @examples
#' \dontrun{
#'
#' imgs <- wt_image_paths("my_sample")
#'
#' site <- wt_extract_folder_level(
#'   imgs,
#'   level = 1
#' )
#'
#' datetime <- wt_image_datetime(
#'   imgs,
#'   group = site,
#'   tz = "US/Central"
#' )
#'
#' check <- wt_image_check(
#'   datetime,
#'   years = 2024,
#'   months = 5:10
#' )
#'
#' followup <- wt_datetime_followup(
#'   check,
#'   imgs,
#'   site,
#'   tz = "US/Central"
#' )
#'
#' followup$summary
#'
#' }
#'
#' @export
wt_datetime_followup <- function(
    check,
    paths,
    group,
    tz
){

  if(!is.list(check) ||
     is.null(check$check_arguments)){
    stop(
      "`check` must be the output from `wt_image_check()`.",
      call. = FALSE
    )
  }

  if(!is.character(paths)){
    stop(
      "`paths` must be a character vector.",
      call. = FALSE
    )
  }

  if(length(paths) != length(group)){
    stop(
      "`group` must have the same length as `paths`.",
      call. = FALSE
    )
  }

  if(!is.character(group) && !is.factor(group)){
    stop(
      "`group` must be a character vector or factor.",
      call. = FALSE
    )
  }

  group <- as.factor(group)

  if(!is.character(tz) ||
     length(tz) != 1 ||
     !tz %in% OlsonNames()){
    stop(
      "`tz` must be a valid timezone represented in `OlsonNames()`.",
      call. = FALSE
    )
  }

  flagged_groups <- unique(
    unlist(
      lapply(
        check[names(check) != "check_arguments"],
        function(x){

          groups <- character(0)

          if("flagged_groups" %in% names(x)){
            groups <- c(
              groups,
              x$flagged_groups
            )
          }

          if("early_groups" %in% names(x)){
            groups <- c(
              groups,
              x$early_groups
            )
          }

          if("future_groups" %in% names(x)){
            groups <- c(
              groups,
              x$future_groups
            )
          }

          groups

        }
      ),
      use.names = FALSE
    )
  )

  if(length(flagged_groups) == 0){

    message(
      "No flagged groups found."
    )

    return(
      list(
        summary = NULL,
        datetime = NULL,
        flagged_groups = character(0)
      )
    )
  }

  idx <- group %in% flagged_groups
  tmp_group <- factor(
    group[idx]
  )

  cat(
    paste0(
      "Pulling datetime data from ",
      length(idx), " images...\n"
    )
  )

  datetime <- wt_image_datetime(
    paths[idx],
    method = "exact",
    group = tmp_group,
    tz = tz
  )

  datetime$group <- factor(
    datetime$group
  )

  args <- check$check_arguments

  summary <- data.frame(
    group = unique(
      as.character(datetime$group)
    )
  )

  # Add min / max datetime from image set
  summary$min_datetime <- as.POSIXct(
    tapply(
      datetime$DateTimeOriginal,
      datetime$group,
      min,
      na.rm = TRUE
    ),
    tz = tz
  )

  summary$max_datetime <- as.POSIXct(
    tapply(
      datetime$DateTimeOriginal,
      datetime$group,
      max,
      na.rm = TRUE
    ),
    tz = tz
  )

  # how long was deployment, based on min/max
  summary$datetime_range_days <- as.numeric(
    difftime(
      summary$max_datetime,
      summary$min_datetime,
      units = "days"
    )
  )

  # Check for year errors if year is available
  #  across all images.
  if(!is.null(args$years)){
    yr_errors <- !as.integer(format(datetime$DateTimeOriginal, "%Y")) %in%
      args$years
  } else {
    yr_errors <- rep(
      FALSE,
      nrow(datetime)
    )
  }
  # Check for month errors if months are available
  #  across all images
  if(!is.null(args$months)){
    month_errors <- !as.integer(format(datetime$DateTimeOriginal, "%m")) %in%
      args$months
  } else {
    month_errors <- rep(
      FALSE,
      nrow(datetime)
    )
  }
  # check the first image to see if it
  #  is a Jan 1. deployment.
  jan1_error_check <- wt_check_datetime_jan1(
    datetime
  )
  jan1_error <- rep(
    FALSE,
    nrow(datetime)
  )
  if(!jan1_error_check$passed){

    err_locs <- sapply(
      jan1_error_check$flagged_groups,
      function(x){
        min(
          which(
            datetime$group %in% x
          )
        )
      }
    )
    jan1_error[err_locs] <- TRUE
  }
  # check for invalid datetimes as well
  invalid_yr_errors <- as.integer(format(datetime$DateTimeOriginal, "%Y")) <
    args$min_year | datetime$DateTimeOriginal > args$max_year

  error_sum <- yr_errors + month_errors +
    jan1_error + invalid_yr_errors

  error_sum[error_sum>1] <- 1

  # number of photos per deployment
  summary$n_images <- as.integer(
    table(datetime$group)[summary$group]
  )
  # how many with errors?
  summary$n_images_with_error <- tapply(
    error_sum,
    datetime$group,
    sum
  )
  # is it a deployment wide error?
  summary$all_images_flagged <- as.integer(summary$n_images) ==
    as.integer(summary$n_images_with_error)

  cat("\nDatetime follow-up\n")
  cat("==================\n\n")

  cat(
    sprintf(
      "Groups investigated: %i\n\n",
      nrow(summary)
    )
  )

  cat(
    sprintf(
      "Deployment-level issues: %i\n",
      n_deployment
    )
  )

  cat(
    sprintf(
      "Image-level issues:      %i\n\n",
      n_individual
    )
  )

  cat("Affected groups:\n\n")
  tmp <- summary[,c(
    "group",
    "n_images",
    "n_images_with_error",
    "all_images_flagged"
  )]
  colnames(tmp) <- c("Groups", "Images", "Errors",
                     "Scope")
  tmp$Scope <- c("Individual", "Deployment")[
    as.numeric(tmp$Scope) + 1
  ]
  print(
    tmp,
    row.names = FALSE
  )

  cat(
    "\nThe complete summary is available in:\n",
    "  result$summary\n"
  )

  cat(
    "Image-level datetime information is available in:\n",
    "  result$datetime\n\n"
  )

  list(
    summary = summary,
    datetime = datetime
  )

  list(
    summary = summary,
    datetime = datetime
  )

}

