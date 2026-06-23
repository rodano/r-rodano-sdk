# Performs aggregate without throwing an error if input has 0 rows
# Input: - 'dep_colnames': name (string) or array of names of columns to summarize by aggregate function
#        - 'indep_colnames': name (string) or array of names of columns used as grouping variables
#        - 'df': source dataframe
#        - 'fun': summary function
#        - '...': further arguments passed to summary function 'fun'
#        - 'out_colnames': Optional. Specify different column names for summarised variables. Must be same length as dep_colnames.
safe_aggregate <- function(depColnames, indepColnames, df, fun, ..., outColnames = NULL) {
  # Handle case where input df has 0 rows: return empty dataframe with expected column names and types
  if (nrow(df) == 0) {
    df_out <- df # Copy input df
    df_out[, !(colnames(df) %in% c(depColnames, indepColnames))] <- NULL # Remove columns that are not in formula
    if (!is.null(outColnames)) colnames(df_out)[colnames(df_out) %in% depColnames] <- outColnames # Rename output column if specified

    # Generic case: call aggregate function
  } else {
    # LEFT SIDE VARIABLE(S)

    dep_list <- list()
    if (is.null(outColnames)) outColnames <- depColnames # Possibly rename output column
    dep_list[outColnames] <- df[, depColnames, drop = FALSE]

    # RIGHT SIDE VARIABLE(S)

    indep_list <- list()
    indep_list[indepColnames] <- df[, indepColnames, drop = FALSE]

    # Actually call aggregate function

    df_out <- aggregate(
      dep_list,
      indep_list,
      fun,
      ...
    )
  }

  return(df_out)
}

#' Transform dates with unknown components to complete dates
#'
#' Manages unknown components in dates and replaces them with numbers based
#' on the specified rounding strategy. The unknown pattern is matched as an
#' exact string, not a regular expression.
#'
#' @param dates Character vector of dates to be checked, with format dd.mm.yyyy.
#'   Unknown components are indicated by the \code{unknownPattern} string.
#' @param round Character. Way to round unknown days and months. One of
#'   \code{"floor"} (earliest possible date), \code{"ceil"} (latest possible
#'   date), or \code{"middle"} (mid-point date). Default: \code{"middle"}.
#' @param unknownPattern Character. String to consider as unknown part of date.
#'   Default: \code{"Unknown"}.
#' @param defaultYear Integer or Character. Year with which to replace year
#'   when year is unknown. If \code{NA} (default), an unknown year will be
#'   considered unacceptable and an error will be thrown if found.
#'
#' @return Character vector of dates free from unknown parts, in the same
#'   format as input dates (dd.mm.yyyy).
#'
#' @export
transform_unknown_dates <- function(dates, round = "middle", unknownPattern = "Unknown", defaultYear = NA) {
  if (!is.na(defaultYear)) defaultYear <- toString(defaultYear) # Allows defaultYear to be an integer
  ds <- strsplit(dates, ".", fixed = TRUE)

  datesOut <- sapply(ds, function(d) {
    if (is.na(d[1])) {
      return(NA)
    }

    if (d[3] == unknownPattern) {
      if (is.na(defaultYear)) {
        stop(sprintf("Unknown year was found (%s).", d))
      } else {
        d[3] <- defaultYear
      }
    }

    if (round == "floor") {
      if (d[2] == unknownPattern) d[2] <- "01"
      if (d[1] == unknownPattern) d[1] <- "01"
    } else if (round == "ceil") {
      if (d[2] == unknownPattern) d[2] <- "12"
      if (d[1] == unknownPattern) {
        if (d[2] == "12") {
          d[1] <- "31"
        } else {
          d[1] <- format(as.Date(sprintf("%s-%02d-01", d[3], as.integer(d[2]) + 1)) - 1, "%d")
        }
      }
    } else if (round == "middle") {
      if (d[1] == unknownPattern) {
        if (d[2] == unknownPattern) {
          d[2] <- "07"
          d[1] <- "01"
        } else {
          d[1] <- "15"
        }
      }
    }

    return(paste(d, collapse = "."))
  })

  return(datesOut)
}

#' Convert partial dates to R Date objects or formatted strings
#'
#' Converts partial dates from string format to R Date objects (default) or
#' to formatted strings with format %Y-%m-%d. Handles dates with unknown
#' components by transforming them according to the specified rounding strategy.
#'
#' @param str Character vector of dates as strings to be transformed.
#' @param format Character. Format of input dates. Default: \code{"\%d.\%m.\%Y"}.
#'   Also supports \code{"\%m.\%Y"} and \code{"\%Y"}.
#' @param unknownDate Logical. Indicates whether dates may contain unknown
#'   parts. Default: \code{FALSE}.
#' @param defaultYear Integer or Character. Year to use when year is unknown.
#'   Default: \code{NA}.
#' @param toStr Logical. If \code{TRUE}, returns dates as strings with format
#'   \%Y-\%m-\%d. If \code{FALSE} (default), returns R Date objects.
#' @param round  Character. Way to round unknown days and months. One of
#'   \code{"floor"}, \code{"ceil"}, or \code{"middle"}. Default:
#'   \code{"middle"}.
#'
#' @return Date vector of R Date objects (when \code{toStr = FALSE}) or
#'   character vector of formatted date strings (when \code{toStr = TRUE}).
#'   Returns input unchanged if length is zero.
#'
#' @export
partial_date_to_date <- function(str, format = "%d.%m.%Y", unknownDate = FALSE, defaultYear = NA, toStr = FALSE, round = "middle") {
  # If no date is imputed, return inout value itself
  if (length(str) == 0) {
    return(str)
  }

  # If dates contain unknowns, remove them
  if (unknownDate && format == "%d.%m.%Y") {
    # Change date format from d.m.Y to Y-m-d and keep as strings to keep Unknown
    str <- transform_unknown_dates(str, round = round, defaultYear = defaultYear)
  } else if (toStr && format == "%d.%m.%Y") { # ??? unknown should not exist here. Why to revert order if they're strings?
    str <- sapply(lapply(strsplit(str, "\\."), rev), paste, collapse = "-")
    # Paste change NA to string NA => need to retransform in NA
    is.na(str) <- str == "NA"
    return(str)
  }

  # Return input date as R Date object
  if (format == "%d.%m.%Y") {
    return(as.Date(str, format = format))
  } else if (format == "%m.%Y") {
    return(as.Date(paste("01.", str, sep = ""), format = "%d.%m.%Y"))
  } else if (format == "%Y") {
    return(as.Date(paste("01.01.", str, sep = ""), format = "%d.%m.%Y"))
  } else {
    stop("format not accepted")
  }
}

# Returns true where 'dates' are between 'bound0' and 'bound1'; false everywhere else
dates_in_bounds <- function(dates, bound0, bound1) {
  return(dates >= bound0 & dates <= bound1)
}

#' Ensure an output directory exists
#'
#' Creates the directory at `dirPath` when it does not already exist.
#' This helper is used by report-generation code before writing CSV or Excel
#' outputs.
#'
#' @param dirPath Character scalar. Path to the directory that should exist.
#'
#' @return Called for its side effect. Creates the directory when needed.
#'
#' @examples
#' tmp_dir <- file.path(tempdir(), "rodano-output")
#' check_and_create_path(tmp_dir)
#' dir.exists(tmp_dir)
#'
#' @export
# Checks if directory 'dirPath' exists, and creates it if not
check_and_create_path <- function(dirPath) {
  if (!file.exists(dirPath)) {
    dir.create(dirPath)
  }
}


#' Identify discrepancies between visit checks and medical event records
#'
#' Retrieves visits where medical event check responses are not in accordance
#' with reported medical events. Can detect both false negatives (check says
#' no event but records exist) and false positives (check says event occurred
#' but no records exist).
#'
#' @param reversed Logical. If \code{FALSE}, identifies visits where check is
#'   answered negatively but records are provided. If \code{TRUE}, identifies
#'   visits where check is answered positively but no records are provided.
#' @param dfChecks Data frame containing visit data with medical event checks.
#'   Must include columns for patient IDs, visit dates, and check values.
#' @param dfDetails Data frame containing medical event details. Must include
#'   columns for patient IDs and event dates.
#' @param checksPidColumn Character. Column name for patient IDs in
#'   \code{dfChecks}.
#' @param detailsPidColumn Character. Column name for patient IDs in
#'   \code{dfDetails}.
#' @param checkColumn Character. Column name for the medical event check in
#'   \code{dfChecks}.
#' @param checkValue Value indicating the check response of interest (e.g.,
#'   "No" for \code{reversed = FALSE} or "Yes" for \code{reversed = TRUE}).
#' @param eventDateColumn Character. Column name for medical event dates in
#'   \code{dfDetails}.
#' @param upperBoundColumn Character. Column name for visit dates in
#'   \code{dfChecks} (upper temporal bound for event matching).
#' @param lowerBoundColumn Character or \code{NA}. Column name for previous
#'   visit dates in \code{dfChecks} (lower temporal bound). If \code{NA}
#'   (default), no lower bound is applied.
#'
#' @return A data frame containing the original check information merged with
#'   a \code{num_of_events} column showing the count of matching medical events
#'   within the temporal window. Only rows with discrepancies are returned.
#'
#' @export
get_check_details_discrepancy <- function(reversed, dfChecks, dfDetails, checksPidColumn, detailsPidColumn, checkColumn, checkValue, eventDateColumn, upperBoundColumn, lowerBoundColumn = NA) {
  # Selects only visits were medical event check was answered negatively
  dfChecks <- dfChecks[!is.na(dfChecks[, checkColumn]) & dfChecks[, checkColumn] == checkValue, ]

  # Merge visits with any associated patient's medical event
  merged <- merge(
    x = dfChecks,
    y = dfDetails,
    by.x = checksPidColumn,
    by.y = detailsPidColumn,
    all.x = TRUE
  )

  # Compute what merged events are within time window
  eventsInRange <- !is.na(merged[, eventDateColumn]) &
    (merged[, eventDateColumn] <= merged[, upperBoundColumn]) # &
  # ifelse(is.na(lowerBoundColumn),
  #        TRUE,
  #        merged[, eventDateColumn] > merged[, lowerBoundColumn])
  if (!is.na(lowerBoundColumn)) eventsInRange <- eventsInRange & merged[, eventDateColumn] > merged[, lowerBoundColumn]

  # Invalidate matches where events are outside of expected range
  merged[!eventsInRange, eventDateColumn] <- NA

  # Count number of medical events wrongfully assigned to patients-events pairs
  aggregated <- safe_aggregate(eventDateColumn, c(checksPidColumn, upperBoundColumn), merged, function(edates) sum(!is.na(edates)), outColnames = "num_of_events")

  # Filter out problematic observations
  if (reversed) {
    aggregated <- aggregated[aggregated$num_of_events == 0, ]
  } else {
    aggregated <- aggregated[aggregated$num_of_events > 0, ]
  }

  # Retrieve original information along with newly computed number of medical events
  merge(x = dfChecks, y = aggregated, by = c(checksPidColumn, upperBoundColumn))
}


#' Construct output filename for a report
#'
#' Builds a standardized filename for report output, composed of project name,
#' document name, date, and extension. The file path includes the output
#' directory.
#'
#' @param projectName Character. Name of the project.
#' @param documentName Character. Name of the report document.
#' @param extension Character. File format extension (without dot).
#' @param outDir Character. Directory path where the report will be saved.
#' @param outDate Date. Date the output was computed. Default: \code{Sys.Date()}
#'   (current date).
#'
#' @return Character. Complete file path including directory, composed of
#'   \code{projectName_documentName_date.extension}.
#'
#' @export
build_output_filename <- function(projectName, documentName, extension, outDir, outDate = Sys.Date()) {
  name <- paste(projectName, documentName, format(outDate, format = "%Y-%m-%d"), sep = "_")
  name <- paste(sprintf("%s/%s", outDir, name), extension, sep = ".")
  return(name)
}


#' Retrieve event closest to target date within optional offset
#'
#' Retrieves the event with a date closest to the specified target date.
#' Optionally filters events to those within a specified time offset around
#' the target date before selecting the closest match.
#'
#' @param df Data frame containing the events (one event per row).
#' @param dateCol Character. Column name where event dates are stored (as
#'   strings).
#' @param targetDate Date. The ideal date being looked for.
#' @param offset Integer or numeric vector. Optional allowed number of days
#'   around \code{targetDate} for event retrieval. If a vector of length 2,
#'   the first element is used for the lower bound and the second for the
#'   upper bound. Default: \code{NA} (no filtering).
#'
#' @return A data frame row (i.e., a one-row data frame) representing the
#'   event best matching the requested temporal criteria. Returns a zero-row
#'   data frame if no event satisfies the constraint.
#'
#' @export
get_closest_event <- function(df, dateCol, targetDate, offset = NA) {
  # If there is no possible event, return empty dataframe
  if (nrow(df) == 0) {
    return(df)
  }

  # Dates must be transformed before comparison
  df[, "DATE_TMP"] <- partial_date_to_date(df[, dateCol])

  # Filter temporal non-matching events, if needed
  if (all(!is.na(offset))) {
    if (length(offset) == 1) {
      df <- df[dates_in_bounds(df[, "DATE_TMP"], targetDate - offset, targetDate + offset), ]
    } else {
      df <- df[dates_in_bounds(df[, "DATE_TMP"], targetDate - offset[1], targetDate + offset[2]), ]
    }
  }

  # Visit with closest date
  closest <- df[which.min(abs(df[, "DATE_TMP"] - targetDate)), ]

  # Clear temporary date column
  closest$DATE_TMP <- NULL

  return(closest)
}

# Allows to comapre even if there are NAs
# a and b are elements, vectors or data.frames
# Returns true where a=b, a=NA and b=NA
compna <- function(a, b) {
  ((a == b) &
     (is.na(a) | !is.na(b)) &
     (is.na(b) | !is.na(a))) |
    (is.na(a) & is.na(b))
}

#' Retrieve center assignment for patients at specified dates
#'
#' Retrieves the center (parent scope) of patients at specified dates based
#' on a transfer list and current center assignments. Handles patient transfers
#' by matching dates against transfer periods. Returns \code{NA} if no date
#' matches are found.
#'
#' @param trans Data frame. Transfer list containing patient transfer records
#'   with start and stop dates.
#' @param pat Character vector. Patient identifiers.
#' @param dat Character or Date vector. Dates at which to determine center
#'   assignments.
#' @param cIn Character vector. Current center assignments for patients.
#' @param cColname Character. Column name for center identifiers in the
#'   transfer list. Default: \code{"Center"}.
#'
#' @return Character vector of center codes for each patient at the specified
#'   dates. May return \code{NA} if no date matches are found.
#'
#' @export
get_center_on_date <- function(trans, pat, dat, cIn, cColname = "Center") {
  # Retrieve current center
  cOut <- cIn

  # Remove time from start and end dates
  # trans$'Start date' <- get_subfield_values(strsplit(trans$'Start date', ' '), 1)
  # trans$'Stop date' <- get_subfield_values(strsplit(trans$'Stop date', ' '), 1)

  # Get transfered patients
  t <- pat %in% trans$Patient

  cOut[t] <- mapply(
    function(p, d) {
      trans_ <- trans[trans$Patient == p, ] # Get patient's rows

      if (is.na(d)) {
        # If date is na, get first center - or last?
        c_ <- trans_[1, cColname]
      } else {
        d <- transform_unknown_dates(d, round = "floor") # Format
        trans_$Matches <- (is.na(trans_$"Stop date") & (as.Date(d, "%d.%m.%Y") >= as.Date(trans_$"Start date"))) |
          ((as.Date(d, "%d.%m.%Y") <= as.Date(trans_$"Stop date")) & (as.Date(d, "%d.%m.%Y") >= as.Date(trans_$"Start date")))
        # TODO If none found: manageUnknown(d, 'ceil') and re-do the check
        c_ <- trans_[trans_$Matches, cColname][1] # Take first matching center (in case EDSS performed the day of transfer)
      }

      return(c_) # Return just the center code
    },
    pat[t],
    dat[t]
  )

  return(cOut)
}

# Moves list elements "elToMove" within list "lIn", after reference element "elRef".
# "elToMove" must be given in desired order.
# If "insertLeft", "elToMove" will be placed before "elRef".
# Returns modified list "lOut"
move_elements <- function(lIn, elToMove, elRef, insertLeft = FALSE) {
  elIdx <- sapply(lIn, function(x) which(x == lIn)) # Index of each element of the list

  if (insertLeft) {
    elPre <- lIn[which(elIdx[elRef] > elIdx)] # Elements present before reference element
    elPost <- lIn[which(elIdx[elRef] <= elIdx)] # Elements present after reference element, including it
  } else {
    elPre <- lIn[which(elIdx[elRef] >= elIdx)] # Elements present before reference element, including it
    elPost <- lIn[which(elIdx[elRef] < elIdx)] # Elements present after reference element
  }

  if (any(elPre %in% elToMove)) elPre <- elPre[-which(elPre %in% elToMove)] # Elements to insert before elements to move
  if (any(elPost %in% elToMove)) elPost <- elPost[-which(elPost %in% elToMove)] # Elements to insert after elements to move

  lOut <- c(elPre, elToMove, elPost) # Re-ordered list

  return(lOut)
}

# Reverts order of rows in df
revert_df <- function(df) {
  return(df[seq(dim(df)[1], 1), ])
}

# TODO allow to specify depth of subfield
# Returns value contained in nested list
# o: object, sf: subfield
# Does not manage when subfield is not found in structure nor when subfield is in a deeper level.
get_subfield_values <- function(o, sf) {
  sapply(o, function(x) x[sf][[1]])
}

#' Circular shift of vector elements
#'
#' Rotates the elements of a vector to the right by \code{i} positions.
#' Elements that fall off the end wrap around to the beginning.
#'
#' @param x A vector to shift.
#' @param i Integer number of positions to shift to the right (default: 1).
#'   Negative values shift to the left. Values larger than \code{length(x)}
#'   are taken modulo the vector length.
#'
#' @return A vector of the same length as \code{x} with elements rotated.
#'
#' @examples
#' shift(1:5) # c(5, 1, 2, 3, 4)
#' shift(1:5, 2) # c(4, 5, 1, 2, 3)
#' shift(1:5, -1) # c(2, 3, 4, 5, 1)
#'
#' @export
shift <- function(x, i = 1) {
  n <- length(x)
  if (n == 0) {
    return(x)
  }
  i <- i %% n
  if (i == 0) {
    return(x)
  }
  return(x[c((n - i + 1):n, 1:(n - i))])
}

#' Write check results to CSV with change tracking
#'
#' Writes a dataframe of check results to a CSV file, assigning each row a
#' unique MD5-based identifier. If a previous report exists for the same check,
#' the function carries forward \code{resolved} and \code{comment} values from
#' matching rows, enabling incremental review of findings across report runs.
#'
#' @param projectName Character. Name of the project.
#' @param tableResult Data frame of check results to export.
#' @param checkId Character. Identifier for the check (used in filename and
#'   as a column value).
#' @param previousReportFolder List with elements \code{folder_path} and
#'   \code{date}, as returned by \code{\link{get_previous_report_folder}}.
#' @param outDate Date used in the output filename.
#' @param outputPath Character. Directory where the CSV will be written.
#'
#' @return The enriched \code{tableResult} data frame (invisibly \code{NULL}
#'   when \code{tableResult} has zero rows).
#'
#' @export
print_table_to_csv <- function(projectName, tableResult, checkId, previousReportFolder, outDate = Sys.Date(), outputPath) {
  if (nrow(tableResult) > 0) {
    tableResult$check_id <- checkId # include md5 for check id
    tableResult$id <- openssl::md5(apply(tableResult, 1, paste, collapse = "")) # id unique for each entry of each check
    # TODO: change ID system so that there will be a match if the columns are variable... maybe also filter results on update time...

    tableResult <- tableResult[, c(ncol(tableResult), 1:(ncol(tableResult) - 2))] # Place row id at the begining and remove check id
    tableResult <- tableResult[order(tableResult[, 2], tableResult[, 1]), ] # Order first by patient code then by row id

    # Compare with previous result, if any
    tableResult$resolved <- rep(FALSE, nrow(tableResult)) # Suppose all findings are new, for now
    tableResult$comment <- rep("", nrow(tableResult)) # Set empty comments, for now

    if (!is.null(previousReportFolder)) {
      filename_previous <- build_output_filename(projectName, checkId, "csv", previousReportFolder$folder_path, previousReportFolder$date) # Get path to previous results
      if (file.exists(filename_previous)) { # Check if there was any prior findings file
        tableResultPrevious <- read.csv(filename_previous)
        # Was this finding resolved in the past already?
        # Where result is found in previous result, retrieve its previous 'resolved' value.
        # (Note: sort = FALSE ensures output of merge has same order as dataframe 'x' (i.e. tableresult) so that replacement will be made in the correct rows)
        tableResult[
          tableResult$id %in% tableResultPrevious$id,
          c("resolved", "comment")
        ] <- merge(
          x = tableResult,
          y = tableResultPrevious,
          all = FALSE,
          by = "id",
          sort = FALSE
        )[, c("resolved.y", "comment.y")]
      }
    }

    filename <- build_output_filename(projectName = projectName, documentName = checkId, extension = "csv", outDate = outDate, outDir = outputPath)

    write.csv(x = tableResult, file = filename, row.names = FALSE, na = "") # Print result as csv table
    return(tableResult)
  }
}


#' Run all checks and produce a findings report with summary
#'
#' Iterates over a list of check results, writes each one to a CSV via
#' \code{\link{print_table_to_csv}}, and produces a summary CSV comparing
#' current observation counts with the previous report run. The output folder
#' is created automatically under \code{outDir} and named
#' \code{<outDate>_<reportName>}.
#'
#' @param projectName Character. Name of the project.
#' @param reportName Character. Report identifier (e.g.
#'   \code{"data_validation"}).
#' @param results A list of check results. Each element must be a list with
#'   \code{table_result} (data frame), \code{check_id} (character), and
#'   \code{description} (character).
#' @param outDir Character. Parent directory where the dated report folder
#'   will be created.
#' @param outDate Date for the report (default: Sys.Date()). Used in folder and
#'   file names, and to locate the previous report for comparison.
#'
#' @return Called for its side effects: writes per-check CSV files and a
#'   summary CSV into the report folder.
#'
#' @seealso \code{\link{print_table_to_csv}},
#'   \code{\link{get_previous_report_folder}},
#'   \code{\link{build_output_filename}}
#'
#' @export
report_findings <- function(projectName, reportName, results, outDir, outDate = Sys.Date()) {
  # build (and create) output folder
  outputPath <- sprintf("%s/%s", outDir, paste(outDate, reportName, sep = "_"))
  check_and_create_path(outputPath)

  # get previous reports
  previousReportFolder <- get_previous_report_folder(projectName, reportName, outDate = outDate, outDir = outDir)
  # iterate over list of check results
  summaryResults <- do.call(
    rbind.data.frame,
    lapply(results, function(result) {
      # write findings
      tmpDf <- print_table_to_csv(
        projectName = projectName,
        tableResult = result[["table_result"]],
        checkId = result[["check_id"]],
        previousReportFolder = previousReportFolder,
        outDate = outDate,
        outputPath = outputPath
      )
      # build summary df
      return(list(
        check_id = result[["check_id"]], description = result[["description"]],
        observations = ifelse(is.null(tmpDf), 0, nrow(tmpDf)),
        unresolved_issues = ifelse(is.null(tmpDf), 0, nrow(tmpDf[!as.logical(tmpDf$resolved), ]))
      ))
    })
  )
  # compare with previous summary, if any
  previousReportFolder <- get_previous_report_folder(projectName, reportName, outDate = outDate, outDir = outDir)

  if (!is.null(previousReportFolder)) {
    latestSummary <- build_output_filename(
      projectName = projectName,
      outDir = previousReportFolder$folder_path,
      documentName = sprintf("%s_summary", reportName),
      extension = "csv",
      outDate = previousReportFolder$date
    )

    if (file.exists(latestSummary)) {
      # read previous summary
      previousSummary <- read.csv(latestSummary, stringsAsFactors = FALSE)
      # merge with current summary
      summaryResults <- merge(
        x = summaryResults,
        y = previousSummary[
          , # do not want column description
          c("check_id", "observations", "unresolved_issues")
        ],
        by = "check_id",
        all.x = TRUE,
        suffixes = c("", paste("_", previousReportFolder$date))
      )
      # compute changes
      summaryResults$observations_change <- summaryResults$observations - summaryResults[, paste("observations_", previousReportFolder$date)]
      summaryResults$unresolved_issues_change <- summaryResults$unresolved_issues - summaryResults[, paste("unresolved_issues_", previousReportFolder$date)]
    } else {
      # no previous summary, set changes to 0
      summaryResults$observations_change <- 0
      summaryResults$unresolved_issues_change <- 0
    }
  } else {
    # no previous report folder, set changes to 0
    summaryResults$observations_change <- 0
    summaryResults$unresolved_issues_change <- 0
  }

  # write summary results
  write.table(
    x = summaryResults,
    file = build_output_filename(
      projectName = projectName,
      outDir = outputPath,
      documentName = sprintf("%s_summary", reportName),
      extension = "csv",
      outDate = outDate
    ),
    sep = ",",
    row.names = FALSE
  )
}

#' Retrieve path to most recent previous report folder
#'
#' Retrieves the path of the latest report folder matching the project name
#' and report type. Only considers folders created before the specified output
#' date. Folder names must contain a date in format YYYY-MM-DD.
#'
#' @param projectName Character. Name of the project.
#' @param reportName Character. Type of report (e.g., \code{"data_validation"},
#'   \code{"data_listing"}).
#' @param outDir Character. Parent directory containing report folders.
#' @param outDate Date. Date of current report. Only folders created before
#'   this date will be considered. Default: \code{Sys.Date()}.
#'
#' @return List with three elements:
#'   \itemize{
#'     \item \code{folder_path}: Character path to the previous report folder
#'     \item \code{files_path}: Character vector of full file paths in that folder
#'     \item \code{date}: Character date extracted from the folder name (YYYY-MM-DD)
#'   }
#'   Returns \code{NULL} if no previous report folder is found.
#'
#' @keywords internal
#' @noRd
get_previous_report_folder <- function(projectName, reportName, outDir, outDate = Sys.Date()) {
  # pattern_file <- sprintf("%s_%s_summary.*\\.csv", projectName, documentName)
  files <- list.dirs(outDir, full.names = TRUE, recursive = FALSE)
  date_pattern <- "\\d{4}-\\d{2}-\\d{2}"

  # Extract date from each folder name using the pattern
  folder_dates <- sapply(basename(files), function(x) {
    m <- regmatches(x, regexpr(date_pattern, x))
    if (length(m) == 0) {
      return(NA)
    }
    return(m)
  })

  # Convert to Date and filter by report_name and before out_date
  valid_idx <- !is.na(folder_dates) & grepl(reportName, basename(files)) & as.Date(folder_dates) < as.Date(outDate)
  if (!any(valid_idx)) {
    return(NULL)
  }

  # Find the most recent folder by date
  latest_idx <- which.max(as.Date(folder_dates[valid_idx]))
  previous_folder_path <- files[valid_idx][latest_idx]
  extracted_date <- folder_dates[valid_idx][latest_idx]


  return(list(
    folder_path = previous_folder_path,
    files_path = list.files(previous_folder_path, full.names = TRUE, recursive = FALSE),
    date = extracted_date
  ))
}

#' Check if two time periods overlap
#'
#' Checks if two time periods overlap given their start and end dates.
#' Assumes \code{startP1 <= startP2}. Periods with \code{NA} as end date
#' are considered ongoing (infinite end).
#'
#' @param startP1 Date. Start date of the first period.
#' @param endP1 Date or \code{NA}. End date of the first period. If \code{NA},
#'   the period is considered ongoing.
#' @param startP2 Date. Start date of the second period.
#' @param endP2 Date or \code{NA}. End date of the second period. If \code{NA},
#'   the period is considered ongoing.
#'
#' @return Logical. \code{TRUE} if periods overlap, \code{FALSE} otherwise.
#'
#' @export
periods_overlap <- function(startP1, endP1, startP2, endP2) {
  (startP1 <= startP2) &
    (is.na(endP1) | endP1 >= startP2)
}


#' Add parent scope (center) column to child scope (patient) data frame
#'
#' Adds a parent scope (center) column to a data frame of child scopes
#' (patients). Extracts parent codes from child codes using a regex pattern,
#' and updates assignments based on active transfers (those with no stop date).
#'
#' @param transfers Data frame. Transfer records containing scope and parent
#'   scope assignments.
#' @param scopeModelIdTrans Character. Column name for child scope identifiers
#'   in the transfers data frame. Default: \code{"Patient"}.
#' @param parentScopeModelIdTrans Character. Column name for parent scope
#'   identifiers in the transfers data frame. Default: \code{"Center"}.
#' @param df Data frame. Data frame to which the parent scope column will be
#'   added.
#' @param scopeModelIdDf Character. Column name for child scope identifiers
#'   in \code{df}. Default: \code{"PATIENT"}.
#' @param parentScopeModelIdDf Character. Column name for parent scope
#'   identifiers to be added to \code{df}. Default: \code{"CENTER"}.
#' @param parentCodePattern Character. Regex pattern to extract parent code
#'   from child code. Default: \code{"^...."} (first 4 characters).
#'
#' @return Data frame with added parent scope column as the first column,
#'   followed by all original columns.
#'
#' @export
add_parent_scope <- function(transfers,
                             scopeModelIdTrans = "Patient",
                             parentScopeModelIdTrans = "Center",
                             df,
                             scopeModelIdDf = "PATIENT",
                             parentScopeModelIdDf = "CENTER",
                             parentCodePattern = "^....") {
  # Extract parent code from child code
  parent_substr <- rep(NA_character_, nrow(df))
  not_na_idx <- !is.na(df[[scopeModelIdDf]])
  parent_substr[not_na_idx] <- regmatches(
    df[[scopeModelIdDf]][not_na_idx],
    regexpr(pattern = parentCodePattern, df[[scopeModelIdDf]][not_na_idx], perl = TRUE)
  )
  df[[parentScopeModelIdDf]] <- parent_substr
  transfers <- transfers[is.na(transfers[["Stop date"]]), ]
  df[[parentScopeModelIdDf]] <- ifelse(df[[scopeModelIdDf]] %in% transfers[[scopeModelIdTrans]],
    transfers[[parentScopeModelIdTrans]][match(df[[scopeModelIdDf]], transfers[[scopeModelIdTrans]])],
    df[[parentScopeModelIdDf]]
  )
  # Move last  into first position
  df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
  return(df)
}

#' Compare metrics report with previous Excel report
#'
#' Reports findings to an Excel workbook with each check as a separate sheet.
#' Reads a previous Excel report to compare with the current report and
#' calculates differences in counts. Returns results with difference columns
#' when previous report exists.
#'
#' @param projectName Character. Name of the project.
#' @param reportName Character. Report identifier.
#' @param results List. Named list of data frames, each representing metrics
#'   for a specific check or sheet.
#' @param outDir Character. Directory containing previous reports.
#' @param outDate Date. Date for the current report. Default: \code{Sys.Date()}.
#' @param datePreviousReport Character or Date. Date of the previous report
#'   to compare against. If \code{NULL} or file doesn't exist, returns results
#'   unchanged.
#'
#' @return List of data frames with the same structure as \code{results}.
#'   When a previous report exists and contains an \code{N} column, adds a
#'   \code{"Difference since <date>"} column showing count changes. Returns
#'   original results if no previous report exists.
#'
#' @export
report_metrics_diff_excel <- function(projectName, reportName, results, outDir, outDate = Sys.Date(), datePreviousReport = NULL) {
  # read the previous report
  previous_report <- build_output_filename(
    projectName = projectName,
    outDir = outDir,
    documentName = reportName,
    extension = "xlsx",
    outDate = datePreviousReport
  )
  # try to read the previous report, if it doesn't exists return results as is
  if (!file.exists(previous_report)) {
    return(results)
  }

  # go through the sheets and compare with previous report
  out <- lapply(names(results), function(result_name) {
    # name of the element in list results
    df <- results[[result_name]]
    if (file.exists(previous_report)) {
      df_prev <- readxl::read_xlsx(path = previous_report, sheet = result_name)
    } else {
      df_prev <- data.frame()
    }
    if (!is.null(df_prev) && nrow(df_prev) > 0) {
      # compare resolved status
      # create unique id for each row
      # remove the column with name N if exists
      if ("N" %in% colnames(df)) {
        # dftmp <- as.data.frame(df[, !colnames(df) %in% "N"])
        # df_prevtmp <- as.data.frame(df[, !colnames(df) %in% "N"])
        if (ncol(df) < 3) {
          df[, "dummy"] <- 1
        }
        if (ncol(df_prev) < 3) {
          df_prev[, "dummy"] <- 1
        }

        df$id <- openssl::md5(apply(df[, !colnames(df) %in% "N"], 1, paste, collapse = ""))
        df_prev$id <- openssl::md5(apply(df_prev[, !colnames(df_prev) %in% "N"], 1, paste, collapse = ""))
        merged_df <- merge(
          x = df,
          y = df_prev,
          all = TRUE,
          by = c("id"),
          sort = FALSE,
          suffixes = c("", ".prev")
        )

        # Calculate the difference in counts
        merged_df$diff <- ifelse(is.na(merged_df$N.prev), merged_df$N, merged_df$N - merged_df$N.prev)

        merged_df[, "id"] <- NULL
        merged_df[, "dummy"] <- NULL
        # Remove any previous .prev columns
        merged_df <- merged_df[, !grepl("\\.prev$", colnames(merged_df))]

        # set the name of the diff column
        colnames(merged_df)[colnames(merged_df) == "diff"] <- paste("Difference since", datePreviousReport)
        return(merged_df)
      } else {
        return(df)
      }
    } else {}
  })
  # set the names of the output list
  names(out) <- names(results)
  return(out)
}
