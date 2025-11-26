now <- Sys.Date()


# Performs aggregate without throwing an error if input has 0 rows
# Input: - 'dep_colnames': name (string) or array of names of columns to summarize by aggregate function
#        - 'indep_colnames': name (string) or array of names of columns used as grouping variables
#        - 'df': source dataframe
#        - 'fun': summary function
#        - '...': further arguments passed to summary function 'fun'
#        - 'out_colnames': Optional. Specify different column names for summarised variables. Must be same length as dep_colnames.
safe_aggregate <- function(dep_colnames, indep_colnames, df, fun, ..., out_colnames = NULL) {
  # Handle case where input df has 0 rows: return empty dataframe with expected column names and types
  if (nrow(df) == 0) {
    df_out <- df # Copy input df
    df_out[, !(colnames(df) %in% c(dep_colnames, indep_colnames))] <- NULL # Remove columns that are not in formula
    if (!is.null(out_colnames)) colnames(df_out)[colnames(df_out) %in% dep_colnames] <- out_colnames # Rename output column if specified

    # Generic case: call aggregate function
  } else {
    # LEFT SIDE VARIABLE(S)

    dep_list <- list()
    if (is.null(out_colnames)) out_colnames <- dep_colnames # Possibly rename output column
    dep_list[out_colnames] <- df[, dep_colnames, drop = FALSE]

    # RIGHT SIDE VARIABLE(S)

    indep_list <- list()
    indep_list[indep_colnames] <- df[, indep_colnames, drop = FALSE]

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

# Manages unknown components in date and replaces them with numbers
# Input: - 'dates': (list of) date(s) to be checked, of type string and with format dd-mm-yyyy
#        - 'round': way to round the unknown days and months. One of 'floor', 'ceil' or 'middle'
#        - 'unkownPattern': string to consider as unknown part of date
#        - 'defaultYear': year with which to replace year when year is unknown.
#                         If value is NA (default), an unknown year will be considered unacceptable and an error will be thrown if found
# Ouput: (List of) date(s), free from Unknown parts, in the same format as input (list of) date(s)
# !!! Unknown pattern is not a pattern anymore but the exact string.
transform_unknown_dates <- function(dates, round = "middle", unkownPattern = "Unknown", defaultYear = NA) {
  if (!is.na(defaultYear)) defaultYear <- toString(defaultYear) # Allows defaultYear to be an integer
  ds <- strsplit(dates, ".", fixed = TRUE)

  datesOut <- sapply(ds, function(d) {
    if (is.na(d[1])) {
      return(NA)
    }

    if (d[3] == unkownPattern) {
      if (is.na(defaultYear)) {
        stop(sprintf("Unkown year was found (%s).", d))
      } else {
        d[3] <- defaultYear
      }
    }

    if (round == "floor") {
      if (d[2] == unkownPattern) d[2] <- "01"
      if (d[1] == unkownPattern) d[1] <- "01"
    } else if (round == "ceil") {
      if (d[2] == unkownPattern) d[2] <- "12"
      if (d[1] == unkownPattern) d[1] <- format(as.Date(sprintf("%s-%i-01", d[3], as.integer(d[2]) + 1)) - 1, "%d") # Last day of given month
    } else if (round == "middle") {
      if (d[1] == unkownPattern) {
        if (d[2] == unkownPattern) {
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

# Converts partial dates from KV to R Date object (if toStr = F as per default) or to string with format %Y-%m-%d
# Input: - 'str': (list of) date(s) as strings, to be transformed to dates
#        - 'format': format of input dates 'str'
#        - 'unknownDate': boolean, indicates whether dates may contain unknwn parts or not
#        - 'toStr': boolean, indicates whether transformed dates should be as string (T) or not (F)
#        - 'round': way to round the unknown days and months. One of 'floor', 'ceil' or 'middle'
partial_date_to_date <- function(str, format = "%d.%m.%Y", unknownDate = FALSE, defaultYear = NA, toStr = FALSE, round = "middle") {
  # If no date is imputed, return inout value itself
  if (length(str) == 0) {
    return(str)
  }

  # If dates contain unknowns, remove them
  if (unknownDate & format == "%d.%m.%Y") {
    str <- transform_unknown_dates(str, round = round, defaultYear = defaultYear)
  } # Change date format from d.m.Y to Y-m-d and keep as strings to keep Unkonwn
  else if (toStr & format == "%d.%m.%Y") { # ??? unknown should not exist here. Why to revert order if they're strings?
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

# Checks if directory 'dir_path' exists, and creates it if not
check_and_create_path <- function(dir_path) {
  if (!file.exists(dir_path)) {
    dir.create(dir_path)
  }
}

# Constructs file name for report output
# Input: - 'projectName': name of the project
#        - 'report_type': type of report (e.g. 'data_validation', 'data_listing')
#        - 'documentName': name of the report
#        - 'extension': format of the output file
#        - 'outDir': path where the report will be saved
#        - (optional) 'outDate': date the output was computed. If NULL, "now" is used
# Output: Complete file name including path, composed of projectname, report_type, documentName, today's date and extension
build_output_filename <- function(projectName, report_type, documentName, extension, outDir = dir.output, outDate = now) {
  name <- paste(projectName, documentName, format(outDate, format = "%Y-%m-%d"), sep = "_")
  name <- paste(sprintf("%s/%s", outDir, name), extension, sep = ".")
  return(name)
}


# Retrieves event closest to target date 'targetDate' (type: Date).
# Selects only event between [targetDate - offset; targetDate + offset] if specified.
# Inuput: dataframe 'df' containing the events on each row, which dates are stored under column 'dateCol' (type: String);
# 'targetDate' the ideal date being looked for; 'offset' (optional) the allowed number of days around 'targetDate' for event retrieval.
# If 'offset' is a vector, first element will be used to compute lower bound and second element for upper bound.
# Output: a dataframe row (i.e.: a list) representing the event best matching requested temporal criteria;
# a 0-rows dataframe if no event satisfies constraint
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

# TODO if cIn is null, extract first part of the patient code
# Retrieves center of a patients 'pat' at dates 'dat'
# based on transfer list 'trans' and current centers 'cIn'
# May return NA if no date matches!
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
        c_ <- trans_[1, cColname]
      } # If date is na, get first center - or last?
      else {
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

print_table_to_csv <- function(project_name, report_type, tableresult, check_id, previous_report_folder, out_date = out_date, output_path = output_path) {
  if (nrow(tableresult) > 0) {
    tableresult$check_id <- check_id # include md5 for check id
    tableresult$id <- md5(apply(tableresult, 1, paste, collapse = "")) # id unique for each entry of each check
    # TODO: change ID system so that there will be a match if the columns are variable... maybe also filter results on update time...

    tableresult <- tableresult[, c(ncol(tableresult), 1:(ncol(tableresult) - 2))] # Place row id at the begining and remove check id
    tableresult <- tableresult[order(tableresult[, 2], tableresult[, 1]), ] # Order first by patient code then by row id

    # Compare with previous result, if any
    tableresult$resolved <- rep(FALSE, nrow(tableresult)) # Suppose all findings are new, for now
    tableresult$comment <- rep("", nrow(tableresult)) # Set empty comments, for now

    filename_previous <- build_output_filename(project_name, report_type, check_id, "csv", previous_report_folder$folder_path, previous_report_folder$date) # Get path to previous results
    if (file.exists(filename_previous)) { # Check if there was any prior findings file
      tableresult_previous <- read.csv(filename_previous)
      # Was this finding resolved in the past already?
      # Where result is found in previous result, retrieve its previous 'resolved' value.
      # (Note: sort = FALSE ensures output of merge has same order as dataframe 'x' (i.e. tableresult) so that replacement will be made in the correct rows)
      tableresult[
        tableresult$id %in% tableresult_previous$id,
        c("resolved", "comment")
      ] <- merge(
        x = tableresult,
        y = tableresult_previous,
        all = FALSE,
        by = "id",
        sort = FALSE
      )[, c("resolved.y", "comment.y")]
    }

    filename <- build_output_filename(projectName = project_name, report_type = report_type, documentName = check_id, extension = "csv", outDate = out_date, outDir = output_path)

    write.csv(x = tableresult, file = filename, row.names = FALSE, na = "") # Print result as csv table
    return(tableresult)
  }
}


report_findings <- function(project_name, report_name, results, out_date = today) {
  # build (and create) output folder
  output_path <- sprintf("%s/%s", dir.output, paste(out_date, report_name, sep = "_"))
  check_and_create_path(output_path)

  # get previous reports
  previous_report_folder <- get_previous_report_folder(project_name, report_name)

  # iterate over list of check results
  summary_results <- do.call(
    rbind.data.frame,
    lapply(results, function(result) {
      # write findings
      tmpdf <- print_table_to_csv(
        project_name = project_name,
        tableresult = result[["table_result"]],
        check_id = result[["check_id"]],
        previous_report_folder = previous_report_folder,
        out_date = out_date,
        output_path = output_path
      )
      # build summary df
      return(list(
        check_id = result[["check_id"]], description = result[["description"]],
        observations = ifelse(is.null(tmpdf), 0, nrow(tmpdf)),
        unresolved_issues = ifelse(is.null(tmpdf), 0, nrow(tmpdf[!as.logical(tmpdf$resolved), ]))
      ))
    })
  )
  # compare with previous summary, if any
  previous_report_folder <- get_previous_report_folder(project_name, report_name)
  latest_summary <- build_output_filename(
    projectName = project_name,
    report_type = report_name,
    outDir = previous_report_folder$folder_path,
    documentName = sprintf("%s_summary", report_name),
    extension = "csv",
    outDate = previous_report_folder$date
  )

  if (
    file.exists(latest_summary)
  ) {
    # read previous summary
    previous_summary <- read.csv(latest_summary, stringsAsFactors = FALSE)
    # merge with current summary
    summary_results <- merge(
      x = summary_results,
      y = previous_summary[
        , # do not want column description
        c("check_id", "observations", "unresolved_issues")
      ],
      by = "check_id",
      all.x = TRUE,
      suffixes = c("", paste("_", previous_report_folder$date))
    )
    # compute changes
    summary_results$observations_change <- summary_results$observations - summary_results[, paste("observations_", previous_report_folder$date)]
    summary_results$unresolved_issues_change <- summary_results$unresolved_issues - summary_results[, paste("unresolved_issues_", previous_report_folder$date)]
  } else {
    # no previous summary, set changes to 0
    summary_results$observations_change <- 0
    summary_results$unresolved_issues_change <- 0
  }

  # write summary results
  write.table(
    x = summary_results,
    file = build_output_filename(
      projectName = project_name,
      report_type = report_name,
      outDir = output_path,
      documentName = sprintf("%s_summary", report_name),
      extension = "csv",
      outDate = out_date
    ),
    sep = ",",
    row.names = FALSE
  )
}

# Retrieves the path of the latest report folder given project name and report_type
# Input: - 'projectName': name of the project
#        - 'report_name': type of report (e.g. 'data_validation', 'data_listing')
#        - 'out_date': date of current report. Only folders created before this date will be considered.
get_previous_report_folder <- function(projectname, report_name, out_date = today) {
  # pattern_file <- sprintf("%s_%s_summary.*\\.csv", projectname, document_name)
  files <- list.dirs(dir.output, full.names = TRUE, recursive = FALSE)
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
  valid_idx <- !is.na(folder_dates) & grepl(report_name, basename(files)) & as.Date(folder_dates) < as.Date(out_date)
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

# Checks if two periods p1 and o2 overlap given their start dates start_p1 and start_p2
# and end date end_p2, with start_p1 <= start_p2.
# If an end date is NA, the period is considered ongoing.
periods_overlap <- function(start_p1, end_p1, start_p2, end_p2) {
  (start_p1 <= start_p2) &
    (is.na(end_p1) | end_p1 >= start_p2)
}


# Add a parent scope (center) column to a dataframe of child scope (patients)
# Input: - 'df': dataframe to which the parent scope column will be added
add_parent_scope <- function(transfers,
                             scope_model_id_trans = "Patient",
                             parent_scope_model_id_trans = "Center",
                             df,
                             scope_model_id_df = "PATIENT",
                             parent_scope_model_id_df = "CENTER",
                             parent_code_pattern = "^....") {
  # Extract parent code from child code
  parent_substr <- rep(NA_character_, nrow(df))
  not_na_idx <- !is.na(df[[scope_model_id_df]])
  parent_substr[not_na_idx] <- regmatches(
    df[[scope_model_id_df]][not_na_idx],
    regexpr(pattern = parent_code_pattern, df[[scope_model_id_df]][not_na_idx], perl = TRUE)
  )
  df[[parent_scope_model_id_df]] <- parent_substr
  transfers <- transfers[is.na(transfers[["Stop date"]]), ]
  df[[parent_scope_model_id_df]] <- ifelse(df[[scope_model_id_df]] %in% transfers[[scope_model_id_trans]],
    transfers[[parent_scope_model_id_trans]][match(df[[scope_model_id_df]], transfers[[scope_model_id_trans]])],
    df[[parent_scope_model_id_df]]
  )
  # Move last  into first position
  df <- df[, c(ncol(df), 1:(ncol(df) - 1))]
  return(df)
}

############################################################################
# Reports findings to Excel workbook with each check as a separate sheet
# Reads previous Excel report to compare with current report
report_metrics_diff_excel <- function(project_name, report_name, results, out_date = today, date_previous_report = NULL) {
  # read the previous report
  previous_report <- build_output_filename(
    projectName = project_name,
    report_type = report_name,
    outDir = dir.output,
    documentName = report_name,
    extension = "xlsx",
    outDate = date_previous_report
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
        colnames(merged_df)[colnames(merged_df) == "diff"] <- paste("Difference since", date_previous_report)
        return(merged_df)
      } else {
        return(df)
      }
    } else {
    }
  })
  # set the names of the output list
  names(out) <- names(results)
  return(out)
}
