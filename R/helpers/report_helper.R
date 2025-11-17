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
