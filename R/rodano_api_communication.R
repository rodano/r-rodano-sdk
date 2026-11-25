########## Communication with Rodano's API ##########
#' @importFrom httr POST GET PUT DELETE content add_headers http_status authenticate
#' @importFrom pbapply pbapply pbmapply pbsapply pboptions
#' @importFrom uuid UUIDgenerate
#' @importFrom getPass getPass
NULL

.get_with_retries <- function(url, auth = NULL, maxAttempts = 5, operation = "Request") {
  maxAttempts <- suppressWarnings(as.integer(maxAttempts)[1])
  if (is.na(maxAttempts) || maxAttempts < 1) {
    warning(sprintf("%s failed: maxAttempts must be a positive integer.", operation))
    return(NULL)
  }

  last_response <- NULL
  last_error <- NULL

  for (attempt in seq_len(maxAttempts)) {
    last_error <- NULL
    request <- list(url = url)
    if (!is.null(auth)) {
      request$config <- auth
    }

    last_response <- tryCatch(
      do.call(httr::GET, request),
      error = function(e) {
        last_error <<- conditionMessage(e)
        NULL
      }
    )

    if (!is.null(last_response) && last_response$status_code == 200) {
      return(last_response)
    }
  }

  detail <- if (!is.null(last_error)) {
    sprintf(" Last error: %s", last_error)
  } else if (!is.null(last_response)) {
    sprintf(" Last HTTP status: %i.", last_response$status_code)
  } else {
    ""
  }

  warning(sprintf(
    "%s failed at %s after %i attempt%s.%s",
    operation,
    url,
    maxAttempts,
    if (maxAttempts == 1) "" else "s",
    detail
  ), call. = FALSE)

  NULL
}

#############################
### Connection and robots ###
#############################

#' Retrieve authentication token
#'
#' Retrieves an authentication token for a given platform using specific
#' credentials. Prompts for email and password if not provided.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param email Character or NULL. The email used for login. If NULL,
#'   prompts the user for input.
#' @param pwd Character or NULL. The password used for login. If NULL,
#'   prompts the user for secure input.
#'
#' @return A character string containing the authentication token, or
#'   \code{NULL} with a warning if login fails.
#'
#' @export
get_connection_token <- function(urlBase, email = NULL, pwd = NULL) {
  if (is.null(email)) email <- readline(prompt = "Please enter e-mail: ")
  if (is.null(pwd)) pwd <- getPass::getPass(msg = "Enter password: ")

  resp <- tryCatch(
    httr::POST(
      url = sprintf("%s/sessions", urlBase),
      body = list(email = email, password = pwd),
      encode = "json"
    ),
    error = function(e) {
      warning(sprintf("Login request failed: %s", conditionMessage(e)), call. = FALSE)
      NULL
    }
  )

  if (is.null(resp)) {
    return(NULL)
  }

  if (resp$status_code == 201) {
    return(as.character(httr::content(resp)$token))
  }

  warning(sprintf(
    "Failed to login %s (HTTP %i). Please check your credentials.",
    email,
    resp$status_code
  ), call. = FALSE)
  NULL
}

#' Retrieve robot credentials
#'
#' Retrieves a robot account from a given platform. If multiple robots are
#' available or if the desired robot is ambiguous, the user will be prompted
#' to select from a list.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param token Character. Authentication token of a user enabled to view robots.
#' @param role Character or NULL. ID of the role wanted for the robot. If NULL,
#'   all robot roles are considered.
#' @param autoLogout Logical. If TRUE, logs out the user used for robot
#'   retrieval (default: FALSE).
#'
#' @return A JSON robot object containing robot credentials and configuration,
#'   or \code{NULL} with a warning if none can be retrieved.
#'
#' @export
get_connection_robot <- function(urlBase, token, role = NULL, autoLogout = FALSE) {
  a_ <- create_authentication(list(Token = token))
  resp <- tryCatch(
    httr::GET(
      url = sprintf("%s/robots", urlBase),
      config = a_
    ),
    error = function(e) {
      warning(sprintf("Robot retrieval failed: %s", conditionMessage(e)), call. = FALSE)
      NULL
    }
  )

  if (autoLogout) logout_user(urlBase, a_)

  if (is.null(resp)) {
    return(NULL)
  }

  if (resp$status_code == 200) {
    robots <- httr::content(resp, type = "application/json", encoding = "UTF-8")$objects # Get list of existing robots
    robots <- robots[sapply(robots, function(r) !r$removed)] # Retain only enabled robots
    if (!is.null(role)) robots <- robots[sapply(robots, function(r) r$roles[[1]]$profileId == role)] # Retain requested role

    if (length(robots) == 0) {
      warning(sprintf("No robots found on %s.", urlBase), call. = FALSE)
      return(NULL)
    }

    if (length(robots) > 1) { # Let user choose if more than 1 robot was found or if user did not specify wanted role
      print("Please enter row of desired robot:")
      mapply(
        function(i, r) {
          print(sprintf("%i. %s %s", i, r$name, r$roles[[1]]$profileId))
        },
        seq_along(robots),
        robots
      )

      r_idx <- 0
      while (r_idx <= 0 || r_idx > length(robots)) {
        r_idx <- as.numeric(readline())
        if (r_idx > length(robots)) print(sprintf("Max. number of robots is %i.", length(robots)))
      }

      return(robots[[r_idx]])
    }

    return(robots[[1]])
  }

  warning(sprintf(
    "Robots could not be retrieved from %s (HTTP %i).",
    urlBase,
    resp$status_code
  ), call. = FALSE)
  NULL
}


#' Create authentication object
#'
#' Creates an authentication object for API requests. Accepts either a token
#' (for robots) or username/password combination (for registered users).
#'
#' @param opt A list or named vector containing authentication credentials.
#'   Must include either \code{Token} or both \code{Username} and \code{Password}.
#'
#' @return An authentication configuration object for use with httr.
#'
#' @export
create_authentication <- function(opt) {
  if ("Token" %in% names(opt)) {
    return(httr::add_headers(Authorization = sprintf("Bearer %s", opt$Token)))
  } else if (all(c("Username", "Password") %in% names(opt))) {
    return(authenticate(opt$Username, opt$Password, type = "basic"))
  } else {
    stop("Authentication options do not contain necessary information (either \"Token\" or \"Username\" + \"Password\").")
  }
}

#' Retrieve connected user information
#'
#' Retrieves the User Data Transfer Object (DTO) for the currently
#' authenticated user. Used to verify that authentication is active.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#'
#' @return A User DTO object containing user information and permissions on
#'   success. Returns \code{NULL} with a warning on failure (connection error,
#'   invalid credentials, etc.).
#'
#' @details
#' This function is designed for checking connection status and does not stop
#' execution on error. Instead, it returns \code{NULL} and issues a warning,
#' allowing the caller to check connectivity without error handling overhead.
#'
#' @examples
#' \dontrun{
#' # Check if user is connected
#' auth <- create_authentication(list(Token = "my_token"))
#' user <- get_connected_user_dto("https://test.rodano.ch/api", auth)
#'
#' if (!is.null(user)) {
#'   cat("Connected as:", user$name, "\n")
#' } else {
#'   cat("Not connected\n")
#' }
#' }
#'
#' @export
get_connected_user_dto <- function(urlBase, auth) {
  resp <- .get_with_retries(
    sprintf("%s/me", urlBase),
    auth = auth,
    maxAttempts = 1,
    operation = "Connected user retrieval"
  )

  if (is.null(resp)) {
    return(NULL)
  }

  httr::content(resp, encoding = "UTF-8")
}

#' Log out user session
#'
#' Deletes a user's session on the platform, invalidating the current
#' authentication token.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#'
#' @return Invisibly \code{TRUE} on success or \code{FALSE} with a warning if
#'   logout fails.
#'
#' @export
logout_user <- function(urlBase, auth) {
  resp <- tryCatch(
    httr::DELETE(
      url = sprintf("%s/sessions", urlBase),
      config = auth
    ),
    error = function(e) {
      warning(sprintf("Logout request failed: %s", conditionMessage(e)), call. = FALSE)
      NULL
    }
  )

  if (is.null(resp)) {
    return(invisible(FALSE))
  }

  if (resp$status_code == 204) {
    return(invisible(TRUE))
  }

  warning(sprintf("Logout unsuccessful (HTTP %i).", resp$status_code), call. = FALSE)
  invisible(FALSE)
}

############################
### Study configuration ###
############################

#' Retrieve study configuration
#'
#' Extracts the complete study configuration from the platform. Includes
#' retry logic to handle temporary failures.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#'
#' @return A nested list containing the study's configuration settings, or
#'   \code{NULL} with a warning if retrieval fails.
#'
#' @export
get_config <- function(urlBase, auth, maxAttempts = 5) {
  cfgAddress <- sprintf("%s/config/study", urlBase)
  getRes <- .get_with_retries(
    cfgAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Configuration retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  cont <- httr::content(getRes,
    type = "application/json",
    encoding = "UTF-8"
  )

  return(cont)
}

#' Retrieve public study configuration
#'
#' Extracts the public study configuration from the platform. This endpoint
#' does not require authentication and includes retry logic for reliability.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#'
#' @return A nested list containing the study's public configuration settings,
#'   or \code{NULL} with a warning if retrieval fails.
#'
#' @export
get_public_config <- function(urlBase, maxAttempts = 5) {
  cfgAddress <- sprintf("%s/config/public-study", urlBase)
  getRes <- .get_with_retries(
    cfgAddress,
    maxAttempts = maxAttempts,
    operation = "Public configuration retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  cont <- httr::content(getRes,
    type = "application/json",
    encoding = "UTF-8"
  )

  return(cont)
}

########################
### Users and scopes ###
########################

#' Create new user
#'
#' Creates a new user in the platform. Optionally activates the user
#' account if password is provided.
#'
#' @param name Character. Name of the user.
#' @param email Character. Email address of the user.
#' @param profile Character. Profile ID to assign to the user.
#' @param parentPk Integer. Primary key of the parent scope to attach the user to.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param activate Logical. Whether the user must be enabled immediately
#'   (default: FALSE).
#' @param pwd Character or NULL. Password for the user. When NULL (default),
#'   no password is set and user is not automatically enabled.
#'
#' @return Parsed response content from user creation request.
#'
#' @export
add_user <- function(name, email, profile, parentPk, urlBase, auth, activate = FALSE, pwd = NULL) {
  # Create user
  r_ <- httr::POST(
    url = sprintf("%s/users", urlBase),
    body = list(
      name = name, email = email,
      externallyManaged = "false", languageId = "en",
      role = list(scopePk = parentPk, profileId = profile)
    ),
    config = auth,
    encode = c("json")
  )
  if (r_$status_code != 201) stop("Unable to create user")
  u <- httr::content(r_, "parsed")

  # Enable user
  if (activate && !is.null(pwd)) {
    invitation <- httr::content(
      httr::GET(
        url = sprintf("%s/mails?intent=%s&sortBy=creationTime&orderAscending=false&recipient=%s", urlBase, "Send%20user%20activation%20e-mail", URLencode(email, reserved = TRUE, repeated = TRUE)),
        config = auth
      ),
      "parsed"
    )$objects[[1]]$textBody
    uuid <- regmatches(invitation, regexpr("[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}", invitation))
    r_ <- httr::POST(
      url = sprintf("%s/user/activation/%s", urlBase, uuid),
      body = list("acceptPolicies" = "true", "password" = pwd),
      config = auth,
      encode = c("json")
    )
    if (r_$status_code != 204) stop("Unable to enable user")
  }

  return(u)
}

#' Create new scope
#'
#' Creates a new scope with start date set to current time (UTC).
#' Optionally sets up automatic enrollment criteria for virtual scopes.
#'
#' @param code Character. Code identifier for the scope.
#' @param name Character. Display name for the scope.
#' @param model Character. Model identifier for the scope type.
#' @param parentPk Integer. Primary key of the parent scope.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param criteria List or NULL. Enrollment criteria for automatic enrollment.
#'   Should be a list of conditions with elements "datasetModelId",
#'   "attributeId", "operator" (e.g., "EQUALS"), and "value". When NULL
#'   (default), no automatic enrollment is configured.
#'
#' @return Parsed response content from scope creation request.
#'
#' @export
add_scope <- function(code, name, model, parentPk, urlBase, auth, criteria = NULL) {
  # Create scope
  rScope <- httr::POST(
    url = sprintf("%s/scopes", urlBase),
    body = list(
      "parentScopePk" = parentPk, "code" = code, "shortname" = name,
      "modelId" = model, "startDate" = format(as.POSIXlt(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%S.000Z")
    ),
    config = auth,
    encode = c("json")
  )

  # Check for success and extract content of response
  if (rScope$status_code != 201) stop(sprintf("Unable to create %s %s (%s) with parent %s at %s.", model, name, code, parentPk, urlBase))
  rScope <- httr::content(rScope, "parsed")

  # Optional: add enrolment model
  # TODO: handle errors here
  if (!is.null(criteria)) {
    # Inject criteria into scope creation's response; format it to be sendable
    rScope$enrollmentModel <- criteria
    rScope <- recursive_apply(rScope, function(el) {
      if (is.logical(el)) el <- ifelse(el, "true", "false")
      if (length(el) == 0) el <- ""
      return(el)
    })
    # Add criteria to enrolment model
    httr::POST(
      url = sprintf("%s/scopes/%i/enrollment/count", urlBase, rScope$pk),
      body = rScope$enrollmentModel,
      config = auth,
      encode = c("json")
    )
    # Save enrolment model
    httr::PUT(
      url = sprintf("%s/scopes/%i", urlBase, rScope$pk),
      body = rScope,
      config = auth,
      encode = c("json")
    )
  }

  return(rScope)
}

################################
### Data / metadata extracts ###
################################

#' Extract data table from platform
#'
#' Retrieves a CSV data export for a specified table from the platform API
#' and returns it as a data frame. Includes retry logic and column type
#' inference configuration.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param expName Character. The data table model ID to retrieve.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#' @param guessMax Integer. Number of rows to scan for column type inference.
#'   The CSV parser (via \code{httr::content()} using \code{readr::read_csv()})
#'   examines the first N rows to infer the data type of each column (numeric,
#'   character, date, etc.). If later rows contain values incompatible with the
#'   inferred type, they may be coerced or read as NA. Options:
#'   \itemize{
#'     \item -1 (default): Scans all rows (most accurate, recommended for sparse data)
#'     \item 0: Uses readr's default behavior (1,000 rows)
#'     \item Positive integer: Scans specified number of rows
#'   }
#'   Increase this value if you encounter unexpected type coercion, especially
#'   in datasets where certain column types only become apparent after many rows.
#' @param includeModifDate Logical. Should the export include modification
#'   dates of fields? (default: FALSE).
#' @param scopePk Integer or NULL. Optional scope primary key to filter the
#'   extract to a specific scope.
#' @param as_character Logical. If TRUE, all columns are returned as character
#'   strings without any type detection or conversion (default: FALSE).
#'   This disables automatic type inference and overrides the guessMax parameter.
#'
#' @return A data frame containing the extracted table data, or \code{NULL}
#'   with a warning if retrieval fails.
#'
#' @export
get_extract <- function(urlBase, auth, expName, maxAttempts = 5, guessMax = -1, includeModifDate = FALSE, scopePk = NULL, as_character = FALSE) {
  expAddress <- sprintf(
    "%s/extracts?datasetModelIds=%s%s%s",
    urlBase,
    expName,
    ifelse(includeModifDate, "&withModificationDates=true", ""),
    ifelse(!is.null(scopePk), sprintf("&scopePk=%s", scopePk), "")
  )

  getRes <- .get_with_retries(
    expAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Extract retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  # Build content retrieval arguments
  content_args <- list(
    type = "text/csv",
    na = character(),
    encoding = "UTF-8"
  )
  
  # If as_character is TRUE, read all columns as character; otherwise use guessMax
  if (as_character) {
    content_args$col_types <- readr::cols(.default = readr::col_character())
  } else {
    content_args$guess_max <- guessMax
  }
  
  cont <- do.call(httr::content, c(list(x = getRes), content_args))
  df <- as.data.frame(cont)

  return(df)
}

# Internal function to get available parent scopes
# Not exported - used by get_extract_resilient
get_available_parents <- function(urlBase, auth, childScopeModelId = "PATIENT", right = "READ", maxAttempts = 5) {
  # Retrieve available parent scopes based on child scope model and access rights
  # For example, childScopeModelId="PATIENT" returns CENTER parents where user has read access
  scopeAddress <- sprintf(
    "%s/scopes/relations/available-parents?right=%s&scopeModelId=%s",
    urlBase,
    right,
    childScopeModelId
  )

  getRes <- .get_with_retries(
    scopeAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Parent scope list retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  # Parse JSON response
  cont <- httr::content(getRes, as = "parsed", encoding = "UTF-8")

  # Extract scope PKs from the response
  # Response is an array of scope objects, each with a 'pk' field
  if (is.list(cont) && length(cont) > 0) {
    scope_pks <- sapply(cont, function(x) {
      if (is.list(x) && !is.null(x$pk)) {
        return(as.integer(x$pk))
      }
      return(NULL)
    })
    scope_pks <- unlist(scope_pks[!sapply(scope_pks, is.null)])
  } else {
    scope_pks <- integer(0)
  }

  if (length(scope_pks) == 0) {
    warning("No parent scopes found for childScopeModelId: ", childScopeModelId)
  }

  return(scope_pks)
}

#' Extract data table with resilient parent-scope-based download
#'
#' Retrieves a CSV data export by breaking the download into chunks based on
#' parent scopes (e.g., centers). This approach is more resilient for large
#' datasets that may timeout when downloaded as a single request. The function
#' queries for available parent scopes where the user has write access, then
#' downloads data for each parent scope separately and combines the results.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param expName Character. The data table model ID to retrieve.
#' @param childScopeModelId Character. The child scope model ID used to identify
#'   parent scopes (default: "PATIENT"). For example, "PATIENT" will return data
#'   chunked by CENTER parents where the user has write access.
#' @param maxAttempts Integer. Maximum number of retry attempts per scope
#'   (default: 5).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details.
#' @param includeModifDate Logical. Should the export include modification
#'   dates of fields? (default: FALSE).
#' @param as_character Logical. If TRUE, all columns are returned as character
#'   strings without any type detection or conversion (default: FALSE).
#' @param showProgress Logical. Should a progress bar be displayed? (default: TRUE).
#' @param continueOnError Logical. Should the download continue if a scope fails?
#'   If TRUE, failed scopes are skipped with a warning. If FALSE, the function
#'   stops on the first error (default: TRUE).
#'
#' @return A data frame containing the extracted table data from all parent
#'   scopes, or \code{NULL} with a warning if no data can be retrieved.
#'
#' @export
get_extract_resilient <- function(urlBase, auth, expName, childScopeModelId = "PATIENT",
                                  maxAttempts = 5, guessMax = -1, includeModifDate = FALSE,
                                  as_character = FALSE, showProgress = TRUE, continueOnError = TRUE) {
  # Get list of parent scope PKs
  parent_pks <- get_available_parents(urlBase, auth, childScopeModelId, right = "READ", maxAttempts)

  if (is.null(parent_pks)) {
    warning("Cannot perform resilient download because parent scopes could not be retrieved.", call. = FALSE)
    return(NULL)
  }

  if (length(parent_pks) == 0) {
    warning("No parent scopes found. Cannot perform resilient download.", call. = FALSE)
    return(NULL)
  }

  message(sprintf("Starting resilient download for %d parent scopes...", length(parent_pks)))

  # Function to download data for a single parent scope
  download_for_scope <- function(scope_pk) {
    data <- tryCatch(
      get_extract(
        urlBase = urlBase,
        auth = auth,
        expName = expName,
        maxAttempts = maxAttempts,
        guessMax = guessMax,
        includeModifDate = includeModifDate,
        scopePk = scope_pk,
        as_character = as_character
      ),
      error = function(e) {
        warning(
          sprintf("Failed to download data for scope PK %s: %s", scope_pk, conditionMessage(e)),
          call. = FALSE
        )
        NULL
      }
    )

    if (is.null(data)) {
      if (!continueOnError) {
        stop(sprintf("Failed to download data for scope PK %s.", scope_pk), call. = FALSE)
      }
      return(NULL)
    }

    data
  }

  # Download data for each parent scope with optional progress bar
  if (showProgress) {
    results_list <- pbapply::pblapply(parent_pks, download_for_scope)
  } else {
    results_list <- lapply(parent_pks, download_for_scope)
  }

  # Remove NULL results (failed downloads if continueOnError=TRUE)
  results_list <- results_list[!sapply(results_list, is.null)]

  if (length(results_list) == 0) {
    warning("All scope downloads failed. No data retrieved.", call. = FALSE)
    return(NULL)
  }

  # Combine all data frames
  combined_df <- do.call(rbind, results_list)

  # Reset row names
  rownames(combined_df) <- NULL

  message(sprintf(
    "Successfully combined data from %d parent scopes (%d total rows).",
    length(results_list), nrow(combined_df)
  ))

  return(combined_df)
}

#' Extract workflow summary report
#'
#' Retrieves workflow summary details from the platform as a CSV export.
#' Can optionally include historical workflow data.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param repName Character. Name of the workflow report to retrieve.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details.
#' @param withHistory Logical. Should the export include historical workflow
#'   data? (default: FALSE).
#' @param scopePk Integer. Optional scope primary key to filter the
#'   report to a specific scope (default: 1).
#'
#' @return A data frame containing the workflow report, or \code{NULL} with a
#'   warning if retrieval fails.
#'
#' @export
get_report <- function(urlBase, auth, repName, maxAttempts = 5, guessMax = -1, withHistory = FALSE, scopePk = 1) {
  repAddress <- sprintf("%s/widget/workflow-summary/%s/export%s?scopePk=%s", urlBase, repName, ifelse(withHistory, "/history", ""), scopePk)

  getRes <- .get_with_retries(
    repAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Report retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  cont <- httr::content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}

#' Extract workflow summary report with resilient parent-scope-based download
#'
#' Retrieves a workflow summary report by breaking the download into chunks based on
#' parent scopes (e.g., centers). This approach is more resilient for large
#' datasets that may timeout when downloaded as a single request. The function
#' queries for available parent scopes where the user has read access, then
#' downloads the report for each parent scope separately and combines the results.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param repName Character. Name of the workflow report to retrieve.
#' @param childScopeModelId Character. The child scope model ID used to identify
#'   parent scopes (default: "PATIENT"). For example, "PATIENT" will return data
#'   chunked by CENTER parents where the user has read access.
#' @param maxAttempts Integer. Maximum number of retry attempts per scope
#'   (default: 5).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details.
#' @param withHistory Logical. Should the export include historical workflow
#'   data? (default: FALSE).
#' @param showProgress Logical. Should a progress bar be displayed? (default: TRUE).
#' @param continueOnError Logical. Should the download continue if a scope fails?
#'   If TRUE, failed scopes are skipped with a warning. If FALSE, the function
#'   stops on the first error (default: TRUE).
#'
#' @return A data frame containing the workflow report data from all parent
#'   scopes, or \code{NULL} with a warning if no data can be retrieved.
#'
#' @seealso \code{\link{get_report}} for retrieving a single workflow report.
#'
#' @export
get_report_resilient <- function(urlBase, auth, repName, childScopeModelId = "PATIENT",
                                 maxAttempts = 5, guessMax = -1, withHistory = FALSE,
                                 showProgress = TRUE, continueOnError = TRUE) {
  # Get list of parent scope PKs
  parent_pks <- get_available_parents(urlBase, auth, childScopeModelId, right = "READ", maxAttempts)

  if (is.null(parent_pks)) {
    warning("Cannot perform resilient download because parent scopes could not be retrieved.", call. = FALSE)
    return(NULL)
  }

  if (length(parent_pks) == 0) {
    warning("No parent scopes found. Cannot perform resilient download.", call. = FALSE)
    return(NULL)
  }

  message(sprintf("Starting resilient download for %d parent scopes...", length(parent_pks)))

  # Function to download report for a single parent scope
  download_for_scope <- function(scope_pk) {
    data <- tryCatch(
      get_report(
        urlBase = urlBase,
        auth = auth,
        repName = repName,
        maxAttempts = maxAttempts,
        guessMax = guessMax,
        withHistory = withHistory,
        scopePk = scope_pk
      ),
      error = function(e) {
        warning(
          sprintf("Failed to download report for scope PK %s: %s", scope_pk, conditionMessage(e)),
          call. = FALSE
        )
        NULL
      }
    )

    if (is.null(data)) {
      if (!continueOnError) {
        stop(sprintf("Failed to download report for scope PK %s.", scope_pk), call. = FALSE)
      }
      return(NULL)
    }

    data
  }

  # Download report for each parent scope with optional progress bar
  if (showProgress) {
    results_list <- pbapply::pblapply(parent_pks, download_for_scope)
  } else {
    results_list <- lapply(parent_pks, download_for_scope)
  }

  # Remove NULL results (failed downloads if continueOnError=TRUE)
  results_list <- results_list[!sapply(results_list, is.null)]

  if (length(results_list) == 0) {
    warning("All scope downloads failed. No data retrieved.", call. = FALSE)
    return(NULL)
  }

  # Combine all data frames
  combined_df <- do.call(rbind, results_list)

  # Reset row names
  rownames(combined_df) <- NULL

  message(sprintf(
    "Successfully combined data from %d parent scopes (%d total rows).",
    length(results_list), nrow(combined_df)
  ))

  return(combined_df)
}

#' Extract workflow widget data
#'
#' Retrieves workflow widget data from the platform as a CSV export.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param widName Character. Name of the workflow widget to retrieve.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details.
#'
#' @return A data frame containing the widget data, or \code{NULL} with a
#'   warning if retrieval fails.
#'
#' @export
get_widget <- function(urlBase, auth, widName, maxAttempts = 5, guessMax = -1) {
  widAddress <- sprintf("%s/widget/workflow/%s/export?scopePks=1", urlBase, widName)

  getRes <- .get_with_retries(
    widAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Widget retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  cont <- httr::content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}

#' Extract overdue widget report
#'
#' Downloads the overdue report widget data from the platform. This report
#' shows items that are past their expected completion dates.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param overdueWidName Character. Name of the overdue widget to retrieve.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details.
#'
#' @return A data frame containing the overdue report data, or \code{NULL}
#'   with a warning if retrieval fails.
#'
#' @export
get_overdue_widget <- function(urlBase, auth, overdueWidName, maxAttempts = 5, guessMax = -1) {
  repAddress <- sprintf("%s/widget/overdue/%s/export", urlBase, overdueWidName)

  getRes <- .get_with_retries(
    repAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Overdue report retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  cont <- httr::content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}


#' Extract patient transfer records
#'
#' Retrieves the patient transfer history from the platform. Transfers
#' represent movements of patients between scopes (e.g., between centers
#' or sites).
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#' @param scopeModelId Character. ID of the scope model for which to extract
#'   transfers (default: "PATIENT").
#'
#' @return A data frame containing the transfer records, or \code{NULL} with a
#'   warning if retrieval fails.
#'
#' @export
get_transfers <- function(urlBase, auth, maxAttempts = 5, scopeModelId = "PATIENT") {
  trfAddress <- sprintf("%s/reports/transfers?scopeModelId=%s", urlBase, scopeModelId)

  getRes <- .get_with_retries(
    trfAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Transfers retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  df <- as.data.frame(httr::content(getRes,
    type = "text/csv",
    encoding = "UTF-8"
  ))

  return(df)
}

#' Extract event records
#'
#' Retrieves the event export file from the platform. Events represent
#' significant occurrences or milestones within the study (e.g., patient
#' visits, data collection events).
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of
#'   failure (default: 5).
#' @param scopeModelId Character. ID of the scope model for which to extract
#'   events (default: "PATIENT").
#'
#' @return A data frame containing the event records, or \code{NULL} with a
#'   warning if retrieval fails.
#'
#' @export
get_events <- function(urlBase, auth, maxAttempts = 5, scopeModelId = "PATIENT") {
  evtAddress <- sprintf("%s/reports/events?scopeModelId=%s", urlBase, scopeModelId)

  getRes <- .get_with_retries(
    evtAddress,
    auth = auth,
    maxAttempts = maxAttempts,
    operation = "Events retrieval"
  )
  if (is.null(getRes)) {
    return(NULL)
  }

  df <- as.data.frame(httr::content(getRes,
    type = "text/csv",
    encoding = "UTF-8"
  ))

  return(df)
}

##############################
### Batch retrieval of     ###
### extracts and reports   ###
##############################

#' Retrieve multiple data tables
#'
#' Retrieves data from multiple tables on the server in a batch operation using
#' \code{\link{get_extract}}. Returns a named list where each element is a
#' data frame from one table.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param tableIds Character vector. Data table model IDs to retrieve.
#' @param includeModifDate Logical. Should exports include modification dates of
#'   fields? (default: FALSE).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details on handling sparse
#'   data with date columns.
#' @param as_character Logical. If TRUE, all columns are returned as character
#'   strings without any type detection or conversion (default: FALSE).
#'   This disables automatic type inference and overrides the guessMax parameter.
#'
#' @return A named list, with one data frame per successfully retrieved table
#'   and \code{NULL} for failed tables. List names are the lowercase table IDs.
#'
#' @seealso
#'   \code{\link{get_extract}} for retrieving a single data table,
#'   \code{\link{get_extracts_resilient}} for resilient batch downloads.
#'
#' @export
get_extracts <- function(urlBase, auth, tableIds, includeModifDate, guessMax = -1, as_character = FALSE) {
  # Get all exports
  data <- lapply(tableIds, function(datasetId) {
    tempdf <- get_extract(urlBase, auth, datasetId, includeModifDate = includeModifDate, guessMax = guessMax, as_character = as_character)
    return(tempdf)
  })
  # Format and return the list
  names(data) <- tolower(tableIds)
  return(data)
}

#' Retrieve multiple data tables with resilient parent-scope-based download
#'
#' Retrieves data from multiple tables on the server in a batch operation using
#' \code{\link{get_extract_resilient}}. This approach breaks downloads into chunks
#' based on parent scopes (e.g., centers), making it more resilient for large
#' datasets that may timeout when downloaded as single requests. Returns a named
#' list where each element is a data frame from one table.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param tableIds Character vector. Data table model IDs to retrieve.
#' @param childScopeModelId Character. The child scope model ID used to identify
#'   parent scopes (default: "PATIENT"). For example, "PATIENT" will return data
#'   chunked by CENTER parents where the user has read access.
#' @param includeModifDate Logical. Should exports include modification dates of
#'   fields? (default: FALSE).
#' @param guessMax Integer. Number of rows to scan for column type inference
#'   (default: -1). See \code{\link{get_extract}} for details on handling sparse
#'   data with date columns.
#' @param as_character Logical. If TRUE, all columns are returned as character
#'   strings without any type detection or conversion (default: FALSE).
#' @param showProgress Logical. Should a progress bar be displayed? (default: TRUE).
#' @param continueOnError Logical. Should the download continue if a scope fails?
#'   If TRUE, failed scopes are skipped with a warning for that table. If FALSE,
#'   the function stops on the first error (default: TRUE).
#'
#' @return A named list, with one data frame per successfully retrieved table
#'   and \code{NULL} for failed tables. List names are the lowercase table IDs.
#'
#' @details
#'   This function is particularly useful when:
#'   \itemize{
#'     \item Working with large datasets that may timeout
#'     \item Downloading data across multiple centers or sites
#'     \item Needing robust error handling for partial failures
#'   }
#'
#'   For smaller datasets or single-table downloads, consider using
#'   \code{\link{get_extract}} or \code{\link{get_extract_resilient}} directly.
#'
#' @seealso
#'   \code{\link{get_extracts}} for non-resilient batch downloads,
#'   \code{\link{get_extract_resilient}} for single table with resilient download.
#'
#' @export
get_extracts_resilient <- function(urlBase, auth, tableIds, childScopeModelId = "PATIENT",
                                   includeModifDate = FALSE, guessMax = -1, as_character = FALSE,
                                   showProgress = TRUE, continueOnError = TRUE) {
  # Get all exports using resilient method
  data <- lapply(tableIds, function(datasetId) {
    tempdf <- get_extract_resilient(
      urlBase = urlBase,
      auth = auth,
      expName = datasetId,
      childScopeModelId = childScopeModelId,
      guessMax = guessMax,
      includeModifDate = includeModifDate,
      as_character = as_character,
      showProgress = showProgress,
      continueOnError = continueOnError
    )
    return(tempdf)
  })
  # Format and return the list
  names(data) <- tolower(tableIds)
  return(data)
}

#' Retrieve multiple workflow reports
#'
#' Retrieves multiple workflow reports from the server in a batch operation using
#' \code{\link{get_report}}. Returns a named list where each element is a
#' data frame from one report.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param reportIds Character vector. Workflow report names to retrieve.
#' @param maxAttempts Integer. Maximum number of retry attempts in case of failure (default: 5).
#' @param withHistory Logical. Should reports include historical data?
#'
#' @return A named list, with one data frame per successfully retrieved report
#'   and \code{NULL} for failed reports. List names are the lowercase IDs.
#'
#' @seealso \code{\link{get_report}} for retrieving a single workflow report.
#'
#' @export
get_reports <- function(urlBase, auth, reportIds, maxAttempts = 5, withHistory = FALSE) {
  # Get all workflow reports
  reports <- lapply(reportIds, function(reportName) {
    tempdf <- get_report(urlBase, auth, reportName, maxAttempts = maxAttempts, withHistory = withHistory)
    return(tempdf)
  })
  # Format and return the list
  names(reports) <- tolower(reportIds)
  return(reports)
}

#' Retrieve multiple widget reports
#'
#' Retrieves multiple widget reports from the server in a batch operation using
#' \code{\link{get_widget}}. Returns a named list where each element is a
#' data frame from one widget.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param widgetIds Character vector. Widget names to retrieve.
#'
#' @return A named list, with one data frame per successfully retrieved widget
#'   report and \code{NULL} for failed reports. List names are the lowercase IDs.
#'
#' @seealso \code{\link{get_widget}} for retrieving a single widget report.
#'
#' @export
get_widget_reports <- function(urlBase, auth, widgetIds) {
  # Get all widget reports
  widgetReports <- lapply(widgetIds, function(reportName) {
    tempdf <- get_widget(urlBase, auth, reportName)
    return(tempdf)
  })
  # Format and return the list
  names(widgetReports) <- tolower(widgetIds)
  return(widgetReports)
}

#' Retrieve multiple overdue widget reports
#'
#' Retrieves multiple overdue widget reports from the server in a batch
#' operation using \code{\link{get_overdue_widget}}. Returns a named list
#' where each element is a data frame from one overdue widget.
#'
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param widgetIds Character vector. Overdue widget names to retrieve.
#'
#' @return A named list, with one data frame per successfully retrieved widget
#'   report and \code{NULL} for failed reports. List names are the lowercase IDs.
#'
#' @seealso \code{\link{get_overdue_widget}} for retrieving a single overdue widget report.
#'
#' @export
get_overdue_widget_reports <- function(urlBase, auth, widgetIds) {
  # Get all overdue widget reports
  overduewidgetReports <- lapply(widgetIds, function(reportName) {
    tempdf <- get_overdue_widget(urlBase, auth, reportName)
    return(tempdf)
  })
  # Format and return the list
  names(overduewidgetReports) <- tolower(widgetIds)
  return(overduewidgetReports)
}

##############################
### Data / metadata update ###
##############################

#' Build URLs for data modification
#'
#' Constructs API endpoint URLs for data modification requests. Creates
#' one URL per row in the input data frame.
#'
#' @param df Data frame containing dataset identifiers. Must include columns
#'   for scope ID, dataset ID, and optionally event ID.
#' @param urlBase Character. The base URL to the platform's API.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#' @param scopeModelId Character. ID of the scope model where data is to be
#'   updated (default: "PATIENT").
#'
#' @return A character vector of URLs, one for each row in \code{df}.
#'
#' @keywords internal
#' @noRd
build_url <- function(df, urlBase, v = FALSE, scopeModelId = "PATIENT") {
  # TODO: check if df has necessary column names

  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")
  urls <- pbapply::pbapply(
    df,
    1,
    function(x, u) {
      visitPart <- ifelse("EVENT_ID" %in% colnames(df),
        sprintf("/events/%s", x["EVENT_ID"]),
        ""
      ) # Visit: if not furnished, we must not include it
      sprintf(
        "%s/scopes/%s%s/datasets/%s",
        u,
        x[paste(scopeModelId, "ID", sep = "_")],
        visitPart,
        x["DATASET_ID"]
      )
    },
    urlBase
  )

  return(urls)
}

#' Build URLs for repeatable dataset operations
#'
#' Constructs API endpoint URLs for repeatable dataset import, removal, or
#' restoration requests. Creates one URL per row in the input data frame.
#'
#' @param df Data frame containing scope and event identifiers.
#' @param urlBase Character. The base URL to the platform's API.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#' @param scopeModelId Character. ID of the scope model where data is to be
#'   updated (default: "PATIENT").
#' @param removeRestoreMode Logical. Are we building URLs for dataset removal
#'   or restoration? (default: FALSE).
#' @param action Character or NULL. Action to perform: "remove" or "restore".
#'   Required when \code{removeRestoreMode = TRUE}.
#' @param rationale Character or NULL. Rationale for the action. Required when
#'   \code{removeRestoreMode = TRUE}.
#'
#' @return A character vector of URLs, one for each row in \code{df}.
#'
#' @keywords internal
#' @noRd
build_url_multiple <- function(df, urlBase, v = FALSE, scopeModelId = "PATIENT", removeRestoreMode = FALSE, action = NULL, rationale = NULL) {
  if (removeRestoreMode && (is.null(action) || (action != "remove" && action != "restore"))) stop("An action remove or restore is required for dataset.")
  if (removeRestoreMode && is.null(rationale)) stop("A rationale is required for dataset removal.")
  rationale <- URLencode(rationale)

  # TODO: check if df has necessary column names

  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")
  urls <- pbapply::pbapply(
    df,
    1,
    function(x, u) {
      visitPart <- ifelse("EVENT_ID" %in% colnames(df),
        sprintf("/events/%s", x["EVENT_ID"]),
        ""
      ) # Visit: if not furnished, we must not include it
      removeRestPart <- ifelse(removeRestoreMode,
        sprintf("/%s/%s?rationale=%s", x["DATASET_ID"], action, rationale),
        ""
      ) # removal of daaset
      sprintf(
        "%s/scopes/%s%s/datasets%s",
        u,
        x[paste(scopeModelId, "ID", sep = "_")],
        visitPart,
        removeRestPart
      )
    },
    urlBase
  )

  return(urls)
}

#' Build URLs for workflow modifications
#'
#' Constructs API endpoint URLs for workflow modification requests. Creates
#' one URL per row in the input data frame.
#'
#' @param df Data frame containing workflow identifiers. Must include scope ID,
#'   workflow ID, and optionally event, dataset, and field IDs.
#' @param urlBase Character. The base URL to the platform's API.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#' @param scopeModel Character. ID of the scope model (as formatted in exports)
#'   where workflow is to be updated (default: "Patient").
#'
#' @return A character vector of URLs, one for each row in \code{df}.
#'
#' @keywords internal
#' @noRd
build_wf_url <- function(df, urlBase, v = FALSE, scopeModel = "Patient") {
  # TODO: check if df has necessary column names

  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")
  urls <- pbapply::pbapply(
    df,
    1,
    function(x, u) {
      visitPart <- ifelse("Event ID" %in% colnames(df) && !is.na(x["Event ID"]),
        sprintf("/events/%s", x["Event ID"]),
        ""
      ) # Visit: if not furnished, we must not include it
      datasetPart <- ifelse("Dataset ID" %in% colnames(df),
        sprintf("/datasets/%s", x["Dataset ID"]),
        ""
      ) # Dataset: if not furnished, we must not include it
      fieldPart <- ifelse("Field ID" %in% colnames(df),
        sprintf("/fields/%s", x["Field ID"]),
        ""
      ) # Field: if not furnished, we must not include it
      sprintf(
        "%s/scopes/%s%s%s%s/workflows/%s",
        u,
        x[paste(scopeModel, "ID", sep = " ")],
        visitPart,
        datasetPart,
        fieldPart,
        x["Workflow ID"]
      )
    },
    urlBase
  )

  return(urls)
}


#' Build URLs for workflow initialization
#'
#' Constructs API endpoint URLs for workflow initialization requests.
#' Retrieves field primary keys from dataset and field IDs, then builds
#' complete URLs.
#'
#' @param df Data frame containing workflow initialization identifiers. Must
#'   include scope PK, workflow ID, action ID, context, and optionally event PK,
#'   dataset PK.
#' @param urlBase Character. The base URL to the platform's API.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#' @param scopeModel Character. ID of the scope model (default: "PATIENT").
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param fieldID Character. Field ID on which the workflow will be initiated.
#'
#' @return A character vector of URLs, one for each row in \code{df}.
#'
#' @keywords internal
#' @noRd
build_wf_init_url_on_field <- function(df, urlBase, v = FALSE, scopeModel = "PATIENT", auth, fieldID) {
  # TODO: check if df has necessary column names

  # get field pks from dataset pk and field ID.
  r <- apply(df, 1, simplify = FALSE, function(x) {
    url <- build_url(t(data.frame(x)), urlBase)
    httr::GET(
      url = url,
      config = auth,
      encode = c("json")
    )
  })

  # Add fieldId to main dataframe
  fieldspk <- sapply(r, function(resp) {
    dataset <- httr::content(resp)
    result <- sapply(dataset$fields, function(field) {
      if (field$modelId == fieldID) field$pk else NULL
    })
    unlist(result, use.names = FALSE) # Convert to vector and remove names
  })

  df$FIELD_ID <- fieldspk

  # Build complete url
  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")
  urls <- pbapply::pbapply(
    df,
    1,
    function(x, u) {
      eventPart <- ifelse("EVENT_ID" %in% colnames(df),
        sprintf("/events/%s", x["EVENT_ID"]),
        ""
      ) # Visit: if not furnished, we must not include it
      datasetPart <- ifelse("DATASET_ID" %in% colnames(df),
        sprintf("/datasets/%s", x["DATASET_ID"]),
        ""
      ) # Dataset: if not furnished, we must not include it
      fieldPart <- ifelse("FIELD_ID" %in% colnames(df),
        sprintf("/fields/%s", x["FIELD_ID"]),
        ""
      ) # Field: if not furnished, we must not include it
      sprintf(
        "%s/scopes/%s%s%s%s/workflows",
        u,
        x[paste(scopeModel, "_ID", sep = "")],
        eventPart,
        datasetPart,
        fieldPart
      )
    },
    urlBase
  )

  return(urls)
}


#' Build payload for data modification
#'
#' Constructs the request payload for data modification requests. Creates
#' structured field-level updates for each dataset instance.
#'
#' @param df Data frame containing information to update. Must include a
#'   \code{DATASET_ID} column and one column per field to update.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#'
#' @return A list of payloads, one for each dataset instance. Each payload
#'   contains the dataset PK and a list of field updates.
#'
#' @keywords internal
#' @noRd
build_payload <- function(df, v = FALSE) {
  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")

  colsToUpdate <- names(df)[!names(df) %in% "DATASET_ID"]

  # Check if urls are always in phase with dataset order?
  pl <- unname(pbapply::pbsapply(df$DATASET_ID, function(datasetPk) {
    datasetRow <- df[df$DATASET_ID == datasetPk, , drop = FALSE]
    datasetPl <- list(
      pk = datasetPk,
      fields = list()
    )

    datasetPl$fields <- unname(sapply(colsToUpdate, function(attributeModelId) {
      list(
        modelId = attributeModelId,
        value = datasetRow[, attributeModelId],
        datasetPk = datasetPk
      )
    }, simplify = FALSE))

    return(datasetPl)
  },
  simplify = FALSE
  ))

  return(pl)
}

#' Build payload for repeatable dataset import
#'
#' Constructs the request payload for repeatable dataset importation.
#' Generates unique UUIDs for each new dataset instance.
#'
#' @param df Data frame containing information to import. Each row represents
#'   one instance of the repeatable dataset.
#' @param datasetId Character. ID of the dataset model being imported.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#'
#' @return A list of payloads, one for each dataset instance. Each payload
#'   contains a unique UUID, the dataset model ID, and field values.
#'
#' @keywords internal
#' @noRd
build_payload_multiple <- function(df, datasetId, v = FALSE) {
  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")

  colsToImport <- names(df)

  pl <- unname(pbapply::pbsapply(rownames(df), function(rowName) {
    # Verify name of elements
    newUuid <- uuid::UUIDgenerate()
    datasetPl <- list(
      id = newUuid,
      modelId = datasetId,
      fields = list()
    )

    datasetPl$fields <- unname(sapply(colsToImport, function(attributeModelId) {
      list(
        modelId = attributeModelId,
        value = df[rowName, attributeModelId],
        datasetId = newUuid
      )
    }, simplify = FALSE))

    return(datasetPl)
  },
  simplify = FALSE
  ))
  #
  # pl <- recursive_apply(pl, function(el) {
  # 	if (is.logical(el)) el <- ifelse(el, "true", "false")
  # 	if (length(el) == 0) el <- ""
  # 	return(el)
  # })

  return(pl)
}

#' Build payload for workflow modifications from data frame
#'
#' Constructs the request payload for workflow state changes. Extracts
#' workflow information from data frame columns, allowing each row to
#' specify a different workflow action.
#'
#' @param df Data frame with workflow information. Must include columns:
#'   \code{Workflow}, \code{Action}, and \code{Context}.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#'
#' @return A list of payloads, one for each workflow action. Each contains
#'   workflow ID, action ID, and rationale.
#'
#' @keywords internal
#' @noRd
build_wf_payload_from_df <- function(df, v = FALSE) {
  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")
  payloadList <- pbapply::pbapply(
    df,
    1,
    function(x) {
      return(list(
        workflowId = unname(x["Workflow"]),
        actionId = unname(x["Action"]),
        rationale = unname(x["Context"])
      ))
    }
  )

  return(payloadList)
}

#' Build payload for workflow initialization with explicit parameters
#'
#' Constructs the request payload for workflow initialization. Accepts
#' explicit workflow parameters that will be applied to all records in the
#' data frame. Used when you want to trigger the same workflow action on
#' multiple records.
#'
#' @param df Data frame with records on which to initialize workflows.
#' @param workflow Character. Workflow ID to initialize.
#' @param action Character. Action ID to perform.
#' @param context Character. Rationale or context for the action.
#' @param v Logical. Should progress bar be displayed? (default: FALSE).
#'
#' @return A list of payloads, one for each workflow initialization.
#'
#' @keywords internal
#' @noRd
build_wf_init_payload <- function(df, workflow, action, context, v = FALSE) {
  if (v) pbapply::pboptions(type = "txt") else pbapply::pboptions(type = "none")
  payloadList <- pbapply::pbapply(
    df,
    1,
    function(x) {
      return(list(
        workflowId = workflow,
        actionId = action,
        rationale = context
      ))
    }
  )

  return(payloadList)
}

#' Send PUT requests
#'
#' Sends multiple PUT requests in batch, one for each URL/payload pair.
#' Includes error handling to prevent failures from stopping the entire batch.
#'
#' @param url Character vector. URLs for the PUT requests.
#' @param payload List or NULL. Payloads for each request. If NULL, sends
#'   empty payloads.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param comment Character. Context to insert in the audit trail header.
#' @param v Logical. Should progress timer be displayed? (default: FALSE).
#'
#' @return A list of response objects from the PUT requests.
#'
#' @keywords internal
#' @noRd
send_put <- function(url, payload = NULL, auth, comment, v = FALSE) {
  if (v) pbapply::pboptions(type = "timer") else pbapply::pboptions(type = "none")

  if (is.null(payload)) {
    # If the payload is NULL, set it to an empty string
    payload <- ""
  }

  res <- pbapply::pbmapply(
    function(x, y, z, c) {
      tryCatch(
        {
          httr::PUT(
            url = x,
            body = y,
            config = z,
            c,
            encode = c("json")
          )
        },
        error = function(e) {
          return(status_code = NULL)
        }
      )
    },
    url, # x
    payload,
    rep(list(auth), length(url)), # z
    rep(list(comment), length(url)),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )
  return(res)
}

#' Send POST requests
#'
#' Sends multiple POST requests in batch, one for each URL/payload pair.
#' Includes error handling to prevent failures from stopping the entire batch.
#'
#' @param url Character vector. URLs for the POST requests.
#' @param payload List. Payloads for each request.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param comment Character. Context to insert in the audit trail header.
#' @param v Logical. Should progress timer be displayed? (default: FALSE).
#'
#' @return A list of response objects from the POST requests.
#'
#' @keywords internal
#' @noRd
send_post <- function(url, payload, auth, comment, v = FALSE) {
  if (v) pbapply::pboptions(type = "timer") else pbapply::pboptions(type = "none")

  res <- pbapply::pbmapply(
    function(x, y, z, c) {
      tryCatch(
        {
          httr::POST(
            url = x,
            body = y,
            config = z,
            c,
            encode = c("json")
          )
        },
        error = function(e) {
          return(status_code = NULL)
        }
      )
    },
    url,
    payload,
    rep(list(auth), length(url)),
    rep(list(comment), length(url)),
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  return(res)
}

#' Send workflow PUT requests
#'
#' Sends multiple PUT requests for workflow operations in batch. Similar to
#' \code{\link{send_put}} but without audit trail comment headers.
#'
#' @param url Character vector. URLs for the workflow PUT requests.
#' @param payload List. Payloads for each workflow request.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param v Logical. Should progress timer be displayed? (default: FALSE).
#'
#' @return A list of response objects from the PUT requests.
#'
#' @keywords internal
#' @noRd
send_wf_put <- function(url, payload, auth, v = FALSE) {
  if (v) pbapply::pboptions(type = "timer") else pbapply::pboptions(type = "none")

  res <- pbapply::pbmapply(
    function(x, y) {
      tryCatch(
        {
          httr::PUT(
            url = x,
            body = y,
            config = auth,
            encode = c("json")
          )
        },
        error = function(e) {
          return(status_code = NULL)
        }
      )
    },
    url,
    payload,
    SIMPLIFY = FALSE,
    USE.NAMES = TRUE
  )

  return(res)
}

#' Update EDC data
#'
#' High-level function to update Electronic Data Capture (EDC) system data.
#' Orchestrates URL building, payload construction, and request sending.
#'
#' @param df Data frame containing data to update, including dataset and field IDs.
#' @param fNames Character vector. Column names from df corresponding to the field IDs to include in the import.
#' @param atComment Character. Audit trail comment for the update operation.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param verb Integer. Verbosity level: 0 (silent), 1 (operation names),
#'   2+ (progress bars) (default: 0).
#' @param scopeModelId Character. ID of the scope model where data is updated
#'   (default: "PATIENT").
#'
#' @return A list of response objects from the update operation.
#'
#' @export
update_edc <- function(df, fNames, atComment, urlBase, auth, verb = 0, scopeModelId = "PATIENT") {
  # Audit trail comments
  headAT <- create_at_header(atComment)

  # Urls
  if (verb >= 1) print("Building urls...")
  url <- build_url(df, urlBase, verb >= 2, scopeModelId)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")
  pl <- build_payload(df[, c("DATASET_ID", fNames), drop = FALSE], verb >= 2)

  # TODO: check if each field is under query when API can access workflows; close queries / inform user / whatever
  # TODO: see what will happen when resource is locked

  # Send the put request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_put(url, pl, auth, headAT, verb >= 2)

  return(r)
}

#' Update EDC with repeatable datasets
#'
#' High-level function to import or update repeatable datasets in the EDC.
#' Handles dataset creation with unique identifiers.
#'
#' @param df Data frame containing data to import. Each row is one instance.
#' @param fNames Character vector. Column names from \code{df} corresponding
#'   to the field IDs to include in the import.
#' @param datasetId Character. ID of the repeatable dataset model.
#' @param atComment Character. Audit trail comment for the operation.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param verb Integer. Verbosity level: 0 (silent), 1 (operation names),
#'   2+ (progress bars) (default: 0).
#' @param scopeModelId Character. ID of the scope model (default: "PATIENT").
#'
#' @return A list of response objects from the import operation.
#'
#' @export
update_edc_multiple <- function(df, fNames, datasetId, atComment, urlBase, auth, verb = 0, scopeModelId = "PATIENT") {
  # Audit trail comments
  headAT <- create_at_header(atComment)

  # Urls
  if (verb >= 1) print("Building urls...")
  url <- build_url_multiple(df, urlBase, verb >= 2, scopeModelId)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")
  pl <- build_payload_multiple(df[, fNames, drop = FALSE], datasetId, verb >= 2)

  # Send the post request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_post(url, pl, auth, headAT, verb >= 2)

  return(r)
}

#' Remove repeatable datasets
#'
#' Removes one or more instances of repeatable datasets from the EDC.
#' Documents the removal in the audit trail.
#'
#' @param df Data frame with scope PKs and dataset PKs to remove.
#' @param datasetId Character. Dataset model ID (included for consistency).
#' @param atComment Character. Audit trail comment documenting the removal.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param verb Integer. Verbosity level: 0 (silent), 1 (operation names),
#'   2+ (progress bars) (default: 0).
#' @param scopeModelId Character. ID of the scope model (default: "PATIENT").
#'
#' @return A list of response objects from the removal operation.
#'
#' @export
remove_edc_multiple <- function(df, datasetId, atComment, urlBase, auth, verb = 0, scopeModelId = "PATIENT") {
  headAT <- create_at_header(atComment)
  # Urls
  if (verb >= 1) print("Building urls...")
  url <- build_url_multiple(df, urlBase, verb >= 2, scopeModelId, removeRestoreMode = TRUE, action = "remove", rationale = atComment)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")

  # Send the post request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_put(url, payload = NULL, auth, headAT, verb >= 2)

  return(r)
}

#' Trigger workflow action on field
#'
#' Initiates a workflow action on specific field instances. Useful for
#' programmatically triggering workflow state changes.
#'
#' @param df Data frame with scope PK, event PK (optional), and dataset ID.
#' @param workflow Character. Workflow ID to initialize.
#' @param action Character. Action ID to perform.
#' @param fieldID Character. Field model ID on which to trigger the workflow.
#' @param context Character. Rationale or context for the action.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param verb Integer. Verbosity level: 0 (silent), 1 (operation names),
#'   2+ (progress bars) (default: 0).
#' @param scopeModel Character. ID of the scope model (default: "PATIENT").
#'
#' @return A list of response objects from the workflow initialization.
#'
#' @export
trigger_action_workflow_on_field <- function(df, workflow, action, fieldID, context, urlBase, auth, verb = 0, scopeModel = "PATIENT") {
  # TODO check what to put in the header
  headAT <- create_at_header(context)
  # Urls
  if (verb >= 1) print("Building urls...")

  url <- build_wf_init_url_on_field(df = df, urlBase = urlBase, auth = auth, scopeModel = scopeModel, fieldID = fieldID)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")
  pl <- build_wf_init_payload(df, workflow, action, context, verb >= 2)

  # Send the put request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_post(url, pl, auth, headAT, verb >= 2)

  return(r)
}


#' Restore removed repeatable datasets
#'
#' Restores previously removed instances of repeatable datasets in the EDC.
#' Documents the restoration in the audit trail.
#'
#' @param df Data frame with scope PKs and dataset PKs to restore.
#' @param datasetId Character. Dataset model ID (included for consistency).
#' @param atComment Character. Audit trail comment documenting the restoration.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param verb Integer. Verbosity level: 0 (silent), 1 (operation names),
#'   2+ (progress bars) (default: 0).
#' @param scopeModelId Character. ID of the scope model (default: "PATIENT").
#'
#' @return A list of response objects from the restoration operation.
#'
#' @export
restore_edc_multiple <- function(df, datasetId, atComment, urlBase, auth, verb = 0, scopeModelId = "PATIENT") {
  headAT <- create_at_header(atComment)

  # Urls
  if (verb >= 1) print("Building urls...")
  url <- build_url_multiple(df, urlBase, verb >= 2, scopeModelId, removeRestoreMode = TRUE, action = "restore", rationale = atComment)
  # Payloads
  if (verb >= 1) print("Constructing payloads...")

  # Send the post request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_put(url, payload = NULL, auth, headAT, verb >= 2)

  return(r)
}

#' Update workflow status
#'
#' High-level function to update workflow states in the EDC. Orchestrates
#' URL building, payload construction, and request sending for workflows.
#'
#' @param df Data frame with workflow information including patient, visit,
#'   dataset, field IDs, and workflow name, action, and context.
#' @param urlBase Character. The base URL to the platform's API.
#' @param auth An authentication object created by \code{\link{create_authentication}}.
#' @param verb Integer. Verbosity level: 0 (silent), 1 (operation names),
#'   2+ (progress bars) (default: 0).
#' @param scopeModel Character. ID of the scope model as formatted in exports
#'   (default: "Patient").
#'
#' @return A list of response objects from the workflow update operation.
#'
#' @export
update_wf <- function(df, urlBase, auth, verb = 0, scopeModel = "Patient") {
  # Url for request
  if (verb >= 1) print("Building urls...")
  url <- build_wf_url(df, urlBase, verb >= 2, scopeModel = scopeModel)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")
  pl <- build_wf_payload_from_df(df, verb >= 2)

  # Send the put request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_wf_put(url, pl, auth, verb >= 2)

  return(r)
}


##### HELPERS #####

#' Get platform instance URL
#'
#' Retrieves the API URL for a requested study platform from a configuration
#' file. Useful for managing multiple study environments.
#'
#' @param filePath Character. Path to CSV file containing platform listings.
#'   File must have columns: Study, Environment, and Address.
#' @param studyName Character. Name of the study of interest.
#' @param studyEnv Character. Environment identifier (e.g., "local",
#'   "validation", "production").
#'
#' @return Character. The API base URL for the specified study and environment,
#'   or \code{NULL} with a warning if no matching instance exists.
#'
#' @examples
#' \dontrun{
#' # Example CSV file format (instances.csv):
#' # Study,Environment,Address
#' # TRIAL001,local,http://localhost:8080/api
#' # TRIAL001,validation,https://trial001-val.example.com/api
#' # TRIAL001,production,https://trial001.example.com/api
#' # TRIAL002,production,https://trial002.example.com/api
#'
#' # Retrieve production URL for TRIAL001
#' url <- get_instance_url("instances.csv", "TRIAL001", "production")
#' # Returns: "https://trial001.example.com/api"
#' }
#'
#' @export
get_instance_url <- function(filePath, studyName, studyEnv) {
  urls <- read.csv(filePath, header = TRUE, stringsAsFactors = FALSE) # Read instance file csv
  url <- urls[urls$Study == studyName & urls$Environment == studyEnv, "Address"] # Get row of wanted study and environment

  if (length(url) > 1) {
    stop(sprintf(
      "Multiple instances were found in file %s on study %s (environment %s).",
      filePath, studyName, studyEnv
    ))
  }
  if (length(url) == 0) {
    warning(sprintf(
      "No instance was found in file %s on study %s (environment %s).",
      filePath, studyName, studyEnv
    ), call. = FALSE)
    return(NULL)
  }

  return(url)
}

#' Create audit trail header
#'
#' Creates an HTTP header containing the audit trail context/rationale for
#' data modifications.
#'
#' @param m Character. Message to insert in the audit trail comment.
#'
#' @return An httr header object with the X-Rationale field.
#'
#' @keywords internal
#' @noRd
create_at_header <- function(m) {
  httr::add_headers("X-Rationale" = m)
}

#' Interpret PUT response status
#'
#' Interprets HTTP responses from PUT requests and categorizes them by
#' success/failure status and reason.
#'
#' @param responses List. Response objects from PUT/POST requests.
#' @param translate Logical. Should status messages be translated? (Currently
#'   unused, default: FALSE).
#'
#' @return A list with two elements:
#'   \itemize{
#'     \item categories: Character vector of status categories
#'       ("Success", "Client error", "Failure", etc.)
#'     \item reasons: Character vector of status reasons
#'       ("OK", "Bad Request", "No answer was received", etc.)
#'   }
#'
#' @keywords internal
#' @noRd
get_custom_status <- function(responses, translate = FALSE) {
  categories <- sapply(
    responses,
    function(x) {
      ifelse(is.null(x["status_code"]),
        "Failure",
        http_status(x)$category
      )
    }
  )
  reasons <- sapply(
    responses,
    function(x) {
      ifelse(is.null(x["status_code"]),
        "No answer was received",
        http_status(x)$reason
      )
    }
  )

  return(list(categories, reasons))
}


#' Restore original data types
#'
#' Restores the original type of each element in a row. Useful when working
#' with \code{apply} which converts data frames to matrices (losing type info).
#'
#' @param rowIn Vector. Elements with uniform (dummy) type from matrix conversion.
#' @param tIn Character vector. Original type names for each element
#'   ("character", "numeric", "integer", "logical", "Date").
#'
#' @return A list of elements with their original data types restored.
#' @keywords internal
#' @noRd
retrieve_types <- function(rowIn, tIn) {
  rowOut <- mapply(
    function(r, t) {
      if (t == "character") {
        return(as.character(r))
      }
      if (t == "numeric") {
        return(as.numeric(r))
      }
      if (t == "integer") {
        return(as.integer(r))
      }
      if (t == "logical") {
        return(as.logical(r))
      }
      if (t == "Date") {
        return(as.Date(r))
      }
    },
    r = rowIn,
    t = tIn,
    SIMPLIFY = FALSE
  )

  return(rowOut)
}

#' Apply function recursively to list
#'
#' Applies a function recursively to each leaf element of a nested list
#' structure. Unlike \code{rapply}, this function preserves NULL elements.
#'
#' @param x Initially a list, potentially with nested lists.
#' @param fn Function. The function to apply to leaf (non-list) elements.
#'
#' @return The same nested structure with \code{fn} applied to all leaf elements.
#' @keywords internal
#' @noRd
recursive_apply <- function(x, fn) {
  # If x is a list, return a list.
  if (is.list(x)) {
    return(lapply(x, recursive_apply, fn))
  }

  # If x is something else, return fn applied to x.
  return(fn(x))
}
