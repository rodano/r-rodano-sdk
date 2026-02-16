########## Communication with Rodano's API ##########

library("httr")
library("pbapply")
library("uuid")

#############################
### Connection and robots ###
#############################

# 'get_connection_token': Retrieves a token for a given platform with specific credentials
# Input: 'urlBase' - the url to the platform's API
#        'email' (optional) - the e-mail used for login
#        'pwd' (optional) - the password used for login
# Output: a token character string
get_connection_token <- function(urlBase, email = NULL, pwd = NULL) {
  if (is.null(email)) email <- readline(prompt = "Please enter e-mail: ")
  if (is.null(pwd)) pwd <- getPass::getPass(msg = "Enter password: ")

  resp <- POST(
    url = sprintf("%s/sessions", urlBase),
    body = list(email = email, password = pwd),
    encode = "json"
  )
  if (resp$status_code == 201) {
    token <- as.character(content(resp)$token)
  } else {
    stop(sprintf("Failed to login %s. Please check your credentials.", email), call. = FALSE)
  }
  return(token)
}

# 'get_connection_robot': Retrieves a robot from a given platform
# If desired robot is ambiguous, user will be prompted to select robot from a list
# Input: 'urlBase' - the url to the platform's API
#        'token' - the token of a user enabled to view robots
#        'role' (optional) - id of the role wanted for the robot to be retrieved
#        'autoLogout' (optional) - if TRUE, logs out user used for robot retrieval
# Output: A json robot object
get_connection_robot <- function(urlBase, token, role = NULL, autoLogout = FALSE) {
  a_ <- create_authentication(list(Token = token))
  resp <- GET(
    url = sprintf("%s/robots", urlBase),
    config = a_
  )
  if (autoLogout) logout_user(urlBase, a_)
  if (resp$status_code == 200) {
    robots <- content(resp, type = "application/json", encoding = "UTF-8")$objects # Get list of existing robots
    robots <- robots[sapply(robots, function(r) !r$removed)] # Retain only enabled robots
    if (!is.null(role)) robots <- robots[sapply(robots, function(r) r$roles[[1]]$profileId == role)] # Retain requested role

    if (length(robots) == 0) stop(sprintf("No robots found on %s.", urlBase)) # Error if no robotos are found

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
      while (r_idx <= 0 | r_idx > length(robots)) {
        r_idx <- as.numeric(readline())
        if (r_idx > length(robots)) print(sprintf("Max. number of robots is %i.", length(robots)))
      }

      return(robots[[r_idx]])
    }

    return(robots[[1]])
  }

  stop(sprintf("Robots could not be retrieved from %s.", urlBase))
}

# 'create_authentication': Creates an authentication object
# Input: 'opt' - a row of options with columns informing about Username and Password (for robots) or Token (for registered user)
# Output: an authentication object
create_authentication <- function(opt) {
  if ("Token" %in% names(opt)) {
    return(add_headers(Authorization = sprintf("Bearer %s", opt$Token)))
  } else if (all(c("Username", "Password") %in% names(opt))) {
    return(authenticate(opt$Username, opt$Password, type = "basic"))
  } else {
    stop("Authentication options do not contain necessary information (either \"Token\" or \"Username\" + \"Password\").")
  }
}

# 'get_connected_user_dto': Retrieves the User DTO from connected user
# Input: 'urlBase' - the url to the platform's API
#        'auth' - the user's authentication object
# Output: a User DTO object
get_connected_user_dto <- function(urlBase, auth) {
  resp <- GET(
    url = sprintf("%s/me", urlBase),
    config = auth,
    encode = "json"
  )

  if (resp$status_code == 200) {
    userDTO <- content(resp,
      encoding = "UTF-8"
    )
  } else {
    stop(sprintf("Failed to get user data (%i)", resp$status_code))
  }

  return(userDTO)
}

# 'logout_user': Deletes a user's session on a given platform
# Input : 'urlBase' - the url to the platform's API
#         'auth' - the user's authentication object
logout_user <- function(urlBase, auth) {
  resp <- DELETE(
    url = sprintf("%s/sessions", urlBase),
    config = auth
  )

  if (resp$status_code != 204) stop("Logout unsuccessful.")
}

############################
### Study configuration ###
############################

# 'get_config': extracts a study's whole configuration out of a GET response
# Input: 'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
# Output: the study's configuration shaped as a nested list
get_config <- function(urlBase, auth, maxAttempts = 5) {
  cfgAddress <- sprintf("%s/config/study", urlBase)

  # Try max. [maxAttempts] times to retrieve requested report
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = cfgAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Configuration retrieval at: %s failed. %s", cfgAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Configuration retrieval at: %s failed after %i attempts.", cfgAddress, maxAttempts))
  cont <- content(getRes,
    type = "application/json",
    encoding = "UTF-8"
  )

  return(cont)
}

# 'get_public_config': extracts a study's public configuration out of a GET response
# Input: 'urlBase' - the url to platform's API
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
# Output: the study's configuration shaped as a nested list
get_public_config <- function(urlBase, maxAttempts = 5) {
  cfgAddress <- sprintf("%s/config/public-study", urlBase)

  # Try max. [maxAttempts] times to retrieve requested report
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(url = cfgAddress)
      },
      error = function(e) print(sprintf("Configuration retrieval at: %s failed. %s", cfgAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Configuration retrieval at: %s failed after %i attempts.", cfgAddress, maxAttempts))
  cont <- content(getRes,
    type = "application/json",
    encoding = "UTF-8"
  )

  return(cont)
}

########################
### Users and scopes ###
########################

# 'add_user': sends POST requests to create a new user
# Input: 'name' - name of user
#        'email' - email of user
#        'profile' - profile to assign
#        'parentPk' - pk of parent to attach new scope
#        'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'activate' (optional) - boolean indicating whether user must be enabled or not.
#                                FALSE by default
#        'pwd' (optional) - password of user.
#                           When NULL (default), no password is set and user is not automatically enabled.
# Output: parsed response content from user creation's POST request
add_user <- function(name, email, profile, parentPk, urlBase, auth, activate = FALSE, pwd = NULL) {
  # Create user
  r_ <- POST(
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
  u <- content(r_, "parsed")

  # Enable user
  if (activate & !is.null(pwd)) {
    invitation <- content(
      GET(
        url = sprintf("%s/mails?intent=%s&sortBy=creationTime&orderAscending=false&recipient=%s", urlBase, "Send%20user%20activation%20e-mail", URLencode(email, reserved = TRUE, repeated = TRUE)),
        config = auth
      ),
      "parsed"
    )$objects[[1]]$textBody
    uuid <- regmatches(invitation, regexpr("[[:xdigit:]]{8}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{4}-[[:xdigit:]]{12}", invitation))
    r_ <- POST(
      url = sprintf("%s/user/activation/%s", urlBase, uuid),
      body = list("acceptPolicies" = "true", "password" = pwd),
      config = auth,
      encode = c("json")
    )
    if (r_$status_code != 204) stop("Unable to enable user")
  }

  return(u)
}

# 'add_scope': sends a POST request to create a new scope with start date = now (UTC) and optionally an automatic enrolment model (if scope provided, for virtual scopes)
# Input: 'code' - code of scope to create
#        'name' - name of scope to create
#        'model' - name of model of scope to create
#        'parentPk' - pk of parent to attach new scope
#        'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'criteria' (optional) - enrolment criteria.
#                                Shape: list of conditions (shape: list with elts "datasetModelId" (?), "attributeId", "operator"(e.g."EQUALS"), "value").
#                                When NULL (default), no automatic enrolment is set.
# Output: parsed response content from scope creation's POST request
add_scope <- function(code, name, model, parentPk, urlBase, auth, criteria = NULL) {
  # Create scope
  rScope <- POST(
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
  rScope <- content(rScope, "parsed")

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
    POST(
      url = sprintf("%s/scopes/%i/enrollment/count", urlBase, rScope$pk),
      body = rScope$enrollmentModel,
      config = auth,
      encode = c("json")
    )
    # Save enrolment model
    PUT(
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

# 'get_extract': extracts a csv out of a GET response
# Input: 'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'expName' - the name of the extract we want to retrieve
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
#        'guessMax' - number of rows of the extract to use to guess column type
#                       0 (default) - Uses the default behavior of readr::read_csv(), which typically scans the first 1,000 rows to guess column types.
#                       Positive integer (e.g., 100, 1000, 5000) - Explicitly specifies the number of rows to scan for type inference. More rows = more accurate type detection but slower parsing.
#                       -1 - Scans all rows in the file for type inference. Provides the most accurate type detection but can be slow for large files.
#        'includeModifDate' - should export include modification date of fields?
#        'scopePk' - optional scope primary key to filter the extract
# Output: the csv extract as a data.frame
get_extract <- function(urlBase, auth, expName, maxAttempts = 5, guessMax = 0, includeModifDate = FALSE, scopePk = NULL) {
  expAddress <- sprintf(
    "%s/extracts?datasetModelIds=%s%s%s",
    urlBase,
    expName,
    ifelse(includeModifDate, "&withModificationDates=true", ""),
    ifelse(!is.null(scopePk), sprintf("&scopePk=%s", scopePk), "")
  )

  # Try max. [maxAttempts] times to retrieve requested extract
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = expAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Extract retrieval at: %s failed. %s", expAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Extract retrieval at: %s failed after %i attempts.", expAddress, maxAttempts))
  cont <- content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}

# 'get_report': extracts workflow summary details
# Input: 'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'repName' - name of report
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
# Output: the ed exportreport as a data.frame
get_report <- function(urlBase, auth, repName, maxAttempts = 5, guessMax = 0, withHistory = FALSE) {
  repAddress <- sprintf("%s/widget/workflow-summary/%s/export%s?scopePk=1", urlBase, repName, ifelse(withHistory, "/history", ""))

  # Try max. [maxAttempts] times to retrieve requested report
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = repAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Report retrieval at: %s failed. %s", repAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Report retrieval at: %s failed after %i attempts.", repAddress, maxAttempts))
  cont <- content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}

# 'get_widget': extracts workflow widget
# Input: 'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'widName' - name of widget
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
# Output: the exported csv widget as a data.frame
get_widget <- function(urlBase, auth, widName, maxAttempts = 5, guessMax = 0) {
  widAddress <- sprintf("%s/widget/workflow/%s/export?scopePks=1", urlBase, widName)

  # Try max. [maxAttempts] times to retrieve requested report
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = widAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Widget retrieval at: %s failed. %s", widAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Widget retrieval at: %s failed after %i attempts.", widAddress, maxAttempts))
  cont <- content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}

#' download_overdue_patient_report: Download overdue report for a given patient
#' Input: 'urlBase' - the url to platform's API
#'        'auth' - an authentication object
#'        'patient' - patient identifier (string)
#'        'maxAttempts' - maximum number of times to try to get resource in case of failure
#'        'guessMax' - number of rows to use to guess column type
#' Output: the csv report as a data.frame
get_overdue_widget <- function(urlBase, auth, overdueWidName, maxAttempts = 5, guessMax = 0) {
  repAddress <- sprintf("%s/widget/overdue/%s/export", urlBase, overdueWidName)

  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = repAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Overdue report retrieval at: %s failed. %s", repAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  if (getRes$status_code != 200) stop(sprintf("Overdue report retrieval at: %s failed after %i attempts.", repAddress, maxAttempts))
  cont <- content(getRes,
    type = "text/csv",
    na = character(),
    encoding = "UTF-8",
    guess_max = guessMax
  )
  df <- as.data.frame(cont)

  return(df)
}


# 'get_transfers': extracts patient transfers file through GET request
# Input: 'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
# 				 'scopeModelId' - ID of scope model for which we want to extract transfers
# Output: the csv extract content
get_transfers <- function(urlBase, auth, maxAttempts = 5, scopeModelId = "PATIENT") {
  trfAddress <- sprintf("%s/reports/transfers?scopeModelId=%s", urlBase, scopeModelId)

  # Try max. [maxAttempts] times to retrieve requested report
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = trfAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Transfers retrieval at: %s failed. %s", trfAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Transfers retrieval at: %s failed after %i attempts.", trfAddress, maxAttempts))
  df <- as.data.frame(content(getRes,
    type = "text/csv",
    encoding = "UTF-8"
  ))

  return(df)
}

# 'get_events': extracts event export file through GET request
# Input: 'urlBase' - the url to platform's API
#        'auth' - an authentication object
#        'maxAttempts' - maximum number of times to try to get resource in case of failure
# Output: the csv export content
get_events <- function(urlBase, auth, maxAttempts = 5, scopeModelId = "PATIENT") {
  evtAddress <- sprintf("%s/reports/events?scopeModelId=%s", urlBase, scopeModelId)

  # Try max. [maxAttempts] times to retrieve requested report
  attempts <- 0
  success <- FALSE
  while (attempts < maxAttempts & !success) {
    getRes <- tryCatch(
      {
        GET(
          url = evtAddress,
          config = auth
        )
      },
      error = function(e) print(sprintf("Events retrieval at: %s failed. %s", evtAddress, e))
    )
    if (getRes$status_code != 200) {
      attempts <- attempts + 1
    } else {
      success <- TRUE
    }
  }

  # Throw error if still unsuccessful; extract content otherwise
  if (getRes$status_code != 200) stop(sprintf("Events retrieval at: %s failed after %i attempts.", evtAddress, maxAttempts))
  df <- as.data.frame(content(getRes,
    type = "text/csv",
    encoding = "UTF-8"
  ))

  return(df)
}

##############################
### Batch retrieval of     ###
### extracts and reports   ###
##############################
# 'get_extracts': function that retrieves multiple datasets from server
# Input : 'auth' - the user's authentication object
#         'tableIds' - vector of dataset model IDs to retrieve
#         'includeModifDate' - boolean indicating whether to include modification dates
# Output: returns a named list of dataframes containing the datasets
get_extracts <- function(auth, tableIds, includeModifDate){
  # Get all exports
  data <- lapply(tableIds, function(datasetId){
    tempdf <- get_extract(STUDYURL, auth, datasetId, includeModifDate = includeModifDate, guessMax = 5)
    return (tempdf)
  })
  # Format and return the list
  names(data) <- tolower(tableIds)
  return (data)
}

# 'get_reports': function that retrieves multiple workflow reports from server
# Input : 'auth' - the user's authentication object
#         'reportIds' - vector of workflow report names to retrieve
#         'withHistory' - boolean indicating whether to include historical data
# Output: returns a named list of dataframes containing the workflow reports
get_reports <-function(auth, reportIds, withHistory) {
  # Get all workflow reports
  reports <- lapply(reportIds, function(reportName){
    tempdf <- get_report(STUDYURL, auth, reportName, withHistory)
    return (tempdf)
  })
  # Format and return the list
  names(reports) <- tolower(reportIds)
  return(reports)
}

# 'get_widget_reports': function that retrieves multiple widget reports from server
# Input : 'auth' - the user's authentication object
#         'widgetIds' - vector of widget names to retrieve
# Output: returns a named list of dataframes containing the widget reports
get_widget_reports <-function(auth, widgetIds) {
  # Get all widget reports
  widgetReports <- lapply(widgetIds, function(reportName){
    tempdf <- get_widget(STUDYURL, auth, reportName)
    return (tempdf)
  })
  # Format and return the list
  names(widgetReports) <- tolower(widgetIds)
  return(widgetReports)
}

# 'get_overdue_widget_reports': function that retrieves multiple overdue widget reports from server
# Input : 'auth' - the user's authentication object
#         'widgetIds' - vector of overdue widget names to retrieve
# Output: returns a named list of dataframes containing the overdue widget reports
get_overdue_widget_reports <-function(auth, widgetIds) {
  # Get all overdue widget reports
  overduewidgetReports <- lapply(widgetIds, function(reportName){
    tempdf <- get_overdue_widget(STUDYURL, auth, reportName)
    return (tempdf)
  })
  # Format and return the list
  names(overduewidgetReports) <- tolower(widgetIds)
  return(overduewidgetReports)
}

##############################
### Data / metadata update ###
##############################

# 'build_url': builds an url for data modification to send through a PUT request
# Input: 'df' - a dataframe (data.frame) containing all necessary ids (scope, visit (if and only if dataset on visit), dataset)
#        'urlBase' - the url to platform's API
#        'v' - a boolean indicating whether we should show progress bar or not
#        'scopeModelId' - ID of scope model where data is to be updated
# Output: an nObs-long characters vector containing the url for each observation we want to update
build_url <- function(df, urlBase, v = FALSE, scopeModelId = "PATIENT") {
  # TODO: check if df has necessary column names

  if (v) pboptions(type = "txt") else pboptions(type = "none")
  urls <- pbapply(
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

# 'build_url_multiple': builds an url for repeatable data importation to send through a PUT request
# Input: 'df' - a dataframe (data.frame) containing all necessary ids (scope and visit (if and only if dataset on visit))
#        'urlBase' - the url to platform's API
#        'v' - a boolean indicating whether we should show progress bar or not
#        'removal' - boolean to build URL for dataset removal
#        'rationale' - Characters to provide if removal is TRUE
#        'scopeModelId' - ID of scope model where data is to be updated
# Output: an nObs-long characters vector containing the url for each observation we want to update
build_url_multiple <- function(df, urlBase, v = FALSE, scopeModelId = "PATIENT", removeRestoreMode = FALSE, action = NULL, rationale = NULL) {
  if (removeRestoreMode & (is.null(action) || (action != "remove" & action != "restore"))) stop("An action remove or restore is required for dataset.")
  if (removeRestoreMode & is.null(rationale)) stop("A rationale is required for dataset removal.")
  rationale <- URLencode(rationale)

  # TODO: check if df has necessary column names

  if (v) pboptions(type = "txt") else pboptions(type = "none")
  urls <- pbapply(
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

# 'build_wf_url': builds an url for workflow modification to send through a PUT request
# Input: 'df' - a dataframe (data.frame) containing all necessary ids (scope, visit (optionally), dataset (optionally), field (optionally), workflow)
#        'urlBase' - the url to platform's API
#        'v' - a boolean indicating whether we should show progress bar or not
#        'scopeModel' - ID of scope model, formatted for export, where data is to be updated
# Output: an nObs-long characters vector containing the url for each observation we want to update
build_wf_url <- function(df, urlBase, v = FALSE, scopeModel = "Patient") {
  # TODO: check if df has necessary column names

  if (v) pboptions(type = "txt") else pboptions(type = "none")
  urls <- pbapply(
    df,
    1,
    function(x, u) {
      visitPart <- ifelse("Event ID" %in% colnames(df) & !is.na(x["Event ID"]),
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


# 'buildWFInitURL': builds an url for workflow initialisaton to send through a POST request
# Input: 'df' - a dataframe (data.frame) containing all necessary ids (scopePk, eventPk (optionally), datasetPk (optionally), fieldPk (optionally), workflowId, actionId, context)
#        'urlBase' - the url to platform's API
#        'v' - a boolean indicating whether we should show progress bar or not
#        'scopeModel' - ID of scope model, formatted for export, where data is to be updated
#        'auth': an authentication object
#        'fieldID': field id on which the workflow will be initiated
# Output: an nObs-long characters vector containing the url for each observation we want to update
build_wf_init_url_on_field <- function(df, urlBase, v = FALSE, scopeModel = "PATIENT", auth, fieldID) {
  # TODO: check if df has necessary column names

  # get field pks from dataset pk and field ID.
  r <- apply(df, 1, simplify = FALSE, function(x) {
    url <- build_url(t(data.frame(x)), urlBase)
    GET(
      url = url,
      config = auth,
      encode = c("json")
    )
  })

  # Add fieldId to main dataframe
  fieldspk <- sapply(r, function(resp) {
    dataset <- content(resp)
    result <- sapply(dataset$fields, function(field) {
      if (field$modelId == fieldID) field$pk else NULL
    })
    unlist(result, use.names = FALSE) # Convert to vector and remove names
  })

  df$FIELD_ID <- fieldspk

  # Build complete url
  if (v) pboptions(type = "txt") else pboptions(type = "none")
  urls <- pbapply(
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


# 'build_payload': constructs the payload for data modification to send through a PUT request
# Input: 'df' - a dataframe (data.frame) containing all, and only the information to update in EDC + column DATASET_ID
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list containing payload (with 1 item per field to update) associated to each dataset instance
build_payload <- function(df, v = FALSE) {
  if (v) pboptions(type = "txt") else pboptions(type = "none")

  colsToUpdate <- names(df)[!names(df) %in% "DATASET_ID"]

  # Check if urls are always in phase with dataset order?
  pl <- unname(pbsapply(df$DATASET_ID, function(datasetPk) {
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

# 'build_payload_multiple': constructs the payload for repeatable data importation to send through a PUT request
# Input: 'df' - a dataframe (data.frame) containing all, and only the information to update in EDC
# 				 'datasetId': ID of the dataset to be imported
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list containing payload (with 1 item per field to update) associated to each dataset instance
build_payload_multiple <- function(df, datasetId, v = FALSE) {
  if (v) pboptions(type = "txt") else pboptions(type = "none")

  colsToImport <- names(df)

  pl <- unname(pbsapply(rownames(df), function(rowName) {
    # Verify name of elements
    newUuid <- UUIDgenerate()
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

# 'build_wf_payload': constructs the payload for workflow modification to send through a PUT request
# Input: 'df' - a dataframe (data.frame) containing all rows on which to perform action with info 'Workflow', 'Action' and 'Context'.
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list containing payload associated to each workflow instance
build_wf_payload <- function(df, v = FALSE) {
  if (v) pboptions(type = "txt") else pboptions(type = "none")
  payloadList <- pbapply(
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

# 'build_wf_payload': constructs the payload for workflow initialisation to send through a POST request
# Input: 'df' - a dataframe (data.frame) containing all rows on which to perform action with info 'Workflow', 'Action' and 'Context'.
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list containing payload associated to each workflow instance
build_wf_payload <- function(df, workflow, action, context, v = FALSE) {
  if (v) pboptions(type = "txt") else pboptions(type = "none")
  payloadList <- pbapply(
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

# 'send_put': sends as many PUT requests as there are observations
# Input: 'url' - an nObs-long list of url, each related to an observation
#        'payload' - an nObs-long list of payloads, can be null
#        'auth' - an authentication object
#        'comment' - the context to insert in audit trail
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list of PUT responses
send_put <- function(url, payload = NULL, auth, comment, v = FALSE) {
  if (v) pboptions(type = "timer") else pboptions(type = "none")

  if (is.null(payload)) {
    # If the payload is NULL, set it to an empty string
    payload <- ""
  }

  res <- pbmapply(
    function(x, y, z, c) {
      tryCatch(
        {
          o <- PUT(
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

# 'send_post': sends as many POST requests as there are observations
# Input: 'url' - an nObs-long list of url, each related to an observation
#        'payload' - an nObs-long list of payloads
#        'auth' - an authentication object
#        'comment' - the context to insert in audit trail
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list of PUT responses
send_post <- function(url, payload, auth, comment, v = FALSE) {
  if (v) pboptions(type = "timer") else pboptions(type = "none")

  res <- pbmapply(
    function(x, y, z, c) {
      tryCatch(
        {
          o <- POST(
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

# 'send_wf_put': sends as many PUT requests as there are observations
# Input: 'url' - an nObs-long list of url, each related to an observation
#        'payload' - an nObs-long list of payloads
#        'auth' - an authentication object
#        'v' - a boolean indicating whether we should show progress bar or not
# Output: an nObs-long list of PUT responses
send_wf_put <- function(url, payload, auth, v = FALSE) {
  if (v) pboptions(type = "timer") else pboptions(type = "none")

  res <- pbmapply(
    function(x, y) {
      tryCatch(
        {
          o <- PUT(
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

# 'update_edc': Updates the EDC
# Input: 'df' - a dataframe (data.frame) containing all necessary information
#        'fNames' - a vector containing names (string) of fields for EDC updating
#        'atComments' - a vector containing names (string) of columns containing audit trail comments for each element of 'fNames'
#        'urlBase' - the url to platform's API
#        'auth' - an authentication header to perform actions
#        'verb' - degree of progress printing (<1:nothing is displayed; 1: only the name of current operation is printed; >=2: the operations show progress state)
#        'scopeModelId' - ID of scope model where data is to be updated
# Output: the list of PUT request answers
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

# 'update_edc_multiple': Updates the EDC with repeatable datasets
# Input: 'df' - a dataframe (data.frame) containing all necessary information
#        'fNames' - a vector containing names (string) of fields for EDC updating
# 				 'datasetId': ID of the dataset to be imported
#        'atComments' - a vector containing names (string) of columns containing audit trail comments for each element of 'fNames'
#        'urlBase' - the url to platform's API
#        'auth' - an authentication header to perform actions
#        'verb' - degree of progress printing (<1:nothing is displayed; 1: only the name of current operation is printed; >=2: the operations show progress state)
#        'scopeModelId' - ID of scope model where data is to be updated
# Output: the list of PUT request answers
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

# 'remove_edc_multiple': remove the repeatable datasets
# Input: 'df' - a dataframe (data.frame) containing scopePk and datasetPk to remove dataset with correpsoding datasetPk
#        'atComments' - an audit trail comment to document multiple dataset removal
#        'urlBase' - the url to platform's API
#        'auth' - an authentication header to perform actions
#        'verb' - degree of progress printing (<1:nothing is displayed; 1: only the name of current operation is printed; >=2: the operations show progress state)
#        'scopeModelId' - ID of scope model where data is to be updated
# Output: the list of PUT request answers
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

# 'triggerActionWorkflow': trigger an action on an item
# Input: 'df' - a dataframe containing scopePk, eventPk, dataset id,
trigger_action_workflow_on_field <- function(df, workflow, action, fieldID, context, urlBase, auth, verb = 0, scopeModel = "PATIENT") {
  # TODO check what to put in the header
  headAT <- create_at_header(context)
  # Urls
  if (verb >= 1) print("Building urls...")

  url <- build_wf_init_url_on_field(df = df, urlBase = urlBase, auth = auth, scopeModel = scopeModel, fieldID = fieldID)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")
  pl <- build_wf_payload(df, workflow, action, context, verb >= 2)

  # Send the put request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_post(url, pl, auth, headAT, verb >= 2)

  return(r)
}


# 'restore_edc_multiple': remove the repeatable datasets
# Input: 'df' - a dataframe (data.frame) containing scopePk and datasetPk to remove dataset with correpsoding datasetPk
#        'atComments' - an audit trail comment to document multiple dataset removal
#        'urlBase' - the url to platform's API
#        'auth' - an authentication header to perform actions
#        'verb' - degree of progress printing (<1:nothing is displayed; 1: only the name of current operation is printed; >=2: the operations show progress state)
#        'scopeModelId' - ID of scope model where data is to be updated
# Output: the list of PUT request answers
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

# 'update_wf': Updates workflow status
# Input: 'df' - a dataframe (data.frame) containing all necessary information (Patient, visit, dataset, field and workflow ids + workflow name, action and context)
#        'urlBase' - the url to platform's API
#        'auth' - an authentication header to perform actions
#        'verb' - degree of progress printing (<1:nothing is displayed; 1: only the name of current operation is printed; >=2: the operations show progress state)
#        'scopeModel' - ID of scope model, formatted for export, where wf is to be updated
# Output: the list of PUT request answers
update_wf <- function(df, urlBase, auth, verb = 0, scopeModel = "Patient") {
  # Url for request
  if (verb >= 1) print("Building urls...")
  url <- build_wf_url(df, urlBase, verb >= 2, scopeModel = scopeModel)

  # Payloads
  if (verb >= 1) print("Constructing payloads...")
  pl <- build_wf_payload(df, verb >= 2)

  # Send the put request
  if (verb >= 1) print("Sending PUT requests...")
  r <- send_wf_put(url, pl, auth, verb >= 2)

  return(r)
}


##### HELPERS #####

# 'get_instance_url': Retrieves url for a requested platform
# Input: 'filePath' - path to file which contains listing of platforms
#        'studyName' - name of study of interest
#        'studyEnv' - environment of interest (local, validation, production)
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
    stop(sprintf(
      "No instance was found in file %s on study %s (environment %s).",
      filePath, studyName, studyEnv
    ))
  }

  return(url)
}

# 'create_at_header': Creates a header with audit trail context
# Input: 'm' - the message to insert in AT comment
# Output: a header object
create_at_header <- function(m) {
  add_headers("X-Rationale" = m)
}

# 'get_custom_status': Interprets the PUT responses and associates an exit status
# Input: 'responses' - a list with all reponses from PUT request
# Ouput: a same-length array with strings indicating whether the request succeeded or not
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


# 'retrieve_types': gives back to each element its original type (function created because 'apply' transforms data.frame into matrix)
# Input: 'rowIn' - vector of elements with the same dummy type
#        'tIn' - vector of cahracters containing types of each variable
# Output: a list of the elements given as input but with their correct data type
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

# 'recursive_apply': Applies a function recursively to each element of a list, not ignoring NULL elements
# Input: 'x' - initially, a list with (or without) nested lists
#        'fn' - the function to apply to leaf elements
# Output: the same structure with function applied to each leaf elements
recursive_apply <- function(x, fn) {
  # If x is a list, return a list.
  if (is.list(x)) {
    return(lapply(x, recursive_apply, fn))
  }

  # If x is something else, return fn applied to x.
  return(fn(x))
}
