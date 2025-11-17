source("../rodano_api_communication.R", chdir = TRUE)
source("setup_config.R", chdir = TRUE)
library(testthat)
###########################################################
###########################################################
## Precondition of test suite:
##  To be run on study-demo
##  Study contains a robot with role Investigator
##  Study contains a robot with role Administrator
###########################################################
###########################################################


test_that("get_public_config", {
  output <- get_public_config(URLBASE)
  expect_true(all(c("id", "shortname", "defaultLanguage") %in% names(output)))
  expect_true("id" %in% names(output$defaultLanguage))
})

test_that("get_connection_token", {
  output <- get_connection_token(URLBASE, USER_EMAIL)
  expect_true(!is.null(output))
  expect_true(!is.na(output))

  TOKEN <<- output
  AUTH <<- create_authentication(list(Token = output))
})

test_that("get_connected_user_dto", {
  output <- get_connected_user_dto(URLBASE, AUTH)
  expect_true("roles" %in% names(output))
})

test_that("get_connection_robot", {
  output_0 <- get_connection_robot(URLBASE, TOKEN, role = DATAENTRY_ID)
  expect_true(all(c("roles", "name", "key") %in% names(output_0))) # 'key' only used by robot-using tools
  expect_true(all(c("status", "profileId") %in% names(output_0$roles[[1]])))

  AUTH_ROBOT_DATAENTRY <<- create_authentication(list(Username = output_0$name, Password = output_0$key))

  output_1 <- get_connection_robot(URLBASE, TOKEN, role = MANAGER_ID)
  expect_true(all(c("roles", "name", "key") %in% names(output_1))) # 'key' only used by robot-using tools
  expect_true(all(c("status", "profileId") %in% names(output_1$roles[[1]])))

  AUTH_ROBOT_MANAGER <<- create_authentication(list(Username = output_1$name, Password = output_1$key))
})

test_that("get_config", {
  output <- get_config(URLBASE, AUTH)
  expect_true("datasetModels" %in% names(output))
  expect_true(all(c("exportable", "shortname", "id") %in% names(output$datasetModels[[1]])))
  expect_true("en" %in% names(output$datasetModels[[1]]$shortname))
})

test_that("add_scope", {
  gen_scopecode <- function() paste(sample(0:9, 10, TRUE), collapse = "")

  # Regular scope
  output_0 <- add_scope(gen_scopecode(), "Test Scope", SCOPEMODEL_ID_LV2, 1, URLBASE, AUTH, criteria = NULL)
  expect_true("pk" %in% names(output_0)) # Needed for further tests and scope creation tools

  SCOPE_PK_LV2 <<- output_0$pk

  # Virtual scope with enrolment model
  output_1 <- add_scope(gen_scopecode(), "Test Scope", SCOPEMODEL_ID_LV2_VIRT, 1, URLBASE, AUTH, criteria = list(list(
    "datasetModelId" = DATASET_ON_SCOPE_ID,
    "fieldModelId" = ATTRIBUTE_ON_SCOPE_ID,
    "operator" = "Equals to",
    "value" = ATTRIBUTE_ON_SCOPE_VALUE
  )))
})

test_that("add_user", {
  gen_email <- function() paste0(c(sample(0:9, 10, TRUE), "@rodano.ch"), collapse = "")

  # Just create and invite the user

  output_1 <- add_user("Test User", gen_email(), DATAENTRY_ID, SCOPE_PK_LV2, URLBASE, AUTH, activate = FALSE, pwd = NULL)

  # Also activate the user

  output_2 <- add_user("Test User", gen_email(), DATAENTRY_ID, SCOPE_PK_LV2, URLBASE, AUTH, activate = TRUE, pwd = "TestPwd01!")
})

test_that("get_extract", {
  # No modification date

  output_1 <- get_extract(URLBASE, AUTH, DATASET_ID)
  expect_true(ncol(output_1) > 0)

  DF <<- output_1[1:10, ]

  # With modification date

  output_2 <- get_extract(URLBASE, AUTH, DATASET_ID, includeModifDate = TRUE)
  expect_true(ncol(output_2) > ncol(output_1))
})

test_that("get_report", {
  output_1 <- get_report(URLBASE, AUTH, REPORT_ID)
  expect_true(ncol(output_1) > 0)

  WF <<- output_1

  output_2 <- get_report(URLBASE, AUTH, REPORT_ID, withHistory = TRUE)
  expect_true(ncol(output_2) > 0)
})

test_that("get_widget", {
  output <- get_widget(URLBASE, AUTH, WIDGET_ID)
  expect_true(ncol(output) > 0)
})

test_that("get_transfers", {
  output <- get_transfers(URLBASE, AUTH, scopeModelId = SCOPEMODEL_ID_LEAF)
  expect_true(ncol(output) > 0)
})

test_that("get_events", {
  output <- get_events(URLBASE, AUTH, scopeModelId = SCOPEMODEL_ID_LEAF)
  expect_true(ncol(output) > 0)
})

test_that("update_edc", {
  DF[, ATTRIBUTE_ID] <- ATTRIBUTE_VALUE

  output <- update_edc(DF, c(ATTRIBUTE_ID), "Testing update_edc funtionality.", URLBASE, AUTH_ROBOT_DATAENTRY, scopeModelId = SCOPEMODEL_ID_LEAF)
  expect_true(all(sapply(output, function(el) "status_code" %in% names(el))))
  expect_true(all(sapply(output, function(el) el$status_code) == 200))
})

# test_that("update_edc_multiple", {
#     df_ <- data.frame(matrix(nrow = 1, ncol = 0))
#     df_[[ATTRIBUTE_MULTIPLE_ID]] <- ATTRIBUTE_MULTIPLE_VALUE
#     df_[[paste(SCOPEMODEL_ID_ROOT, "ID", sep = "_")]] <- 1

#     # df, fNames, datasetId, atComment, urlBase, auth, verb=0, scopeModelId='PATIENT'
#     output <- update_edc_multiple(df_, ATTRIBUTE_MULTIPLE_ID, DATASET_MULTIPLE_ID, "Testing update_edc_multiple funtionality.", URLBASE, AUTH_ROBOT_MANAGER, scopeModelId = SCOPEMODEL_ID_ROOT)
#     expect_true(all(sapply(output, function(el) "status_code" %in% names(el))))
#     expect_true(all(sapply(output, function(el) el$status_code) == 201))
# })

test_that("update_wf", {
  wf <- WF
  wf$Workflow <- WORKFLOW_ID
  wf$Action <- ACTION_ID
  wf$Context <- "Testing update_wf functionality."

  SCOPEMODEL_ID_LEAF_formatted <- paste(toupper(substr(SCOPEMODEL_ID_LEAF, 1, 1)),
    tolower(substr(SCOPEMODEL_ID_LEAF, 2, nchar(SCOPEMODEL_ID_LEAF))),
    sep = ""
  )
  output <- update_wf(wf, URLBASE, AUTH, scopeModel = SCOPEMODEL_ID_LEAF_formatted)
  expect_true(all(sapply(output, function(el) "status_code" %in% names(el))))
  expect_true(all(sapply(output, function(el) el$status_code) == 200))
})

test_that("logout_user", {
  logout_user(URLBASE, AUTH)
})


## TODO Add tests for remove_edc_multiple and restore_edc_multiple
