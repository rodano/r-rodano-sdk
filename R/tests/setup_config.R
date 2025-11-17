# Configuration file for R Rodano SDK tests

# Base URL of Rodano instance
URLBASE <- "http://localhost:7586/api"

# Email of user to connect as
USER_EMAIL <- 'to_be_set@rodano.ch'

# ID of role with data entry rights on DATASET_ID. A robot with this role should be available in DB, with right on whole study
DATAENTRY_ID <- 'INVESTIGATOR'
# ID of role with workflow action rights on ACTION_ID A robot with this role should be available in DB
MANAGER_ID <- 'ADMIN'

# ID of a dataset to export
DATASET_ID <- 'DEMOGRAPHICS'
# ID of a multiple dataset, to import. Must be on root scope and be writtable by role MANAGER_ID
DATASET_MULTIPLE_ID <- 'RANDOMISATION'

# ID of report to export. Selected report should relate to a single workflow, where an action is available.
REPORT_ID <- 'DATA_MANAGEMENT_STATUS'

# ID of widget to export
WIDGET_ID <- 'EVENTS_TO_REVIEW'

# ID of a non-virtual scope model (must directly descend from root scope)
SCOPEMODEL_ID_LV2 <- 'CENTER'

# ID of a virtual scope (must directly descend from root scope)
SCOPEMODEL_ID_LV2_VIRT <- 'AGE_AGGREGATE'
# ID of a dataset on scope which is used for auto-enrolment
DATASET_ON_SCOPE_ID <- 'SUBJECT_DOCUMENT'
# ID of a field within DATASET_ON_SCOPE_ID
ATTRIBUTE_ON_SCOPE_ID <- 'AGE'
# Trigger value on ATTRIBUTE_ON_SCOPE_ID for enrolment model
ATTRIBUTE_ON_SCOPE_VALUE <- '15'

# ID of a field within DATASET_ID document. Must be editable by DATAENTRY_ID
ATTRIBUTE_ID <- 'GENDER'
# New value with which the attribute ATTRIBUTE_ID should be updated
ATTRIBUTE_VALUE <- 'M'
# ID of a field within DATASET_MULTIPLE_ID repeatable document. Must be editable by DATAENTRY_ID.
ATTRIBUTE_MULTIPLE_ID <- 'RAND_NUM'
# New value with which the attribute ATTRIBUTE_MULTIPLE_ID should be imported
ATTRIBUTE_MULTIPLE_VALUE <- '1'

# ID of workflow summarized by REPORT_ID
WORKFLOW_ID <- 'DM_STATUS'
# ID of the action to perform. Must be performeable by MANAGER_ID.
ACTION_ID <- 'REVIEW'

# ID of the leaf scope model, on which the eCRF is implemented
SCOPEMODEL_ID_LEAF <- 'PATIENT'
# ID of root scope model
SCOPEMODEL_ID_ROOT <- 'STUDY'