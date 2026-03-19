#!/usr/bin/env bash
#
# migrate_to_snake_case.sh
#
# Renames old camelCase function calls from the rodano SDK
# to the current snake_case convention in user R scripts.
#
# Usage:
#   ./migrate_to_snake_case.sh <target_directory>
#   ./migrate_to_snake_case.sh --dry-run <target_directory>
#
# Options:
#   --dry-run   Show what would change without modifying files
#
# Examples:
#   ./migrate_to_snake_case.sh ~/projects/my_study/R
#   ./migrate_to_snake_case.sh --dry-run /path/to/scripts

set -euo pipefail

# ---------- CLI parsing ----------
DRY_RUN=false
TARGET_DIR=""

for arg in "$@"; do
  case "$arg" in
    --dry-run) DRY_RUN=true ;;
    -*) echo "Unknown option: $arg" >&2; exit 1 ;;
    *)  TARGET_DIR="$arg" ;;
  esac
done

if [[ -z "$TARGET_DIR" ]]; then
  echo "Usage: $0 [--dry-run] <target_directory>" >&2
  exit 1
fi

if [[ ! -d "$TARGET_DIR" ]]; then
  echo "Error: '$TARGET_DIR' is not a directory." >&2
  exit 1
fi

# ---------- Rename map ----------
# Each entry is "oldCamelCase:new_snake_case".
# Longer names appear before shorter ones to prevent partial matches
# when using simple string replacement without word boundaries.
RENAMES=(
  # --- rodano_api_communication.R ---
  "triggerActionWorkflowOnField:trigger_action_workflow_on_field"
  "buildWfInitUrlOnField:build_wf_init_url_on_field"
  "getOverdueWidgetReports:get_overdue_widget_reports"
  "restoreEdcMultiple:restore_edc_multiple"
  "removeEdcMultiple:remove_edc_multiple"
  "updateEdcMultiple:update_edc_multiple"
  "buildPayloadMultiple:build_payload_multiple"
  "buildUrlMultiple:build_url_multiple"
  "getWidgetReports:get_widget_reports"
  "getConnectedUserDto:get_connected_user_dto"
  "getConnectionToken:get_connection_token"
  "getConnectionRobot:get_connection_robot"
  "createAuthentication:create_authentication"
  "getOverdueWidget:get_overdue_widget"
  "getPublicConfig:get_public_config"
  "getCustomStatus:get_custom_status"
  "getInstanceUrl:get_instance_url"
  "createAtHeader:create_at_header"
  "buildWfPayload:build_wf_payload"
  "recursiveApply:recursive_apply"
  "retrieveTypes:retrieve_types"
  "getTransfers:get_transfers"
  "buildPayload:build_payload"
  "getExtracts:get_extracts"
  "getReports:get_reports"
  "logoutUser:logout_user"
  "sendWfPut:send_wf_put"
  "buildWfUrl:build_wf_url"
  "getExtract:get_extract"
  "getWidget:get_widget"
  "getEvents:get_events"
  "getReport:get_report"
  "getConfig:get_config"
  "updateEdc:update_edc"
  "buildUrl:build_url"
  "updateWf:update_wf"
  "addScope:add_scope"
  "sendPost:send_post"
  "addUser:add_user"
  "sendPut:send_put"
  "reportMetricsDiffExcel:report_metrics_diff_excel"
  "getPreviousReportFolder:get_previous_report_folder"
  "transformUnknownDates:transform_unknown_dates"
  "findStartDateAfterEndDate:find_start_date_after_end_date"
  "findAttributeNotResetInv:find_attribute_not_reset_inv"
  "findAttributeNotReset:find_attribute_not_reset"
  "buildOutputFilename:build_output_filename"
  "checkAndCreatePath:check_and_create_path"
  "partialdateToDate:partial_date_to_date"
  "getSubfieldValues:get_subfield_values"
  "printTableToCsv:print_table_to_csv"
  "reportFindings:report_findings"
  "getClosestEvent:get_closest_event"
  "getCenterOnDate:get_center_on_date"
  "addParentScope:add_parent_scope"
  "periodsOverlap:periods_overlap"
  "safeAggregate:safe_aggregate"
  "datesInBounds:dates_in_bounds"
  "moveElements:move_elements"
  "revertDf:revert_df"
)

# ---------- Processing ----------
# Collect R files (*.R and *.r)
mapfile -t R_FILES < <(find "$TARGET_DIR" -type f \( -name '*.R' -o -name '*.r' \) | sort)

if [[ ${#R_FILES[@]} -eq 0 ]]; then
  echo "No .R files found in '$TARGET_DIR'."
  exit 0
fi

echo "Scanning ${#R_FILES[@]} R file(s) in '$TARGET_DIR' ..."
TOTAL_CHANGES=0

for file in "${R_FILES[@]}"; do
  file_changes=0
  for entry in "${RENAMES[@]}"; do
    old="${entry%%:*}"
    new="${entry#*:}"
    # Use word-boundary matching (\b) to avoid replacing inside longer identifiers
    count=$(grep -oP "\b${old}\b" "$file" 2>/dev/null | wc -l || true)
    if [[ "$count" -gt 0 ]]; then
      file_changes=$((file_changes + count))
      if [[ "$DRY_RUN" == true ]]; then
        echo "  [dry-run] $file: $old -> $new ($count occurrence(s))"
      else
        sed -i "s/\\<${old}\\>/${new}/g" "$file"
      fi
    fi
  done

  if [[ "$file_changes" -gt 0 ]]; then
    TOTAL_CHANGES=$((TOTAL_CHANGES + file_changes))
    if [[ "$DRY_RUN" == false ]]; then
      echo "  Updated: $file ($file_changes replacement(s))"
    fi
  fi
done

if [[ "$TOTAL_CHANGES" -eq 0 ]]; then
  echo "No camelCase function calls found. Nothing to do."
else
  if [[ "$DRY_RUN" == true ]]; then
    echo ""
    echo "Dry run complete. $TOTAL_CHANGES replacement(s) would be made."
    echo "Re-run without --dry-run to apply."
  else
    echo ""
    echo "Done. $TOTAL_CHANGES replacement(s) applied."
  fi
fi
