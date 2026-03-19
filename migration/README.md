# migrate\_to\_snake\_case.sh

Bash script that renames old **camelCase** function calls from the Rodano R SDK to their current **snake\_case** equivalents in user R scripts.

## Usage

```bash
# Preview changes (no files modified)
./migrate_to_snake_case.sh --dry-run <target_directory>

# Apply changes
./migrate_to_snake_case.sh <target_directory>
```

## Examples

```bash
# Dry-run against a study's R scripts
./migrate_to_snake_case.sh --dry-run ~/projects/my_study/R

# Apply the migration
./migrate_to_snake_case.sh ~/projects/my_study/R
```

## What it does

1. Recursively finds all `.R` / `.r` files under the target directory.
2. For each file, replaces every known camelCase function name with its snake\_case equivalent using Perl word-boundary matching (`\b`) to avoid partial replacements.
3. Reports per-file replacement counts.

## Covered functions

| Source file | Example renames |
|---|---|
| `rodano_api_communication.R` | `getConnectionToken` → `get_connection_token`, `buildUrl` → `build_url`, `sendPut` → `send_put`, … |
| `report_helper.R` | `safeAggregate` → `safe_aggregate`, `reportFindings` → `report_findings`, `checkAndCreatePath` → `check_and_create_path`, … |

The full mapping is defined in the `RENAMES` array inside the script.

## Options

| Flag | Description |
|---|---|
| `--dry-run` | Show what would change without modifying any files |

## Requirements

- Bash 4+ (for `mapfile`)
- GNU `sed` and GNU `grep` with Perl regex support (`-P`)
