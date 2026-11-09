# LTV Household Data Cleaning Pipeline

Streamlined R-based pipeline for cleaning household survey data using validation rules and human-reviewed corrections.

## Quick Start

1. **Install dependencies** (once):
   ```r
   source("00_setup.R")
   ```

2. **Check data quality**:
   ```r
   source("01_data_checking.R")
   ```
   - Generates `outputs/cleaning_log.xlsx`
   - Review all flagged issues and make decisions
   - Save as `outputs/cleaning_log_reviewed.xlsx`

3. **Apply corrections**:
   ```r
   source("02_data_editing.R")
   ```
   - Reads reviewed log and applies changes
   - Outputs clean data to `data/clean/`

## Directory Structure

```
LTV_cleaning_project/
├── 00_setup.R                  # Package installation
├── 01_data_checking.R          # Quality checks
├── 02_data_editing.R           # Apply corrections
├── utils/
│   └── helpers.R               # Shared functions
├── data/
│   ├── raw/                    # Original data (never modified)
│   └── clean/                  # Clean outputs
├── resources/
│   └── logical_checks.xlsx     # Validation rules
└── outputs/                    # Logs and reports
```

## Validation Checks

The pipeline runs four types of validation:

1. **Duplicate Detection** - Flags duplicate Kobo UUIDs
2. **Duration Checks** - Interview time 10-180 minutes
3. **Logical Rules** - 12 custom checks from `logical_checks.xlsx`
4. **Outlier Detection** - IQR-based statistical outliers (3× threshold)

## Data Flow

```
Raw Excel → Check → Review Log → Edit → Clean Data
                ↓                   ↓
           cleaning_log.xlsx    clean outputs
```

## Key Features

- **Immutable raw data** - Never modifies original files
- **Human review required** - No automatic corrections
- **Full audit trail** - Every change logged
- **Dual export** - RDS (R native) and XLSX (portable)
- **Select-multiple handling** - Reconstructs parent fields when children are edited

## Change Types

In the cleaning log review, use these values for `change_type`:

- `no_action` - Flag noted, no correction needed
- `change_response` - Correct value (requires `new_value`)
- `blank_response` - Set to NA/missing
- `remove_survey` - Delete entire record

---

## Technical Details

### Datetime Conversion

The pipeline handles multiple datetime formats:
- Excel serial numbers (numeric 1-100,000)
- ISO 8601 (YYYY-MM-DD HH:MM:SS)
- Common variations (d/m/Y, m/d/Y, with/without timezone)

All conversions default to UTC timezone.

### UUID Handling

Training data uses `_id` as working UUID (not `_uuid`) because the dataset intentionally contains duplicate/missing `_uuid` values for training purposes. Production data should use Kobo's `_uuid`.

### Dependencies

Core packages: `dplyr`, `tidyr`, `readxl`, `openxlsx`, `readr`, `here`, `cleaningtools` (IMPACT Initiatives)

### Outlier Detection

Uses Tukey's fences with strongness factor 3:
- Lower bound: Q1 - 3×IQR
- Upper bound: Q3 + 3×IQR
- Requires ≥5 unique values per variable

Only flags outliers for review - does not automatically remove them.

### Performance

Typical runtime on ~200 records: <30 seconds total

### Error Handling

Pipeline fails fast with descriptive errors:
- Missing required files
- Invalid change types
- Conflicting cleaning decisions
- Unknown UUIDs or questions in cleaning log

---

**Note:** This pipeline follows IMPACT Initiatives standards for humanitarian data cleaning workflows.