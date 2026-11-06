# LTV Household Data Cleaning Pipeline

## Quick Start

1. **Setup** (run once):
   ```r
   source("00_setup.R")
   ```

2. **Check Data**:
   ```r
   source("01_checking_SIMPLE.R")
   ```
   - Creates `outputs/cleaning_log.xlsx`
   - Review it and save as `outputs/cleaning_log_reviewed.xlsx`

3. **Edit Data**:
   ```r
   source("02_editing_SIMPLE.R")
   ```
   - Reads `cleaning_log_reviewed.xlsx`
   - Creates clean data in `data/clean/`

## Files
- `00_setup.R` - Install packages (run once)
- `01_checking_SIMPLE.R` - Find data issues  
- `02_editing_SIMPLE.R` - Apply corrections
- `resources/logical_checks.xlsx` - Validation rules
- `data/raw/` - Put your raw Excel file here
- `data/clean/` - Clean data output
- `outputs/` - Cleaning logs

## What Gets Checked
- Duplicate UUIDs
- Interview duration (10-180 minutes)
- Logical consistency (12 checks from logical_checks.xlsx)

## Notes
- Raw data is never modified
- All changes require human review
- Simple, direct code - no bloat
