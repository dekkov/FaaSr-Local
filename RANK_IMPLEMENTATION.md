# Rank Function Implementation for FaaSr Local Testing

## Overview

This implementation adds rank functionality to the local FaaSr testing framework (`faasr_test`), mirroring the behavior of the FaaSr package.

## What is Rank?

Rank allows you to invoke a function multiple times in parallel. In the `InvokeNext` field, you can specify a number in parentheses to indicate how many times a function should be executed.

### Syntax

```json
"InvokeNext": ["function_name(3)"]
```

This will invoke `function_name` 3 times with ranks 1/3, 2/3, and 3/3.

## Implementation Details

### 1. Modified Files

#### `R/faasr_test.R`
- Added parsing function `.faasr_parse_invoke_next_string()` to extract rank from `InvokeNext` strings
- Modified queue processing to handle rank as structured items with `name`, `rank_current`, and `rank_max`
- Set global variable `FAASR_CURRENT_RANK_INFO` before each function execution
- Functions are executed multiple times based on rank specification

#### `R/faasr_apis.R`
- Added `faasr_rank()` function to allow user functions to query their current rank
- Returns a list with `Rank` and `MaxRank` fields, or empty list if not running in ranked mode

### 2. Usage in User Functions

```r
my_function <- function(message) {
  # Get rank information
  rank_info <- faasr_rank()
  
  if (length(rank_info) > 0) {
    # Running with rank
    cat(sprintf("Rank: %s/%s\n", rank_info$Rank, rank_info$MaxRank))
    # Do work specific to this rank...
  } else {
    # Running without rank (single execution)
    cat("Single execution\n")
  }
  
  return(TRUE)
}
```

### 3. Example Workflow

See `faasr_data/test_rank.json` for a complete example:

```json
{
  "FunctionInvoke": "start_function",
  "ActionList": {
    "start_function": {
      "FunctionName": "test_rank_function",
      "InvokeNext": ["ranked_function(3)"]
    },
    "ranked_function": {
      "FunctionName": "test_rank_function",
      "InvokeNext": ["final_function"]
    },
    "final_function": {
      "FunctionName": "test_rank_function",
      "InvokeNext": []
    }
  }
}
```

This workflow will:
1. Execute `start_function` once (no rank)
2. Execute `ranked_function` three times (ranks 1/3, 2/3, 3/3)
3. Execute `final_function` once (no rank)

### 4. Key Features

- **Parallel Simulation**: In local testing, ranked functions run sequentially but with proper rank information
- **Conditional Support**: Rank notation works with conditional triggers like `"function[TRUE](3)"`
- **Predecessor Gating**: The `.done` file is only written after the last rank completes
- **Unique Execution Tracking**: Each rank execution is tracked separately to avoid duplicates

### 5. Testing

To test the rank functionality:

```r
source("R/faasr_test.R")
source("R/faasr_apis.R")
faasr_test("faasr_data/test_rank.json")
```

Expected output:
- `start_function` runs once with no rank info
- `ranked_function` runs 3 times with ranks 1/3, 2/3, and 3/3
- `final_function` runs once with no rank info

## Differences from FaaSr Package

- **Execution Mode**: Local testing runs sequentially; FaaSr package triggers parallel cloud executions
- **Global State**: Local version uses `FAASR_CURRENT_RANK_INFO` environment variable; package uses `faasr$FunctionList[[func]]$Rank`
- **Timing**: Local execution is immediate and sequential; cloud execution is asynchronous

## Notes

- Rank numbers start at 1 (not 0)
- The maximum rank is the number specified in parentheses
- Without rank notation, functions execute once with no rank information
- Rank information is accessible via `faasr_rank()` during function execution

