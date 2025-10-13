---
title: "FaaSr Algorithms - Deep Dive"
permalink: /docs/faasr-algorithms/
excerpt: "Detailed explanations of the complex algorithms in faasr_test.R including DFS cycle detection, predecessor gating, and parsing logic."
last_modified_at: 2024-01-15T10:00:00-00:00
toc: true
toc_label: "Algorithms"
toc_icon: "cogs"
---

# Algorithms Explained — Deep Dive into faasr_test.R

This document provides detailed explanations of the complex algorithms in `faasr_test.R`. For the main code walkthrough, see [overview.md](overview.md).

> **Note:** This document assumes familiarity with the basic workflow execution concepts covered in the main walkthrough.
{: .notice--info}

## Table of Contents

1. [Main Execution Loop](#main-execution-loop)
2. [Parsing InvokeNext Strings](#parsing-invokenext-strings)
3. [Predecessor Gating](#predecessor-gating)
4. [Cycle Detection (DFS)](#cycle-detection)

---

## Main Execution Loop

The main while loop is the workflow engine that processes functions from the queue one by one.

### Step 1: Dequeue and Check Visited
```r
queue_item <- queue[[1]]; queue <- queue[-1]
name <- queue_item$name
rank_current <- queue_item$rank_current
rank_max <- queue_item$rank_max

visited_key <- paste0(name, "_rank_", rank_current, "/", rank_max)
if (visited_key %in% visited) next
```

**What's happening:**
- **Pop from queue**: `queue[[1]]` gets the first item, `queue[-1]` creates a new list without it
- **Extract rank info**: Each queue item has `name` (e.g., "funcA"), `rank_current` (1), and `rank_max` (3)
- **Visited tracking**: Creates unique key like `"funcA_rank_1/3"` to prevent re-execution
- **Why check visited?** Multiple predecessors can enqueue the same function multiple times

### Step 2: Predecessor Gating
```r
cfg <- faasr_predecessor_gate(wf$ActionList, name, state_dir)
if (identical(cfg, "next")) {
  next
}
```

**What's happening:**
- Checks if all **unconditional** predecessors have completed (`.done` files exist)
- Returns `"next"` if not ready → skip and continue loop (function stays in queue for retry)
- Returns `TRUE` if ready → proceed with execution
- This handles the "wait until all parents are done" synchronization

### Step 3: Set Rank Context
```r
if (rank_max > 1) {
  rank_info <- paste0(rank_current, "/", rank_max)
  assign("FAASR_CURRENT_RANK_INFO", rank_info, envir = .GlobalEnv)
  cli::cli_h2(sprintf("Running %s (%s) - Rank %s", name, func_name, rank_info))
} else {
  assign("FAASR_CURRENT_RANK_INFO", NULL, envir = .GlobalEnv)
  cli::cli_h2(sprintf("Running %s (%s)", name, func_name))
}
```

**What's happening:**
- If `rank_max > 1`, this is a parallel function with multiple ranks
- Sets global variable so user code can call `faasr_rank()` to know which rank it is
- Displays rank in console output (e.g., "Running funcA (my_func) - Rank 2/3")

### Step 4: Isolated Execution Environment
```r
run_dir <- file.path(temp_dir, name)
if (!dir.exists(run_dir)) dir.create(run_dir, recursive = TRUE)
orig_wd <- getwd()
on.exit(setwd(orig_wd), add = TRUE)
Sys.setenv(FAASR_DATA_ROOT = faasr_data_wd)
setwd(run_dir)
```

**What's happening:**
- Each function gets its own working directory: `faasr_data/temp/funcA/`
- `on.exit` ensures we return to original directory even if function fails
- `FAASR_DATA_ROOT` tells APIs where to find shared files
- Isolation prevents functions from interfering with each other's temp files

### Step 5: Execute and Capture Result
```r
f <- get(func_name, envir = .GlobalEnv)
res <- try(do.call(f, args), silent = TRUE)
if (inherits(res, "try-error")) {
  cli::cli_alert_danger(as.character(res))
  stop(sprintf("Error executing %s", func_name))
}
```

**What's happening:**
- `get(func_name, ...)` retrieves the actual R function by name
- `do.call(f, args)` calls `f(arg1=val1, arg2=val2, ...)` dynamically
- `try(..., silent=TRUE)` catches errors without stopping the script
- `res` contains the return value (needed for conditional branching)

### Step 6: Mark Completion
```r
if (rank_current == rank_max) {
  utils::write.table("TRUE", file = file.path(state_dir, paste0(name, ".done")), ...)
}
visited <- c(visited, visited_key)
```

**What's happening:**
- `.done` file signals "this function is complete" for predecessor gating
- Only write `.done` after the **final rank** completes (not after each rank)
- Add to `visited` to prevent re-execution of this specific rank

### Step 7: Enqueue Successors (Conditional Branching)
```r
if (rank_current == rank_max) {
  nexts <- node$InvokeNext %||% list()
  if (is.character(nexts)) nexts <- as.list(nexts)
  for (nx in nexts) {
    if (is.character(nx)) {
      # Simple case: "funcB" or "funcB(3)"
      parsed <- .faasr_parse_invoke_next_string(nx)
      for (r in 1:parsed$rank) {
        queue <- c(queue, list(list(name = parsed$func_name, rank_current = r, rank_max = parsed$rank)))
      }
    } else if (is.list(nx)) {
      # Conditional case: {True: [...], False: [...]}
      branch <- if (isTRUE(res) && !is.null(nx$True)) nx$True 
                else if (identical(res, FALSE) && !is.null(nx$False)) nx$False 
                else NULL
      if (is.null(branch)) next
      for (b in branch) {
        parsed <- .faasr_parse_invoke_next_string(b)
        for (r in 1:parsed$rank) {
          queue <- c(queue, list(list(name = parsed$func_name, rank_current = r, rank_max = parsed$rank)))
        }
      }
    }
  }
}
```

**What's happening:**
- **Only enqueue from final rank** to avoid duplicates (rank 1 shouldn't enqueue, rank 3 does)
- **String case**: Direct successor like `"funcB(3)"` → enqueue 3 ranks
- **Conditional case**: Check function result `res`:
  - If `res` is exactly `TRUE` → take `True` branch
  - If `res` is exactly `FALSE` → take `False` branch
  - Otherwise → skip (no branching)
- **Rank expansion**: If successor is `"funcB(3)"`, enqueue 3 separate items: `(funcB, 1, 3)`, `(funcB, 2, 3)`, `(funcB, 3, 3)`

---

## Parsing InvokeNext Strings

The parser extracts function name and rank from strings like `"FuncName"` or `"FuncName(3)"`.

### Step 1: Trim and Validate Format
```r
s <- trimws(invoke_string)

if (grepl("\\[|\\]", s)) {
  stop("Invalid InvokeNext format: only 'FuncName' and 'FuncName(N)' are supported")
}
```

**What's happening:**
- `trimws()` removes leading/trailing whitespace
- `grepl("\\[|\\]", s)` checks for square brackets `[` or `]`
- Rejects old formats like `"FuncName[TRUE]"` or `"FuncName[FALSE]"`
- **Why reject brackets?** Conditional branching is now handled via `{True: [...], False: [...]}` objects, not inline notation

### Step 2: Parse with Regex
```r
m <- regexec("^(.*?)(?:\\((\\d+)\\))?$", s)
mm_all <- regmatches(s, m)
if (!length(mm_all) || length(mm_all[[1]]) != 3) {
  stop("Invalid InvokeNext format")
}
mm <- mm_all[[1]]
```

**Regex breakdown**: `^(.*?)(?:\\((\\d+)\\))?$`
- `^(.*?)` — Capture group 1: function name (non-greedy, matches any chars)
- `(?:\\((\\d+)\\))?` — Optional non-capturing group:
  - `\\(` — Literal opening parenthesis
  - `(\\d+)` — Capture group 2: one or more digits (the rank number)
  - `\\)` — Literal closing parenthesis
  - `?` — Makes the entire parentheses part optional
- `$` — End of string

**How `regexec()` and `regmatches()` work:**
- `regexec()` finds matches and returns positions
- `regmatches()` extracts the matched strings
- **Result `mm`**: Always a 3-element vector:
  - `mm[1]` = full match (entire string)
  - `mm[2]` = capture group 1 (function name)
  - `mm[3]` = capture group 2 (rank number, or empty string if not present)

**Examples:**
- `"myFunc"` → `mm = c("myFunc", "myFunc", "")`
- `"myFunc(5)"` → `mm = c("myFunc(5)", "myFunc", "5")`
- `"myFunc()"` → Error (no digits between parentheses)

### Step 3: Extract Function Name
```r
func_name <- trimws(mm[2])
if (!nzchar(func_name)) {
  stop("Invalid InvokeNext: empty function name")
}
```

**What's happening:**
- `mm[2]` is the captured function name
- `nzchar()` checks if string has non-zero length
- Catches edge cases like `""` or `"   "` (whitespace only)

### Step 4: Extract Rank (Default to 1)
```r
rank <- 1
if (nzchar(mm[3])) {
  rank <- as.integer(mm[3])
}
```

**What's happening:**
- If `mm[3]` is empty (no parentheses in input) → rank stays 1
- If `mm[3]` has digits (e.g., "5") → convert to integer
- `as.integer("5")` returns `5L` (integer type in R)

### Step 5: Return Parsed Result
```r
list(func_name = func_name, condition = NULL, rank = rank)
```

**What's returned:**
- `func_name`: string like `"myFunc"`
- `condition`: always `NULL` (legacy field, no longer used)
- `rank`: integer like `1` or `5`
- Caller uses these to enqueue the right number of queue items

---

## Predecessor Gating

Predecessor gating ensures functions with multiple parents wait for all unconditional parents to complete.

### The Problem

Consider this workflow:

```
```
funcA --True--> funcB
  |
  +----False--> funcC
  
funcB ---------> funcC
```

- `funcA` can branch to `funcB` (True) or `funcC` (False)
- `funcB` unconditionally invokes `funcC`
- `funcC` has TWO predecessors: `funcA` (conditional) and `funcB` (unconditional)

**If `funcA` returns FALSE:**

- Only `funcC` should run (via False branch)
- `funcB` never runs
- But naive gating would wait for BOTH `funcA.done` AND `funcB.done`
- **Deadlock!** `funcC` waits forever for `funcB` which will never execute

**Solution: Only gate on unconditional predecessors**
{: .notice--success}

### `.faasr_find_predecessors()` — The Discovery Phase

```r
.faasr_find_predecessors <- function(action_list, target_func, unconditional_only = FALSE) {
  preds <- character()
  for (nm in names(action_list)) {
    nx <- action_list[[nm]]$InvokeNext %||% list()
    next_names <- character()
```

The function loops through every action in the workflow and checks if it invokes `target_func`.

#### Case 1: String InvokeNext (Always Unconditional)
```r
if (is.character(nx)) {
  next_names <- sub("\\(.*$", "", nx)
}
```

**Example:** `InvokeNext: ["funcB", "funcC(3)"]`
- `sub("\\(.*$", "", "funcC(3)")` removes `(3)` → `"funcC"`
- Result: `next_names = c("funcB", "funcC")`
- These are **unconditional** successors

#### Case 2: List InvokeNext (May Contain Conditionals)
```r
} else if (is.list(nx)) {
  for (item in nx) {
    if (is.character(item)) {
      next_names <- c(next_names, sub("\\(.*$", "", item))
    } else if (is.list(item)) {
      # This is a conditional branch
      if (!unconditional_only) {
        if (!is.null(item$True)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
        if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
      }
    }
  }
}
```

**Example:** `InvokeNext: ["funcD", {True: ["funcE"], False: ["funcF"]}]`
- Processes two items:
  1. `"funcD"` (string) → adds to `next_names` (unconditional)
  2. `{True: ..., False: ...}` (list) → conditional branch
- **If `unconditional_only = TRUE`**: Skip conditional branches entirely
- **If `unconditional_only = FALSE`**: Include both True and False targets

#### Checking if This Node is a Predecessor
```r
if (length(next_names) && any(next_names == target_func)) {
  preds <- c(preds, nm)
}
```

If any of the successors match `target_func`, add this node `nm` to predecessors list.

#### Example Walkthrough:
```r
# Workflow:
ActionList = {
  funcA: { InvokeNext: [{True: ["funcB"], False: ["funcC"]}] }
  funcB: { InvokeNext: ["funcC"] }
  funcC: { InvokeNext: [] }
}

# Find predecessors of funcC:
.faasr_find_predecessors(ActionList, "funcC", unconditional_only = FALSE)
# Returns: c("funcA", "funcB")  — both paths

.faasr_find_predecessors(ActionList, "funcC", unconditional_only = TRUE)
# Returns: c("funcB")  — only unconditional path
```

### `faasr_predecessor_gate()` — The Enforcement Phase

```r
faasr_predecessor_gate <- function(action_list, current_func, state_dir) {
  # Only gate on unconditional predecessors to avoid blocking on unexecuted conditional branches
  predecessors <- .faasr_find_predecessors(action_list, current_func, unconditional_only = TRUE)
  if (length(predecessors) > 1) {
    done_list <- try(list.files(state_dir), silent = TRUE)
    if (inherits(done_list, "try-error")) done_list <- character()
    for (p in predecessors) {
      if (!(paste0(p, ".done") %in% done_list)) {
        return("next")
      }
    }
  }
  TRUE
}
```

#### Step-by-step Logic:

**1. Find Unconditional Predecessors Only**
```r
predecessors <- .faasr_find_predecessors(action_list, current_func, unconditional_only = TRUE)
```
- Ignores conditional branches to avoid the deadlock scenario

**2. Check if Multiple Predecessors Exist**
```r
if (length(predecessors) > 1) {
```
- If 0 or 1 predecessor → no gating needed (single parent or entry point)
- If 2+ predecessors → need to wait for all to complete

**3. Read .done Files from State Directory**
```r
done_list <- try(list.files(state_dir), silent = TRUE)
```
- Example: `c("funcA.done", "funcB.done")`
- These marker files indicate completed functions

**4. Check Each Predecessor**
```r
for (p in predecessors) {
  if (!(paste0(p, ".done") %in% done_list)) {
    return("next")
  }
}
```
- For each predecessor, check if its `.done` file exists
- If any `.done` file is missing → return `"next"` (skip execution, keep in queue)
- If all `.done` files exist → fall through to `TRUE`

**5. Return TRUE (Ready to Execute)**
```r
TRUE
```
- All unconditional predecessors have completed
- Safe to proceed with execution

### Why This Works:
- Functions in conditional branches aren't required for gating
- Only "guaranteed" predecessors block execution
- Conditional logic handles branch selection separately
- No deadlocks from unexecuted branches

---

## Cycle Detection

Cycle detection uses **Depth-First Search (DFS)** to detect cycles in the workflow graph.

### What is a Cycle?

A cycle occurs when you can follow `InvokeNext` links and return to a node you've already visited:

```
```
funcA → funcB → funcC → funcB  (CYCLE! funcB appears twice)
```

**Why detect cycles?**

- Cyclic workflows would loop forever
- Must validate before execution starts
- Better to fail fast with clear error than hang indefinitely

### `.faasr_build_adjacency()` — Convert Workflow to Graph

```r
.faasr_build_adjacency <- function(action_list) {
  adj <- list()
  for (nm in names(action_list)) {
    nx <- action_list[[nm]]$InvokeNext %||% list()
    next_names <- character()
    if (is.character(nx)) {
      next_names <- sub("\\(.*$", "", nx)
    } else if (is.list(nx)) {
      for (item in nx) {
        if (is.character(item)) {
          next_names <- c(next_names, sub("\\(.*$", "", item))
        } else if (is.list(item)) {
          if (!is.null(item$True)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$True)))
          if (!is.null(item$False)) next_names <- c(next_names, sub("\\(.*$", "", unlist(item$False)))
        }
      }
    }
    if (length(next_names)) adj[[nm]] <- unique(next_names)
  }
  adj
}
```

Creates an adjacency list representation of the workflow graph.

**Example:**
```r
# Workflow:
ActionList = {
  funcA: { InvokeNext: ["funcB", "funcC(3)"] }
  funcB: { InvokeNext: [{True: ["funcD"], False: ["funcE"]}] }
  funcC: { InvokeNext: [] }
}

# Result adjacency list:
adj = list(
  funcA = c("funcB", "funcC"),
  funcB = c("funcD", "funcE"),
  funcC = character(0)
)
```

This creates a simple graph: `adj["funcA"]` tells you which functions `funcA` calls.

### `.faasr_check_workflow_cycle_local()` — The Main DFS Algorithm

```r
.faasr_check_workflow_cycle_local <- function(faasr, start_node) {
  action_list <- faasr$ActionList
  adj <- .faasr_build_adjacency(action_list)
  visited <- new.env(parent = emptyenv())
  stack <- new.env(parent = emptyenv())
```

**Setup:**
- `adj`: Graph representation (who calls whom)
- `visited`: Tracks nodes we've explored (persists across recursion)
- `stack`: Tracks current recursion path (nodes currently being explored)

**Why two tracking structures?**

- `visited`: "Have we checked this node at all?" (global memory)
- `stack`: "Are we currently inside this node's subtree?" (recursion depth)

### The Recursive `is_cyclic()` Function

```r
is_cyclic <- function(node) {
  if (!(node %in% names(action_list))) {
    stop(sprintf("invalid function trigger: %s", node))
  }
```

Validates the node exists in the workflow. Catches typos in `InvokeNext` (e.g., calling non-existent `"funcZ"`).

#### Step 1: Detect Back Edge (Cycle)
```r
  if (isTRUE(get0(node, envir = stack, inherits = FALSE, ifnotfound = FALSE))) {
    return(TRUE)
  }
```

- Check if `node` is already in the current recursion `stack`
- If yes → **CYCLE DETECTED!** We've returned to a node we're currently exploring
- Think of it as: "If I'm already inside funcA's exploration, and funcA appears again, that's a loop"

#### Step 2: Mark Node as Being Explored
```r
  assign(node, TRUE, envir = stack)
  assign(node, TRUE, envir = visited)
```

- Add to `stack`: "We're now exploring this node's children"
- Add to `visited`: "We've seen this node (don't need to check it again later)"

#### Step 3: Recursively Check All Children
```r
  nbrs <- adj[[node]]
  if (!is.null(nbrs) && length(nbrs)) {
    for (nbr in nbrs) {
      if (!isTRUE(get0(nbr, envir = visited, inherits = FALSE, ifnotfound = FALSE))) {
        if (is_cyclic(nbr)) return(TRUE)
      } else if (isTRUE(get0(nbr, envir = stack, inherits = FALSE, ifnotfound = FALSE))) {
        return(TRUE)
      }
    }
  }
```

This is the heart of DFS! For each neighbor:

**Case A: Never Visited Before**
```r
if (!isTRUE(get0(nbr, envir = visited, ...))) {
  if (is_cyclic(nbr)) return(TRUE)
}
```
- If we haven't explored this neighbor yet
- **Recursively check it** (go deeper into the tree)
- If the recursive call finds a cycle, bubble it up

**Case B: Visited but in Current Path**
```r
else if (isTRUE(get0(nbr, envir = stack, ...))) {
  return(TRUE)
}
```
- If neighbor is in `visited` AND in `stack`
- **Back edge detected!** This is a cycle
- Example: `funcA → funcB → funcC → funcA` (funcA is visited AND in stack)

**Case C: Visited but NOT in Current Path**
- If neighbor is in `visited` but NOT in `stack`
- This is a **cross edge** or **forward edge** (already fully explored, no cycle)
- Do nothing (safe to skip)

#### Step 4: Remove from Stack (Backtrack)
```r
  assign(node, FALSE, envir = stack)
  FALSE
}
```

- After exploring all children, mark node as "no longer in current path"
- This is **backtracking**: we've finished exploring this subtree
- `stack` only tracks the **current recursion path**, not all visited nodes

### Visual Example

```
Workflow: A → B → C → D
          A → E
          D → A  (CYCLE!)

DFS Trace:
1. is_cyclic(A): stack = {A}, visited = {A}
2.   is_cyclic(B): stack = {A, B}, visited = {A, B}
3.     is_cyclic(C): stack = {A, B, C}, visited = {A, B, C}
4.       is_cyclic(D): stack = {A, B, C, D}, visited = {A, B, C, D}
5.         Check neighbor A:
            A is in visited ✓
            A is in stack ✓
            → CYCLE DETECTED! Return TRUE
```

### Final Check

```r
if (is_cyclic(start_node)) {
  stop("cycle detected")
}

TRUE
```

- Start DFS from `FunctionInvoke` (entry point)
- If cycle found → error message
- If no cycle → return `TRUE` (validation passed)

### Why This Works

1. **DFS explores all reachable paths** from the start node
2. **`stack` tracks current path** through the graph
3. **Revisiting a node in `stack`** means we've found a loop
4. **`visited` prevents redundant checks** of already-explored subtrees
5. **Backtracking (`stack[node] = FALSE`)** correctly handles branching paths

**Time Complexity:** O(V + E) where V = number of functions, E = number of InvokeNext links

### Edge Cases Handled

- **Self-loops**: `funcA → funcA` (detected immediately when A checks its neighbor A)
- **Indirect cycles**: `A → B → C → A` (detected when C's recursive call reaches A)
- **Multiple entry points**: Only checks reachable nodes from `FunctionInvoke`
- **Conditional branches**: Both True and False paths are checked for cycles
- **Unreachable nodes**: Nodes not reachable from start are not checked (won't execute anyway)

---

## Summary

These algorithms work together to provide:

- **Safe execution**: Cycle detection prevents infinite loops
- **Correct synchronization**: Predecessor gating ensures proper execution order
- **Flexible branching**: Conditional logic supports dynamic workflows
- **Parallel execution**: Rank support enables scalable workflows

For the complete code walkthrough, see [overview.md](overview.md).

