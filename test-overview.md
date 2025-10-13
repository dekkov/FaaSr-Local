---
title: "FaaSr Local Testing - Overview"
permalink: /docs/faasr-local-testing/
excerpt: "Test FaaSr workflows locally without cloud infrastructure. Simple, fast, and reliable local execution environment."
last_modified_at: 2024-01-15T10:00:00-00:00
toc: true
toc_label: "Overview"
toc_icon: "play"
---

# FaaSr Local Testing

## What is faasr_test()?

`faasr_test()` is a local testing environment that lets you run FaaSr workflows on your own machine without needing cloud infrastructure. It's perfect for development, debugging, and testing workflows before deploying them.

## Quick Start

```r
# Run a simple workflow
faasr_test("my_workflow.json")
```

That's it! The function will:
- Load your workflow JSON
- Execute all functions in the correct order
- Handle Parallel rank execution
- Show progress in the console

## Why Use Local Testing?

- **🚀 Fast**: No network delays or cloud setup
- **🔧 Debug**: Easy to debug with R's built-in tools
- **💰 Free**: No cloud costs during development
- **🔒 Private**: Keep your data and code local
- **⚡ Iterate**: Quick feedback loop for development

## Key Features

| Feature | Description |
|---------|-------------|
| **Local Execution** | Run workflows entirely on your machine |
| **Dependency Management** | Automatically handles function dependencies |
| **Parallel Rank Support** | Execute multiple ranks with `FunctionName(3)` |
| **Conditional Branching** | Support for `{True: [...], False: [...]}` logic |
| **Clean State** | Fresh execution every time |
| **Progress Tracking** | Real-time console output |

## Basic Example

```r
# 1. Create a simple workflow JSON
workflow <- '{
  "FunctionInvoke": "start",
  "ActionList": {
    "start": {
      "FunctionName": "my_function",
      "InvokeNext": ["end"]
    },
    "end": {
      "FunctionName": "cleanup"
    }
  }
}'

# 2. Save it
writeLines(workflow, "simple_workflow.json")

# 3. Run it
faasr_test("simple_workflow.json")
```

## Directory Structure

The function creates a local filesystem structure:

```
faasr_data/
├── files/          # Shared data files
├── logs/           # Execution logs
├── temp/           # Temporary execution directories
└── R/              # Your custom functions
```

## Advanced Features

### Parallel Rank Execution
```json
{
  "InvokeNext": ["processData(3)"]
}
```
This creates 3 parallel instances of `processData`.

### Conditional Branching
```json
{
  "InvokeNext": [{
    "True": ["successHandler"],
    "False": ["errorHandler"]
  }]
}
```
Functions return `TRUE` or `FALSE` to control branching.

### Custom Data Root
```r
Sys.setenv(FAASR_DATA_ROOT = "/path/to/custom/data")
faasr_test("workflow.json")
```

## What Happens During Execution?

1. **Load & Validate**: Parse JSON and check for cycles
2. **Source Functions**: Load your R functions from `faasr_data/R/`
3. **Execute Queue**: Run functions in dependency order
4. **Handle Dependencies**: Wait for predecessors to complete
5. **Clean Up**: Remove temporary files

> **For detailed technical information** about algorithms, cycle detection, and implementation details, see [algorithms.md](algorithms.md).
{: .notice--info}
