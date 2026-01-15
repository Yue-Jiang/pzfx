# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

pzfx is an R package for reading and writing GraphPad Prism `.pzfx` files. It parses XML-based `.pzfx` files and handles various table formats (replicates, mean-sd-n, etc.) with appropriate column naming.

## Build and Test Commands

```bash
# Check package (runs R CMD check)
R CMD check .

# Run tests
R -e "testthat::test_local()"

# Run a single test file
R -e "testthat::test_file('tests/testthat/test_read_pzfx.R')"

# Build documentation (requires roxygen2)
R -e "roxygen2::roxygenise()"

# Install package locally for testing
R -e "devtools::install()"
```

## Architecture

### Core Functions (R/)

- **pzfx_tables.R** - Lists table names in a `.pzfx` file using XPath queries
- **read_pzfx.R** - Main entry point for reading; handles table lookup, X/Y format detection, and column assembly
- **write_pzfx.R** - Writes data frames/matrices to `.pzfx` format; builds XML structure with proper Prism formatting
- **read_col.R** - Internal: parses one column, determines subcolumn naming based on format (replicates, SDN, SEN, CVN, etc.)
- **read_subcol.R** - Internal: parses individual subcolumns, handles "Excluded" (stricken) values and decimal separators

### Data Flow

Reading: XML → `xml2::as_list()` → iterate columns → `read_col()` → `read_subcol()` for each subcolumn → assemble data frame

Writing: data frame → build nested list structure → `xml2::as_xml_document()` → write XML

### Key Implementation Details

- Uses `xml2` package for all XML operations
- Supports both `<Table>` and `<HugeTable>` nodes (same structure, different tags)
- Column formats are detected via `XFormat`/`YFormat` attributes and determine suffix naming (_MEAN, _SD, _N, etc.)
- Stricken values in Prism can be excluded, kept, or marked with trailing "*" via `strike_action` parameter
- Handles comma as decimal separator (European locales)
- Date X columns can be returned as numeric (elapsed time), character (date string), or both
