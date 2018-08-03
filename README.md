# pzfx
Read Prism Graphpad .pzfx files into R. It tries to understand different .pzfx table formats, e.g. replicates as subcolumns, or mean-sd-n as subcolumns etc and adds proper suffix to column names of the parsed output.

# Installation

```
# Install development version from GitHub
devtools::install_github("Yue-Jiang/pzfx")
```

# Usage

To list tables in a .pzfx file
```
library(pzfx)
pzfx_tables("/path/to/my/pzfx/file")
```

To read a specific table from a .pzfx file
```
df <- read_pzfx("/path/to/my/pzfx/file", 1) # read first table
df <- read_pzfx("/path/to/my/pzfx/file", "Table Name") # read table by name
```
