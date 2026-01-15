[![R-CMD-check](https://github.com/Yue-Jiang/pzfx/workflows/R-CMD-check/badge.svg)](https://github.com/Yue-Jiang/pzfx/actions)
[![codecov](https://codecov.io/gh/Yue-Jiang/pzfx/branch/master/graph/badge.svg)](https://codecov.io/gh/Yue-Jiang/pzfx)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/pzfx)](https://cran.r-project.org/package=pzfx)

# pzfx <img src="man/figures/logo.png" width="100" align="right" />
Read GraphPad Prism `.pzfx` files into R. It tries to understand different `.pzfx` table formats, e.g. replicates as subcolumns, or mean-sd-n as subcolumns etc and adds proper suffix to column names of the parsed output.

# Installation
```
# Install from CRAN
install.packages("pzfx")

# Install development version from GitHub
devtools::install_github("Yue-Jiang/pzfx")
```

# Usage

To list tables in a `.pzfx` file
```
library(pzfx)
pzfx_tables("/path/to/my/pzfx/file")
```

To read a specific table from a `.pzfx` file
```
df <- read_pzfx("/path/to/my/pzfx/file", 1) # read first table
df <- read_pzfx("/path/to/my/pzfx/file", "Table Name") # read table by name
```

To write a data frame or matrix to a `.pzfx` file
```
write_pzfx(df, "/path/to/my/pzfx/file")
```

To write a list of data frames or matrices to a `.pzfx` file
```
write_pzfx(list("Table Name 1" = df1, "Table Name 2" = df2), "/path/to/my/pzfx/file")
```

# Note

A few years after this package was first released, GraphPad has introduced a new file format `.prism` and is more open. If you are interested in working with the new format in R, please check out the [prism2R](https://github.com/Biomiha/prism2R) package by Miha Kosmac.
