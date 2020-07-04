# pzfx 0.1.0
## New features
* Initial CRAN release. See the documentation at https://yue-jiang.github.io/pzfx/.

# pzfx 0.2.0
## New features
* Added support for writing pzfx files. See `?write_pzfx` for details.

## Bug fixes
* 'HugeTable' in '.pzfx' files is now respected. 'HugeTable' is used for tables with more than 104 data sets and/or 52 subcolumns by 'GraphPad Prism'.

# pzfx 0.3.0
## New features
* Added an option 'date_x' to specify whether to import date X columns as character string of dates or as years from the first time point.
