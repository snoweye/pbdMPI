### Do not delete this file and file name!!
### This file should be loaded before all other *.r files.

### This is to avoid the false positive messages from R CMD check.
###   "no visible binding for global variable"
### Suggested by Prof Brian Ripley
### ?globalVariables

if(getRversion() >= "2.15.1"){
  utils::globalVariables(c(".pbd_env"))
}

RTYPE_INTEGER = 1L
RTYPE_DOUBLE = 2L
RTYPE_RAW = 3L
RTYPE_OTHER = 4L
