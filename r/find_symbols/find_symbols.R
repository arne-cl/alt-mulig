#!/usr/bin/env Rscript

library(argparser)
library(stringr)

find_functions_in_file <- function(filename, function_names) {
  lines <- readLines(filename)
  code <- paste(lines, collapse = "\n")
  expressions <- parse(text = code, keep.source = TRUE)
  
  for (i in seq_along(expressions)) {
    expr <- expressions[[i]]
    if (is.call(expr) && expr[[1]] == "<-" && 
        is.name(expr[[2]]) && as.character(expr[[2]]) %in% function_names) {
      cat(attr(expr, "srcref")[[1]], "\n")
    }
  }
}

find_functions_in_directory <- function(directory, function_names) {
  files <- list.files(directory, pattern = "\\.R$", full.names = TRUE)
  for (file in files) {
    cat("File:", file, "\n")
    find_functions_in_file(file, function_names)
  }
}

cli <- function() {
  parser <- arg_parser("Find symbols in R code and print the code for them.")
  
  parser <- add_argument(parser, "symbols", nargs = 1, help="Symbols to search for in the R code, separated by commas")
  
  parser <- add_argument(parser, c("-f", "--file"), action="store", type="character",
                         help="Files to search")
  
  parser <- add_argument(parser, c("-d", "--directory"), action="store", type="character",
                         help="Directories to search")

  args <- parse_args(parser)
  
  files <- NULL
  directories <- NULL
  
  if (!is.null(args$file)) {
    files <- args$file
  }
  
  if (!is.null(args$directory)) {
    directories <- args$directory
  }
  
  if (is.null(files) && is.null(directories)) {
    directories <- "."
  }
  
  symbols <- strsplit(args$symbols, ",")[[1]]
  
  if (!is.null(files)) {
    for (file in files) {
      find_functions_in_file(file, symbols)
    }
  }
  
  if (!is.null(directories)) {
    for (directory in directories) {
      find_functions_in_directory(directory, symbols)
    }
  }
}

cli()
