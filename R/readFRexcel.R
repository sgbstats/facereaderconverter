#' Reads an Excel or csv file with fidelity
#'
#' Reads a .txt file skipping the first 10 lines, guesses the delimiter from
#' the first remaining line, writes a .csv with the same basename in the same
#' directory, and returns the data
#'
#' @param inpath Path to an existing .txt file.
#' @param outpath Path to save the csv to defaults to the inpath
#' @param return_data Bool to return the data from the txt rather than the metadata
#' @param values_as_numeric Save values as numeric, where applicable
#' @param clean_names returns janitor-style clean names
#' @param fail_codes adds a column with the fail reason, True or False. Column then has 0 for success, 1 for fit_failed, 2 for find_failed
#' @param duplicate_timecodes_as_error throws an error if there are duplicate timecodes, if FALSE then throws warning
#' @param ... arguments passed as necessary
#' @return Invisibly returns the metadata.
#' @examples
#' \dontrun{
#' convertFRFiles(
#'   inpath="FaceReaderOutput.txt",
#'   values_as_numeric = TRUE
#' )
#' }
#'
#' @export
#'
#' @importFrom readr read_lines read_delim read_table cols col_character col_guess
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom dplyr case_when mutate count pull
#' @importFrom stringr str_trim
#' @importFrom janitor clean_names
#' @importFrom hms as_hms
#' @importFrom tidyselect any_of
#' @importFrom readxl read_excel

loadFR <- function(
  inpath,
  return_data = FALSE,
  values_as_numeric = TRUE,
  clean_names = TRUE,
  fail_codes = FALSE,
  duplicate_timecodes_as_error = TRUE,
  ...
) {}
