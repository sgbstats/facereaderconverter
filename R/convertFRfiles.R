#' Convert Facereader TXT to CSV
#'
#' Reads a .txt file skipping the first 10 lines, guesses the delimiter from
#' the first remaining line, writes a .csv with the same basename in the same
#' directory, and returns the path to the created CSV (invisibly).
#'
#' @param inpath Path to an existing .txt file.
#' @param outpath Path to save the csv to defaults to the inpath
#' @param return_data Bool to return the data from the txt rather than the metadata
#' @param values_as_numeric Save values as numeric, where applicable
#' @param clean_names returns janitor-style clean names
#' @return Invisibly returns the metadata.
#' @export
#'
#' @importFrom readr read_lines read_delim read_table cols col_character col_guess
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom dplyr case_when mutate
#' @importFrom stringr str_trim
#' @importFrom janitor clean_names
#' @importFrom hms as_hms
#' @importFrom tidyselect any_of

convertFRFiles <- function(
  inpath,
  outpath = paste0(tools::file_path_sans_ext(inpath), ".csv"),
  return_data = FALSE,
  values_as_numeric = TRUE,
  clean_names = TRUE
) {
  if (!is.character(inpath) || length(inpath) != 1) {
    stop("`inpath` must be a single string to a .txt file.")
  }
  if (!file.exists(inpath)) {
    stop("File does not exist: ", inpath)
  }
  ext <- tolower(tools::file_ext(inpath))
  if (ext != "txt") {
    stop("Input file must have a .txt extension.")
  }

  # metadata

  md <- read_lines(inpath, n_max = 10)

  md_type <- dplyr::case_when(
    grepl("detailed", md[1]) ~ "detailed",
    grepl("state", md[1]) ~ "state",
    TRUE ~ "other"
  )

  if (!grepl("video analysis", md[1], ignore.case = TRUE)) {
    stop("Not a FR file.")
  }

  md_videoname <- gsub("Filename", "", md[6]) |> stringr::str_trim()
  md_time <- gsub("Start time", "", md[5]) |>
    stringr::str_trim() |>
    as.POSIXct(format = "%m/%d/%Y %H:%M:%S")

  first_after_skip <- readr::read_lines(inpath, skip = 10, n_max = 1)
  if (length(first_after_skip) == 0) {
    stop("File has no content after the first 10 lines.")
  }

  # guess delimiter from that first line
  delim <- if (grepl(",", first_after_skip, fixed = TRUE)) {
    ","
  } else if (grepl("\t", first_after_skip, fixed = TRUE)) {
    "\t"
  } else if (grepl(";", first_after_skip, fixed = TRUE)) {
    ";"
  } else {
    "ws" # whitespace
  }

  # read the data skipping the first 10 lines
  df <- switch(
    delim,
    "," = readr::read_delim(
      inpath,
      delim = ",",
      skip = 10,
      col_types = cols(
        `Video Time` = col_character(),
        .default = col_guess(),
      ),
      show_col_types = FALSE
    ),
    "\t" = readr::read_delim(
      inpath,
      delim = "\t",
      skip = 10,
      col_types = readr::cols(
        `Video Time` = col_character(),
        .default = col_guess(),
      ),
      show_col_types = FALSE
    ),
    ";" = readr::read_delim(
      inpath,
      delim = ";",
      skip = 10,
      col_types = cols(
        `Video Time` = col_character(),
        .default = col_guess(),
      ),
      show_col_types = FALSE
    ),
    readr::read_table(
      inpath,
      skip = 10,
      col_types = cols(
        `Video Time` = col_character(),
        .default = col_guess(),
      ),
      show_col_types = FALSE
    )
  )

  if (values_as_numeric) {
    df <- df |>
      dplyr::mutate(`Video Time` = as_hms(`Video Time`))

    if (md_type == "detailed") {
      df <- df |>
        dplyr::mutate(across(
          -any_of(c("Video Time")),
          ~ suppressWarnings(as.numeric(.))
        ))
    }
  }

  if (clean_names) {
    df <- janitor::clean_names(df)
  }
  # construct CSV path (same dir, same basename, .csv extension)
  csv_path <- file.path(
    dirname(outpath),
    paste0(tools::file_path_sans_ext(basename(outpath)), ".csv")
  )

  if (return_data) {
    invisible(df)
  } else {
    readr::write_csv(df, csv_path)

    metadata = data.frame(
      video_filename = md_videoname,
      time = md_time,
      type = md_type,
      inpath = inpath,
      outpath = csv_path
    )
    invisible(metadata)
  }
}
