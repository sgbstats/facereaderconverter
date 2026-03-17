#' Convert a directory of Facereader TXT to CSV
#'
#' Reads all Txt files in a folder and sends them through convertFRFiles.

#' @param inpath Path to an existing .txt file.
#' @param outpath Path to save the csvs to defaults to the inpath
#' @param recursive Bool as to whether to look for all files in directory (`TRUE`) or just the root folder (`FALSE`)
#' @param pattern a regex pattern of files to test, if `NULL` then will look for all txt files
#' @param values_as_numeric Save values as numeric, where applicable
#' @param clean_names returns janitor-style clean names
#' @param save_metadata save the metadata as a csv in the outpath, set to NULL to not save
#' @param ... arguments passed as necessary
#' @return Invisibly returns the metadata.
#' @export
#'
#' @importFrom dplyr across
#' @importFrom tibble as_tibble
#' @importFrom stats "time"

convertFRDirectory <- function(
  inpath,
  outpath = inpath,
  recursive = TRUE,
  pattern = NULL,
  values_as_numeric = TRUE,
  clean_names = TRUE,
  save_metadata = outpath,
  ...
) {
  if (is.null(pattern)) {
    ls <- list.files(
      inpath,
      pattern = ".*\\.txt$",
      recursive = recursive,
      full.names = TRUE
    )
  } else {
    ls <- list.files(
      inpath,
      pattern = paste0(pattern, ".*\\.txt$"),
      recursive = recursive,
      full.names = TRUE
    )
  }

  # initialise metadata with time as POSIXct
  metadata <- tibble::tibble(
    inpath = character(),
    outpath = character(),
    video_filename = character(),
    time = as.POSIXct(character(), tz = "UTC"),
    type = character(),
    status = character(),
    error = character()
  )

  if (outpath != inpath) {
    ls_out <- map_paths(inpath, outpath, ls)
  } else {
    ls_out <- ls
  }

  for (i in seq_along(ls)) {
    tryCatch(
      {
        md <- convertFRFiles(
          ls[i],
          ls_out[i],
          values_as_numeric = values_as_numeric,
          clean_names = clean_names,
          ...
        )

        # coerce success-row types to match metadata
        md <- md |>
          dplyr::mutate(
            video_filename = as.character(video_filename),
            time = as.POSIXct(time, tz = "UTC"),
            type = as.character(type),
            inpath = as.character(inpath),
            outpath = as.character(outpath),
            status = "Success",
            error = NA_character_
          )

        metadata <- dplyr::bind_rows(metadata, md)
      },
      error = function(e) {
        # use POSIXct NA for time and character NA for strings
        md_fail <- tibble::tibble(
          video_filename = NA_character_,
          time = as.POSIXct(NA, tz = "UTC"),
          type = NA_character_,
          inpath = as.character(ls[i]),
          outpath = as.character(ls_out[i]),
          status = "Fail",
          error = as.character(e$message)
        )
        metadata <<- dplyr::bind_rows(metadata, md_fail)
      }
    )
  }
  if (!is.null(save_metadata)) {
    write.csv(metadata, paste0(save_metadata, "/metadata.csv"))
  }
  invisible(metadata)
}
