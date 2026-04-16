#' Add delta column to coding_df
#'
#' @param coding_df Data frame with columns: id, subject, emotion, frame (or video_time), value
#' @param delta_window Window size in seconds
#' @param delta Threshold for both up and down
#' @param fps Frames per second
#' @return coding_df with extra column 'delta'
#'

add_delta_column <- function(
  coding_df,
  delta_window = 0.1,
  delta = 0.1,
  fps = 30L
) {
  requireNamespace("data.table")
  dt <- data.table::as.data.table(coding_df)
  k <- as.integer(round(delta_window * fps))
  dt[, delta := all_deltas_cpp(value, k, delta), by = .(id, subject, emotion)]
  return(dt)
}
