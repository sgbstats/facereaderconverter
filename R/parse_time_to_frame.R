#' Parse video time to frame index
#'
#' Convert a time string into a frame index given frames-per-second (fps).
#' Supports "HH:MM:SS", "MM:SS", or plain seconds and is vectorised over `video_time`.
#'
#' @param video_time Character or numeric. Time strings in "HH:MM:SS", "MM:SS", or numeric seconds.
#' @param fps Numeric. Frames per second.
#' @return Integer vector. Frame indices computed as round(seconds * fps).
#' @examples
#' parse_time_to_frame("00:01:23.5", fps = 30)
#' parse_time_to_frame("1:23.5", fps = 30)
#' parse_time_to_frame("83.5", fps = 30)
#' @export
#' @importFrom data.table tstrsplit
# parser: use existing if available, otherwise local

parse_time_to_frame <- function(video_time, fps) {
  parts <- data.table::tstrsplit(video_time, ":", fixed = FALSE)
  ncols <- length(parts)
  if (ncols == 3L) {
    hh <- as.integer(parts[[1L]])
    mm <- as.integer(parts[[2L]])
    ss <- as.numeric(parts[[3L]])
    sec <- hh * 3600 + mm * 60 + ss
  } else if (ncols == 2L) {
    mm <- as.integer(parts[[1L]])
    ss <- as.numeric(parts[[2L]])
    sec <- mm * 60 + ss
  } else if (ncols == 1L) {
    sec <- as.numeric(parts[[1L]])
  } else {
    stop("Unrecognized video_time format.")
  }
  as.integer(round(sec * fps))
}
