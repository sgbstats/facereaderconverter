#' Apply LOCF and Extract Episodes
#'
#' Applies Last Observation Carried Forward (LOCF) logic to the `coding` data.table (from `convert_to_episodes`), imputing missing `run_id` values within blocks of non-missing `value`, grouped by `id`, `subject`, and `emotion`. Returns both the imputed coding table and a summary of contiguous episodes.
#'
#' @param coding A data.table or data.frame as returned by `convert_to_episodes`, containing columns: `id`, `subject`, `emotion`, `video_time`, `value`, and `run_id`.
#'
#' @return A list with two elements:
#'   \describe{
#'     \item{episodes}{data.table of contiguous episodes after LOCF, with columns \code{start_frame}, \code{end_frame}, \code{n_frames}, \code{duration_s}, \code{id}, \code{subject}, \code{emotion}, and \code{run_id}.}
#'     \item{coding}{Annotated data.table with LOCF-imputed \code{run_id}.}
#'   }
#' @import data.table
#' @export
locf <- function(coding) {
  stopifnot(requireNamespace("data.table"))
  if ("fr_coding" %in% class(coding)) {
    table = coding$coding
  } else {
    table = coding
  }
  dt <- data.table::as.data.table(table)
  fps <- if ("fps" %in% names(attributes(dt))) attr(dt, "fps") else 30L

  # LOCF for run_id within blocks of non-missing value
  dt[
    order(id, subject, emotion), # removed frame
    run_id := zoo::na.locf(run_id, na.rm = FALSE),
    by = .(id, subject, emotion)
  ]

  # Identify contiguous episodes (runs of same run_id)
  dt[, run_grp := data.table::rleid(run_id), by = .(id, subject, emotion)]
  episodes <- dt[
    !is.na(run_id),
    .(
      start_time = video_time[1],
      end_time = video_time[.N],
      n_frames = .N,
      duration_s = .N / fps,
      id = first(id),
      subject = first(subject),
      emotion = first(emotion),
      run_id = first(run_id)
    ),
    by = .(id, subject, emotion, run_grp)
  ][, run_grp := NULL][]

  dt[, run_grp := NULL]
  structure(
    list(
      episodes = episodes,
      coding = dt[]
    ),
    class = "fr_coding"
  )
}
