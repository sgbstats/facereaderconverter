#' Detect episodes using hysteresis + delta + min-duration
#'
#' Convert frame-level coding into episode-level summaries. Ensures the returned
#' episode end_frame is the last prior frame with a non-NA value.
#'
#' @param coding_df data.frame with columns: id, subject, emotion, frame (or video_time), value or with id, subject and video_time with wide emotions
#' @param fps integer Frames per second (sampling rate of the data). Default: 30L.
#' @param T_up numeric Upper threshold for entering an episode. Default: 0.20.
#' @param T_down numeric Lower threshold for exiting an episode. Default: 0.18.
#' @param delta numeric Minimum change (delta) for windowed max-min rule. Default: 0.10.
#' @param delta_window numeric Window size (in seconds) for the delta/max-min rule. Default: 0.1.
#' @param min_dur_sec numeric Minimum episode duration (in seconds). Default: 0.1.
#' @param consecutive_missing integer Maximum allowed consecutive missing (NA) frames while in-state before forcing episode end. Default: 150L.
#' @return A list with two elements:
#' \describe{
#'   \item{episodes}{data.table of detected episodes (start_frame, end_frame, n_frames, duration_s, id, subject, emotion).}
#'   \item{coding}{annotated data.table (original frames with added columns \code{status} and \code{in_state}).}
#' }
#' @details The function uses a C++ implementation \code{hysteresis_state_cpp} if available; otherwise it will error.
#' It relies on \pkg{data.table} for fast grouping and joins.
#' @examples
#' \dontrun{
#' convert_to_episodes(coding_df, fps = 30L)
#' }
#' @import data.table
#' @importFrom tidyr pivot_longer
#' @export
convert_to_episodes <- function(
  coding_df,
  fps = 30L,
  T_up = 0.20,
  T_down = 0.18,
  delta = 0.10,
  delta_window = 0.1,
  min_dur_sec = 0.1,
  consecutive_missing = 150L
) {
  is_scalar <- function(x) length(x) == 1 && !is.na(x)
  is_whole <- function(x) {
    is.numeric(x) && is_scalar(x) && abs(x - round(x)) < .Machine$double.eps^0.5
  }

  if (!is_whole(fps) || fps <= 0) {
    stop("`fps` must be a positive integer scalar.")
  }
  if (
    !is.numeric(delta_window) || !is_scalar(delta_window) || delta_window < 0
  ) {
    stop("`delta_window` must be a numeric scalar >= 0.")
  }
  if (!is.numeric(T_up) || !is_scalar(T_up) || T_up < 0 || T_up > 1) {
    stop("`T_up` must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(T_down) || !is_scalar(T_down) || T_down < 0 || T_down > 1) {
    stop("`T_down` must be a numeric scalar in [0, 1].")
  }
  if (!is.numeric(T_up) || !is_scalar(T_up) || T_up < 0 || T_up > 1) {
    stop("`T_up` must be a numeric scalar in [0, 1].")
  }
  if (T_up < T_down) {
    stop("`T_up` must be >= `T_down`.")
  }
  if (!is.numeric(delta) || !is_scalar(delta) || delta <= 0) {
    stop("`delta` must be a numeric scalar > 0.")
  }
  if (!is.numeric(min_dur_sec) || !is_scalar(min_dur_sec) || min_dur_sec <= 0) {
    stop("`min_dur_sec` must be a numeric scalar > 0.")
  }
  if (!is_whole(consecutive_missing) || consecutive_missing < 0) {
    stop("`consecutive_missing` must be a non-negative integer scalar.")
  }

  # Validate required columns early
  required_cols <- c("id", "subject", "video_time")
  missing_cols <- setdiff(required_cols, names(coding_df))
  if (length(missing_cols) > 0L) {
    stop(
      sprintf(
        "coding_df is missing required column(s): %s. It must contain: %s",
        paste(missing_cols, collapse = ", "),
        paste(required_cols, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!all(c("emotion", "value") %in% names(coding_df))) {
    coding_df <- coding_df |>
      pivot_longer(
        cols = -c("id", "subject", "video_time"),
        names_to = "emotion",
        values_to = "value"
      )
  }
  stopifnot(requireNamespace("data.table"))
  dt <- data.table::as.data.table(coding_df)
  k <- as.integer(round(delta_window * fps))
  min_len <- as.integer(ceiling(min_dur_sec * fps))
  if (!"frame" %in% names(dt)) {
    if (!"video_time" %in% names(dt)) {
      stop("Need either 'frame' or 'video_time' column.")
    }
    dt[, frame := parse_time_to_frame(video_time, fps = fps)]
  }

  # type hygiene & key
  if ("subject" %in% names(dt) && !is.factor(dt$subject)) {
    dt[, subject := as.factor(subject)]
  }
  if ("emotion" %in% names(dt) && !is.factor(dt$emotion)) {
    dt[, emotion := as.factor(emotion)]
  }
  if (!is.integer(dt$frame)) {
    dt[, frame := as.integer(frame)]
  }
  data.table::setkey(dt, id, subject, emotion, frame)

  # choose state detector: C++ if available, else R fallback
  use_cpp <- exists("hysteresis_state_cpp", mode = "function")
  if (use_cpp) {
    dt[,
      state := hysteresis_state_cpp(
        value,
        k,
        T_up,
        T_down,
        delta,
        min_len,
        consecutive_missing
      ),
      by = .(id, subject, emotion)
    ]
  } else {
    stop("CPP not found")
  }

  # episodes: contiguous TRUE runs (keep run_id for mapping)
  dt[, run_id := data.table::rleid(state), by = .(id, subject, emotion)]
  episodes <- dt[
    state == TRUE,
    .(
      start_frame = first(frame),
      end_frame = last(frame),
      n_frames = .N,
      duration_s = .N / fps
    ),
    by = .(id, subject, emotion, run_id)
  ]
  setorder(episodes, id, subject, emotion, start_frame)

  episodes[, ep_id := .I]

  # Candidate frames that are (a) in-state and (b) have observed signal (non-NA)
  valid_in_state <- dt[
    state == TRUE & !is.na(value),
    .(id, subject, emotion, run_id, frame)
  ]

  if (nrow(valid_in_state) > 0L && nrow(episodes) > 0L) {
    data.table::setkey(valid_in_state, id, subject, emotion, run_id, frame)

    # For each episode, find the last valid (non-NA) frame within [start_frame, end_frame]
    # - mult="last" returns the last match on the keyed frame order
    end_map <- valid_in_state[
      episodes,
      on = .(
        id,
        subject,
        emotion,
        run_id,
        frame >= start_frame,
        frame <= end_frame
      ),
      mult = "last",
      .(ep_id = i.ep_id, end_frame_nonNA = frame),
      by = .EACHI
    ]

    # Update end_frame
    episodes[end_map, on = "ep_id", end_frame := i.end_frame_nonNA]

    # Option: drop episodes where we never observed a non-NA value while in-state
    episodes <- episodes[!is.na(end_frame)]

    # Recompute n (TRUE-state frames up to adjusted end_frame) and duration_s
    # This counts ALL in-state frames up to the new end_frame (including frames where value may be NA),
    # but guarantees the end boundary itself is a non-NA frame.
    n_map <- dt[state == TRUE][
      episodes,
      on = .(
        id,
        subject,
        emotion,
        run_id,
        frame >= start_frame,
        frame <= end_frame
      ),
      .(ep_id = i.ep_id, n = .N),
      by = .EACHI
    ]

    episodes[n_map, on = "ep_id", n := i.n]
    episodes[, duration_s := n / fps]
  }

  # recompute frame counts/durations and drop short episodes
  episodes[, ep_id := NULL]
  episodes[, `:=`(
    n_frames = as.integer(end_frame - start_frame + 1L)
  )][, duration_s := n_frames / fps]
  if (nrow(episodes) > 0L) {
    episodes <- episodes[n_frames >= min_len][, n := NULL]
  }

  # Add status column to dt (coding_df)
  dt[, status := NA_integer_]
  if (nrow(episodes) > 0L) {
    # Mark start frames
    dt[
      episodes,
      on = .(id, subject, emotion, frame = start_frame),
      status := 1L
    ]
    # Mark end frames
    dt[episodes, on = .(id, subject, emotion, frame = end_frame), status := 0L]
  }

  dt[, in_state := FALSE]

  if (nrow(episodes) > 0L) {
    # Non-equi join: mark frames within any episode window as TRUE
    dt[
      episodes,
      on = .(id, subject, emotion, frame >= start_frame, frame <= end_frame),
      in_state := TRUE
    ]
  }

  dt[, c("state", "run_id") := NULL]
  # Return both episodes and annotated coding_df
  return(list(
    episodes = episodes,
    coding = dt
  ))
}
