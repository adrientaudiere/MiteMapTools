#' Compute Crossings Between Path Segments
#' @description
#' Internally used in function [import_mitemap()] to compute the number of crossings
#' in a path. A crossing is defined as an intersection between two segments of the
#' path. The function can also compute crossings within a specified time window.
#'
#' @param time_vec (numerical vector) the vector of time
#' @param x_vec (numerical vector) the vector of x coordinates
#' @param y_vec (numerical vector) the vector of y coordinates
#' @param time_window (numerical, default = NULL) If not NULL, only crossings
#'  with segments that started within the last 'time_window' seconds are counted
#'  focusing only on recent "interactions" with previous path.
#'
#' @returns A list with three elements:
#'  - crossings_at_point: A numerical vector indicating the number of crossings
#'  at each point in the path.
#'  - crossings_cumsum: A numerical vector representing the cumulative sum of
#'  crossings up to each point.
#'  - crossings_windowed: A numerical vector indicating the number of crossings
#'  at each point, considering only segments that started within the specified
#'  time window.
#' @export
#' @author Adrien Taudière
#' @examples
#' time_vec <- c(0, 1, 2, 3, 4, 5)
#' x_vec <- c(0, 1, 1, 0, -1, -1)
#' y_vec <- c(0, 0, 1, 1, 0, -1)
#' result <- compute_crossings(time_vec, x_vec, y_vec, time_window = 3)
#' result
#'
#' MM <- import_mitemap(
#'   system.file("extdata", "mitemap_example", package = "MiteMapTools"),
#'   file_name_column = "File (mite ID)", compute_metrics = FALSE
#' )
#'
#' MM |>
#'   group_by(File_name) |>
#'   filter(File_name %in%
#'     c(
#'       "MM012022_05_17_10h23m35s",
#'       "MM012022_05_17_09h23m48s",
#'       "MM012022_05_17_08h23m53s"
#'     )) |>
#'   group_modify(~ {
#'     result <- compute_crossings(.x$X..t.s., .x$x.mm., .x$y.mm., time_window = 10) #'
#'     .x %>%
#'       mutate(
#'         crossings_at_point = result$crossings_at_point,
#'         crossings_cumsum = result$crossings_cumsum,
#'         crossings_windowed = result$crossings_windowed
#'       )
#'   }) |>
#'   group_by(Treatment, File_name) |>
#'   summarize(n_crossings = max(crossings_cumsum, na.rm = TRUE))
#'
#' \dontrun{
#' MM |>
#'   group_by(File_name) |>
#'   group_modify(~ {
#'     result <- compute_crossings(.x$X..t.s., .x$x.mm., .x$y.mm., time_window = 10) #'
#'     .x %>%
#'       mutate(
#'         crossings_at_point = result$crossings_at_point,
#'         crossings_cumsum = result$crossings_cumsum,
#'         crossings_windowed = result$crossings_windowed
#'       )
#'   }) |>
#'   group_by(Treatment, File_name) |>
#'   summarize(n_crossings = max(crossings_cumsum, na.rm = TRUE)) |>
#'   ggstatsplot::ggbetweenstats(Treatment, n_crossings)
#' }
compute_crossings <- function(time_vec, x_vec, y_vec, time_window = NULL) {
  n <- length(time_vec)

  if (n < 3) {
    return(list(
      crossings_at_point = rep(0, n),
      crossings_cumsum = rep(0, n),
      crossings_windowed = rep(0, n)
    ))
  }

  crossings_total <- numeric(n)
  crossings_windowed <- numeric(n)

  # Pre-compute segment properties for faster access
  segments <- data.frame(
    idx = 2:n,
    x1 = x_vec[1:(n - 1)],
    y1 = y_vec[1:(n - 1)],
    x2 = x_vec[2:n],
    y2 = y_vec[2:n],
    time = time_vec[2:n],
    # Bounding box for quick elimination
    min_x = pmin(x_vec[1:(n - 1)], x_vec[2:n]),
    max_x = pmax(x_vec[1:(n - 1)], x_vec[2:n]),
    min_y = pmin(y_vec[1:(n - 1)], y_vec[2:n]),
    max_y = pmax(y_vec[1:(n - 1)], y_vec[2:n])
  )

  # Vectorized segment intersection function
  segments_intersect_vectorized <- function(x1, y1, x2, y2, x3, y3, x4, y4) {
    # Quick bounding box check first
    bbox_intersect <- (pmax(x1, x3) <= pmin(x2, x4)) &
      (pmax(y1, y3) <= pmin(y2, y4)) &
      (pmax(x3, x1) <= pmin(x4, x2)) &
      (pmax(y3, y1) <= pmin(y4, y2))

    if (!any(bbox_intersect)) {
      return(rep(FALSE, length(x1)))
    }

    # Only do detailed check for segments that pass bounding box test
    result <- rep(FALSE, length(x1))
    idx <- which(bbox_intersect)

    if (length(idx) > 0) {
      d1 <- (y4[idx] - y3[idx]) * (x2[idx] - x1[idx]) - (x4[idx] - x3[idx]) * (y2[idx] - y1[idx])

      # Skip parallel lines
      non_parallel <- abs(d1) >= 1e-10
      if (any(non_parallel)) {
        valid_idx <- idx[non_parallel]
        d1_valid <- d1[non_parallel]

        d2 <- (y1[valid_idx] - y3[valid_idx]) * (x4[valid_idx] - x3[valid_idx]) -
          (x1[valid_idx] - x3[valid_idx]) * (y4[valid_idx] - y3[valid_idx])
        d3 <- (y1[valid_idx] - y3[valid_idx]) * (x2[valid_idx] - x1[valid_idx]) -
          (x1[valid_idx] - x3[valid_idx]) * (y2[valid_idx] - y1[valid_idx])

        t1 <- d2 / d1_valid
        t2 <- d3 / d1_valid

        intersects <- (t1 >= 0 & t1 <= 1 & t2 >= 0 & t2 <= 1)
        result[valid_idx] <- intersects
      }
    }

    return(result)
  }

  # Process segments in chunks for better memory management
  chunk_size <- min(1000, n - 2)

  for (i in 3:n) {
    current_time <- time_vec[i]
    current_seg <- segments[i - 1, ] # Current segment

    # Get all previous segments
    prev_segments <- segments[1:(i - 2), ]

    if (nrow(prev_segments) == 0) {
      crossings_total[i] <- 0
      crossings_windowed[i] <- 0
      next
    }

    # Apply time window filter if specified
    if (!is.null(time_window)) {
      time_mask <- (current_time - prev_segments$time) <= time_window
      windowed_segments <- prev_segments[time_mask, ]
    } else {
      windowed_segments <- prev_segments
    }

    # Check intersections for all previous segments (vectorized)
    if (nrow(prev_segments) > 0) {
      intersections <- segments_intersect_vectorized(
        rep(current_seg$x1, nrow(prev_segments)),
        rep(current_seg$y1, nrow(prev_segments)),
        rep(current_seg$x2, nrow(prev_segments)),
        rep(current_seg$y2, nrow(prev_segments)),
        prev_segments$x1,
        prev_segments$y1,
        prev_segments$x2,
        prev_segments$y2
      )
      crossings_total[i] <- sum(intersections)
    }

    # Check intersections for windowed segments
    if (nrow(windowed_segments) > 0) {
      windowed_intersections <- segments_intersect_vectorized(
        rep(current_seg$x1, nrow(windowed_segments)),
        rep(current_seg$y1, nrow(windowed_segments)),
        rep(current_seg$x2, nrow(windowed_segments)),
        rep(current_seg$y2, nrow(windowed_segments)),
        windowed_segments$x1,
        windowed_segments$y1,
        windowed_segments$x2,
        windowed_segments$y2
      )
      crossings_windowed[i] <- sum(windowed_intersections)
    } else {
      crossings_windowed[i] <- crossings_total[i]
    }
  }

  return(list(
    crossings_at_point = crossings_total,
    crossings_cumsum = cumsum(crossings_total),
    crossings_windowed = crossings_windowed
  ))
}
