#' @title clockPlot
#'
#' @description Uses ggplot to create a clock face graphic for visual purposes. This could then be saved and used as a visual aid in a document or in print
#'
#' @param time Either a tibble with a column of type POSIX or a single value of type POSIX (time is what we are plotting)
#' @param column The name of the column if time is a tibble as type String (because it is defining which column is needed)
#' @param secondHand Should the secondhand be shown. Default of FALSE means it is not shown (it controls the second hand visibility)
#' @param labels What labels to show for each hour, if any as type String (it controls the hour number style)
#' @param minuteMarks Should the marks be shown for each minute. Default of TRUE means they are shown (it controls the minute marks visibility)
#' @param hourMarks Should the marks be shown for each hour. Default of TRUE means they are shown (it controls the hour marks visibility)
#' @param border Should there be a border around the clock. Default of TRUE means there is (it controls the border visibility)
#'
#' @return A single clockPlot or the tibble with a clockPlot column in the first position
#'
#' @importFrom magrittr "%>%"
#' @importFrom stats "complete.cases"
#' @importFrom utils "as.roman"
#'
#' @examples
#' clockPlot(Sys.time())
#' clockPlot(Sys.time(), labels = "roman")
#'
#' @export
clockPlot <- function(time, column = "", secondHand = FALSE, labels = "numeric", minuteMarks = TRUE, hourMarks = TRUE, border = TRUE) {

  clockPlotEnv <- new.env()
  hourInterval <- minuteInterval <- x <- y <- NULL

  if(TRUE %in% !complete.cases(time)) {
    stop("Please ensure to first remove all NA values from 'time'")
  }

  if(any(is.na(c(column, secondHand, minuteMarks, hourMarks, border)))) {
    stop("Please ensure that there are no NA values in the clockPlot() call")
  }

  singleTime = time
  totalPlots = 1

  if(lubridate::is.POSIXct(time)) {
    #is POSIXct
  } else if(lubridate::is.POSIXlt(time)) {
    #is POSIXlt
  } else if(dplyr::is.tbl(time)) {
    if(column == "") {
      stop("Must assign value to 'column' if time is of type tibble")
    } else {
      #Is tibble
      totalPlots = nrow(time)
      clockPlotEnv$multiplePlots <- tibble::tibble(clockPlots = list())
    }
  } else {
    stop("Time must of type POSIXct, POSIXlt or a tibble with a column of either type")
  }

  if(!dplyr::is.tbl(time) && column != "") {
    warning("'column' not in use when 'time' is of type POSIX. Ignoring value.")
  }

  if(typeof(secondHand) != "logical") {
    stop("'secondHand' must be of type logical")
  }

  if(!(labels %in% c("numeric", "roman", "none"))) {
    stop("'labels' must be a string of value 'numeric', 'roman', or 'none'")
  }

  if(typeof(minuteMarks) != "logical") {
    stop("'minuteMarks' must be of type logical")
  }

  if(typeof(hourMarks) != "logical") {
    stop("'hourMarks' must be of type logical")
  }

  if(typeof(border) != "logical") {
    stop("'border' must be of type logical")
  }

  hour_labels <- tibble::tibble(hourInterval = c(1:12), x = 5 * sin(hourInterval * pi / 6), y = 5 * cos(hourInterval * pi / 6), roman = as.character(as.roman(hourInterval)))
  hours <- tibble::tibble(hourInterval = c(1:12), x = 4.5 * sin(hourInterval * pi / 6), y = 4.5 * cos(hourInterval * pi / 6))
  hourHand <- tibble::tibble(hourInterval = c(1:12), x = 3 * sin(hourInterval * pi / 6), y = 3 * cos(hourInterval * pi / 6))
  minutes <- tibble::tibble(minuteInterval = c(1:60), x = 4.5 * sin(minuteInterval * pi / 30), y = 4.5 * cos(minuteInterval * pi / 30))
  borderPos <- tibble::tibble(minuteInterval = c(1:61), x = 5.5 * sin(minuteInterval * pi / 30), y = 5.5 * cos(minuteInterval * pi / 30))

  for(i in c(1:totalPlots))
  {
    local({
      i <- i
      if(dplyr::is.tbl(time)) {
        singleTime <- time[column][[i, 1]]
      }

      curTime <- singleTime %>%
        stringr::str_split(" ") %>%
        purrr::map_chr(2) %>%
        lubridate::hms()

      if(lubridate::hour(curTime) >= 24) {
        stop("Time cannot be greater than 23:59:59")
      }

      dispHour <- hourHand[ifelse(lubridate::hour(curTime) > 12, lubridate::hour(curTime) %% 12, ifelse(lubridate::hour(curTime) == 0, 12, lubridate::hour(curTime))),]
      dispMin <- minutes[ifelse(lubridate::minute(curTime) == 0, 60, lubridate::minute(curTime)),]
      dispSecond <- minutes[ifelse(lubridate::second(curTime) == 0, 60, lubridate::second(curTime)),]

      plot <- ggplot2::ggplot(hour_labels, ggplot2::aes(x, y), show.legend = FALSE)

      if(border) {
        plot <- plot + ggplot2::geom_path(data = borderPos, ggplot2::aes(x, y))
      }

      if(labels == "numeric") {
        plot <- plot + ggplot2::geom_point(data = hour_labels, ggplot2::aes(x, y), alpha = 0) + ggplot2::geom_text(ggplot2::aes(label=hourInterval))
      } else if(labels == "roman") {
        plot <- plot + ggplot2::geom_point(data = hour_labels, ggplot2::aes(x, y), alpha = 0) + ggplot2::geom_text(ggplot2::aes(label=hour_labels$roman))
      }

      if(hourMarks) {
        plot <- plot + ggplot2::geom_point(data = hours, ggplot2::aes(x, y), size = 3)
      }

      if(minuteMarks) {
        plot <- plot + ggplot2::geom_point(data = minutes, ggplot2::aes(x, y), alpha = 0.5)
      }

      plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = dispHour[[2]], yend = dispHour[[3]]), colour = "black", size = 3, lineend="round")
      plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = dispMin[[2]], yend = dispMin[[3]]), colour = "black", size = 2, lineend="round")

      if(secondHand) {
        plot <- plot + ggplot2::geom_segment(ggplot2::aes(x = 0, y = 0, xend = dispSecond[[2]], yend = dispSecond[[3]]), colour = "red", size = 1, lineend="round")
      }

      plot <- plot + ggplot2::geom_point(ggplot2::aes(x=0, y=0))
      plot <- plot + ggplot2::coord_fixed()  +
        ggplot2::theme(
          legend.position="none",
          axis.line = ggplot2::element_blank(),
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid.major = ggplot2::element_blank(),
          panel.grid.minor = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          panel.background = ggplot2::element_blank()
        )

      if(dplyr::is.tbl(time)) {
        clockPlotEnv$multiplePlots <- clockPlotEnv$multiplePlots %>%
          tibble::add_row("clockPlots" = list(plot))
      } else {
        clockPlotEnv$singlePlot <- plot
      }
    })
  }

  if(dplyr::is.tbl(time)) {

    time <- time %>% tibble::add_column(clockPlotEnv$multiplePlots)

    time <- time %>%
      dplyr::relocate("clockPlots")
    return(time)
  } else {
    return(clockPlotEnv$singlePlot)
  }
}
