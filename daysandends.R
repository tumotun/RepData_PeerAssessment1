daysandends <- function(date) {
  day <- weekdays(as.Date(date))
  if (day %in% c("Saturday", "Sunday")) {
    result <- "Weekend"
  } else {
    result <- "Weekday"
  }
  result
}