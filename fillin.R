fillin <- function(data) {
  meanint <- ddply(data, .(interval), summarize, steps = mean(steps, na.rm =T))
  for (i in 1:17568) {
    if (i %% 288 == 0){
      j = 288
    } else {
      j = i %% 288
    }
    if (is.na(data$steps[i])){
      data$steps[i] <- meanint$steps[j]
    }
  }
  data
}