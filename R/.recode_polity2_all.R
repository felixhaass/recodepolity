
# internal function to round polity score
round_polity <- function(x) round(x+0.005)

# internal function to trim trailing characters
trim.trailing <- function (x) sub("\\s+$", "", x)


# main function
recode_polity2min <- function(x){

  helpdf <- data.frame(year = x$year, polity = x$polity, polity2min = 0)

  # print(unique(as.character(x$country)))

  # identify if there are -77 or -88 in the beginning of time-series and set to NA, because we cannot
  # know true value
  if(!is.na(helpdf[1, "polity"]) | (helpdf[1 , "polity"] == -77) | (helpdf[1 , "polity"] == -88) ) {
    yearstart <- 1
    while((helpdf[yearstart, "polity"]) == -77 | (helpdf[yearstart, "polity"] == -88)) {
      helpdf[yearstart, "polity2min"] <- NA
      yearstart <- yearstart + 1

    }
  }

  min_df <- helpdf
  min_df[!is.na(helpdf$polity2min), "polity2min"] <- helpdf[!is.na(helpdf$polity2min), "polity"]
  min_df$polity2min <- ifelse(min_df$polity2min == -66, NA, min_df$polity2min)

  # identify interregnum-cum-transition periods

  rownames_ict <- suppressWarnings(as.numeric(row.names(min_df[min_df$polity2min %in% c(-77, -88), ])))
  rownames_ict <- rownames_ict[!is.na(rownames_ict)]

  ict_periods <- split(rownames_ict, cumsum(seq_along(rownames_ict) %in% (which(diff(rownames_ict)>1)+1))) # credit for this line goes to http://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r

  for(period in ict_periods) {
    # first condition is to check if interregnum-cum-transition
    if(-88 %in% min_df[period, "polity2min"] & -77 %in% min_df[period, "polity2min"] ) {

      before <- min_df[as.numeric(row.names(min_df)) == as.numeric(min(period)) - 1, "polity2min"]
      after <- min_df[as.numeric(row.names(min_df)) == as.numeric(max(period)) + 1, "polity2min"]

      # this means time-series ends with transition or interregnum, so we don't know the true value
      if(length(after) == 0) {
        min_df[period, "polity2min"] <- NA
        next()
      }

      # implement rule one: take the lower value
      if(before < after) {
        replace <- min_df[period, "polity2min"]
        replace[replace == -77] <- before
        min_df[period, "polity2min"] <- replace
      } else {
        replace <- min_df[period, "polity2min"]
        replace[replace == -77] <- after
        min_df[period, "polity2min"] <- replace
      }


    }

    # second condition is to check whether it is single strech of -77

    if (!(-88 %in% min_df[period, "polity2min"]) & -77 %in% min_df[period, "polity2min"] ) {
      before <- min_df[as.numeric(row.names(min_df)) == as.numeric(min(period)) - 1, "polity2min"]
      after <- min_df[as.numeric(row.names(min_df)) == as.numeric(max(period)) + 1, "polity2min"]

      # this means a) time-series ends with transition or interregnum, so we don't know the true value
      # b) we have -66 prior or following and don't know true value either
      if(length(after) == 0 || is.na(after) || is.na(before)) {
        min_df[period, "polity2min"] <- NA
        next()
      }

      # implement rule one: take the lower value
      if(before < after ) {
        replace <- min_df[period, "polity2min"]
        replace[replace == -77] <- before
        min_df[period, "polity2min"] <- replace
      } else {
        replace <- min_df[period, "polity2min"]
        replace[replace == -77] <- after
        min_df[period, "polity2min"] <- replace
      }

    }
  }

  # interpolate
  # identify rownames of transition / -88 periods
  rownames <- suppressWarnings(as.numeric(row.names(min_df[min_df$polity2min == -88, ])))
  rownames <- rownames[!is.na(rownames)]
  transition_periods <- split(rownames, cumsum(seq_along(rownames) %in% (which(diff(rownames)>1)+1))) # credit for this line goes to http://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r

  for(period in transition_periods) {

    before <- min_df[as.numeric(row.names(min_df)) == as.numeric(min(period)) - 1, "polity2min"]
    after <- min_df[as.numeric(row.names(min_df)) == as.numeric(max(period)) + 1, "polity2min"]

    rows <- length(period)

    if(length(after) ==  0 || is.na(before) || is.na(after)) {
      min_df[period, "polity2min"] <- NA
    } else {
      #interpolate
      interpolation <- round_polity(approx(c(before, after), method = "linear", n = rows + 2)$y)
      interpolation <- interpolation[2:(length(interpolation) -1)]

      min_df[period, "polity2min"] <- interpolation
    }

  }

  return(min_df)
}


recode_polity2max <- function(x){

  helpdf <- data.frame(year = x$year, polity = x$polity, polity2max = 0)

  # print(unique(as.character(x$country)))

  # identify if there are -77 or -88 in the beginning of time-series and set to NA, because we cannot
  # know true value
  if(!is.na(helpdf[1, "polity"]) | (helpdf[1 , "polity"] == -77) | (helpdf[1 , "polity"] == -88) ) {
    yearstart <- 1
    while((helpdf[yearstart, "polity"]) == -77 | (helpdf[yearstart, "polity"] == -88)) {
      helpdf[yearstart, "polity2max"] <- NA
      yearstart <- yearstart + 1

    }
  }


  max_df <- helpdf
  max_df[!is.na(helpdf$polity2max), "polity2max"] <- helpdf[!is.na(helpdf$polity2max), "polity"]
  max_df$polity2max <- ifelse(max_df$polity2max == -66, NA, max_df$polity2max)

  # identify interregnum-cum-transition periods

  rownames_ict <- suppressWarnings(as.numeric(row.names(max_df[max_df$polity2max %in% c(-77, -88), ])))
  rownames_ict <- rownames_ict[!is.na(rownames_ict)]

  ict_periods <- split(rownames_ict, cumsum(seq_along(rownames_ict) %in% (which(diff(rownames_ict)>1)+1))) # credit for this line goes to http://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r

  for(period in ict_periods) {
    # first condition is to check if interregnum-cum-transition
    if(-88 %in% max_df[period, "polity2max"] & -77 %in% max_df[period, "polity2max"] ) {

      before <- max_df[as.numeric(row.names(max_df)) == as.numeric(min(period)) - 1, "polity2max"]
      after <- max_df[as.numeric(row.names(max_df)) == as.numeric(max(period)) + 1, "polity2max"]

      # this means time-series ends with transition or interregnum, so we don't know the true value
      if(length(after) == 0) {
        max_df[period, "polity2max"] <- NA
        next()
      }

      # implement rule one: take the bigger value
      if(before < after) {
        replace <- max_df[period, "polity2max"]
        replace[replace == -77] <- after
        max_df[period, "polity2max"] <- replace
      } else {
        replace <- max_df[period, "polity2max"]
        replace[replace == -77] <- before
        max_df[period, "polity2max"] <- replace
      }


    }

    # second condition is to check whether it is single strech of -77

    if (!(-88 %in% max_df[period, "polity2max"]) & -77 %in% max_df[period, "polity2max"] ) {
      before <- max_df[as.numeric(row.names(max_df)) == as.numeric(min(period)) - 1, "polity2max"]
      after <- max_df[as.numeric(row.names(max_df)) == as.numeric(max(period)) + 1, "polity2max"]

      # this means a) time-series ends with transition or interregnum, so we don't know the true value
      # b) we have -66 prior or following and don't know true value either
      if(length(after) == 0 || is.na(after) || is.na(before)) {
        max_df[period, "polity2max"] <- NA
        next()
      }

      # implement rule one: take the lower value
      if(before < after ) {
        replace <- max_df[period, "polity2max"]
        replace[replace == -77] <- after
        max_df[period, "polity2max"] <- replace
      } else {
        replace <- max_df[period, "polity2max"]
        replace[replace == -77] <- before
        max_df[period, "polity2max"] <- replace
      }

    }
  }

  # interpolate
  # identify rownames of transition / -88 periods
  rownames <- suppressWarnings(as.numeric(row.names(max_df[max_df$polity2max == -88, ])))
  rownames <- rownames[!is.na(rownames)]
  transition_periods <- split(rownames, cumsum(seq_along(rownames) %in% (which(diff(rownames)>1)+1))) # credit for this line goes to http://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r

  for(period in transition_periods) {

    before <- max_df[as.numeric(row.names(max_df)) == as.numeric(min(period)) - 1, "polity2max"]
    after <- max_df[as.numeric(row.names(max_df)) == as.numeric(max(period)) + 1, "polity2max"]

    rows <- length(period)

    if(length(after) ==  0 || is.na(before) || is.na(after)) {
      max_df[period, "polity2max"] <- NA
    } else {
      #interpolate
      interpolation <- round_polity(approx(c(before, after), method = "linear", n = rows + 2)$y)
      interpolation <- interpolation[2:(length(interpolation) -1)]

      max_df[period, "polity2max"] <- interpolation
    }

  }

  return(max_df)
}


recode_polity2inter <- function(x){

  helpdf <- data.frame(year = x$year, polity = x$polity, polity2inter = 0)

  # print(unique(as.character(x$country)))

  # identify if there are -77 or -88 in the beginning of time-series and set to NA, because we cannot
  # know true value
  if(!is.na(helpdf[1, "polity"]) | (helpdf[1 , "polity"] == -77) | (helpdf[1 , "polity"] == -88) ) {
    yearstart <- 1
    while((helpdf[yearstart, "polity"]) == -77 | (helpdf[yearstart, "polity"] == -88)) {
      helpdf[yearstart, "polity2inter"] <- NA
      yearstart <- yearstart + 1

    }
  }


  inter_df <- helpdf
  inter_df[!is.na(helpdf$polity2inter), "polity2inter"] <- helpdf[!is.na(helpdf$polity2inter), "polity"]
  inter_df$polity2inter <- ifelse(inter_df$polity2inter == -66, NA, inter_df$polity2inter)

  # identify interregnum-cum-transition periods

  rownames <- suppressWarnings(as.numeric(row.names(inter_df[inter_df$polity2inter %in% c(-77, -88), ])))
  rownames <- rownames[!is.na(rownames)]

  transition_periods <- split(rownames, cumsum(seq_along(rownames) %in% (which(diff(rownames)>1)+1))) # credit for this line goes to http://stackoverflow.com/questions/18508363/split-a-numeric-vector-into-continuous-chunks-in-r

  for(period in transition_periods) {

    before <- inter_df[as.numeric(row.names(inter_df)) == as.numeric(min(period)) - 1, "polity2inter"]
    after <- inter_df[as.numeric(row.names(inter_df)) == as.numeric(max(period)) + 1, "polity2inter"]

    rows <- length(period)

    if(length(after) ==  0 || is.na(before) || is.na(after)) {
      inter_df[period, "polity2inter"] <- NA
    } else {
      #interpolate
      interpolation <- round_polity(approx(c(before, after), method = "linear", n = rows + 2)$y)
      interpolation <- interpolation[2:(length(interpolation) -1)]

      inter_df[period, "polity2inter"] <- interpolation
    }

  }

  return(inter_df)
}
