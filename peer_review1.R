

  setwd("C:/WORKINGDIRECTORY")

  ## LOAD PACKAGES
  if (!require("data.table")) {
    install.packages("data.table")
  }
  require(data.table)
  library(data.table)

  if (!require("sqldf")) {
    install.packages("sqldf")
  }
  require(sqldf)
  library(sqldf)  
  
  if (!require("dplyr")) {
    install.packages("dplyr")
  }
  require(dplyr)
  library(dplyr)  

  if (!require("ggplot2")) {
    install.packages("ggplot2")
  }
  require(ggplot2)
  library(ggplot2)
  
  if (!require("lubridate")) {
    install.packages("lubridate")
  }  
  require(lubridate)
  library(lubridate)  
  
  if (!require("lattice")) {
    install.packages("lattice")
  }  
  require(lattice)
  library(lattice) 
  
  temp <- tempfile()
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(url,temp)
  # unzip(temp, list=TRUE)
   unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
          junkpaths = FALSE, exdir = ".", unzip = "internal",
          setTimes = FALSE)
  
  url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"  
  file <- "activity.csv"

  if (file.exists(file)==FALSE){
  temp <- tempfile()
  download.file(url, temp)
  # unzip(temp, list=TRUE)
  unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
        junkpaths = FALSE, exdir = ".", unzip = "internal",
        setTimes = FALSE)
  }
  unlink(temp)

   rm(url)
  rm(file)
  rm(temp)
 
  data <- read.csv("activity.csv", header = TRUE, na.strings = "NA", stringsAsFactors=FALSE)
  data <- data.table(data)
  
  
  # glimpse(data)
  # View(data)

  data$date_time <- sprintf("%04d", data$interval)
  data$date_time <- as.character(sub("(?<=.{2})", ":", data$date_time, perl=TRUE))
  data$date_time <- as.character(sub("(?<=.{5})", ":00", data$date_time, perl=TRUE))
  data$date_time <- paste(data$date, data$date_time, sep = " ")  
  data$date_time <- ymd_hms(data$date_time)

  data$steps_int <- as.numeric(data$steps)  

  row_has_na <- apply(data, 1, function(x){any(is.na(x))})
  data_no_NA <- data[!row_has_na,]
  
  steps_day <- aggregate(data_no_NA$steps_int, by = list(data_no_NA$date), FUN=sum, na.rm=TRUE)
  steps_day <- data.table(steps_day)
  setnames(steps_day, "Group.1", "Date")
  setnames(steps_day, "x", "Steps")

  steps_day$Date <- as.Date(steps_day$Date)
  
  plot1 <- (steps_day)

  ggplot(steps_day, aes(x=Date, y=Steps)) + geom_bar(stat="identity") + 
    labs(x="Day", y="Steps")

  plot1 <- (steps_day$Steps)
  
  hist(plot1, breaks=12, main="Steps Each Day",xlab="Steps", col="red")
  rug(plot1,side=1)

  print(mean(steps_day$Steps))
  print(median(steps_day$Steps))
  
  steps_interval_mean <- aggregate(data_no_NA$steps, by = list(data_no_NA$interval), FUN=mean, na.rm=TRUE)
  steps_interval_mean <- data.table(steps_interval_mean)
  setnames(steps_interval_mean, "Group.1", "Interval")
  setnames(steps_interval_mean, "x", "Steps_mean")
  
  with(steps_interval_mean, plot(Interval, Steps_mean, main = "Average Steps At Each Interval", type = "l", xlab = "(5-min. Intervals)", ylab = "(Average Steps)"))
  
  interval_max_mean <- filter(steps_interval_mean, steps_interval_mean$Steps_mean == max(steps_interval_mean$Steps_mean))
  interval_max_mean <- data.table(interval_max_mean)
  print(interval_max_mean)
  
  row_with_NA <- data[!row_has_na,]
  print(nrow(row_with_NA))
  
  idata <- sqldf(" SELECT data.date, data.date_time, data.interval, data.steps, steps_interval_mean.Steps_mean
                   FROM data 
                   LEFT OUTER JOIN steps_interval_mean 
                   ON data.interval = steps_interval_mean.Interval
                 ")
  idata$Steps_mean <- as.integer(idata$Steps_mean+0.5)
  
  sqldf()
      fn$sqldf("UPDATE idata SET steps = Steps_mean WHERE steps IS NULL")
      idata <- sqldf("select * FROM main.idata")
  sqldf()

  isteps_day <- aggregate(idata$steps, by = list(idata$date), FUN=sum, na.rm=TRUE)
  isteps_day <- data.table(isteps_day)
  setnames(isteps_day, "Group.1", "Date")
  setnames(isteps_day, "x", "Steps")
  
  isteps_day$Date <- as.Date(isteps_day$Date)
  
  plot1 <- (isteps_day$Steps)
  hist(plot1, breaks=12, main="Imputed Steps Each Day",xlab="Imputed Steps", col="red")
  rug(plot1,side=1)
  
  print(mean(isteps_day$Steps))
  print(median(isteps_day$Steps))
  
  plot(isteps_day$Date, isteps_day$Steps, type="n", xlab="Date", ylab="Steps", cex.lab=".75", main = "Imputed Overlay to Non Imputed Steps Per Interval")
  points(isteps_day$Date, isteps_day$Steps, type="l", col="red")
  points(steps_day$Date, steps_day$Steps, type="l", col="blue")
  legend("topleft",legend = c("Imputed Steps", "Non Imputed Steps"), col = c("red", "blue"), lwd = .75, cex = .65, bty="n")
  
  
  isteps_interval_mean <- aggregate(idata$steps, by = list(idata$interval), FUN=mean, na.rm=TRUE)
  isteps_interval_mean <- data.table(isteps_interval_mean)
  setnames(isteps_interval_mean, "Group.1", "Interval")
  setnames(isteps_interval_mean, "x", "Steps_mean")
  i_interval_max_mean <- filter(isteps_interval_mean, isteps_interval_mean$Steps_mean == max(isteps_interval_mean$Steps_mean))
  i_interval_max_mean <- data.table(i_interval_max_mean)
  print(i_interval_max_mean)
  
  idiff <- sqldf(" SELECT data.date, data.interval, data.steps, steps_interval_mean.Steps_mean
                   FROM data 
                   LEFT OUTER JOIN steps_interval_mean 
                   ON data.interval = steps_interval_mean.Interval
                 ")
  idiff$Steps_mean <- as.integer(idiff$Steps_mean+0.5)

  sqldf()
  fn$sqldf("UPDATE idiff SET Steps_mean = 0 WHERE steps IS NOT NULL")
  idiff <- sqldf("select * FROM main.idiff")
  sqldf()
  
  idiff_day <- aggregate(idiff$Steps_mean, by = list(idiff$date), FUN=sum)  
  idiff_day <- data.table(idiff_day)
  setnames(idiff_day, "Group.1", "Date")
  setnames(idiff_day, "x", "Steps")
  idiff_day$Date <- as.Date(idiff_day$Date)
  g <- ggplot(data=idiff_day, aes(x=Date, y=Steps, fill=Steps))
  p <- g + geom_bar(stat='identity', Fill = "red")  
  p <- p + labs(y="Steps", title = "Imputed Steps Added In Per Day")
  print(p) 
  
  idata$date_time <- sprintf("%04d", idata$interval)
  idata$date_time <- as.character(sub("(?<=.{2})", ":", idata$date_time, perl=TRUE))
  idata$date_time <- as.character(sub("(?<=.{5})", ":00", idata$date_time, perl=TRUE))
  idata$date_time <- paste(idata$date, idata$date_time, sep = " ")  
  idata$date_time <- ymd_hms(idata$date_time)
  idata$date <- as.Date(idata$date)
  
  
  
  idata$wend <- as.factor(ifelse(weekdays( idata$date_time) %in% c("Saturday","Sunday"), "Weekend", "Weekday"))
  isteps_interval_mean_wk <- aggregate(idata$steps, by = list(idata$interval, idata$wend), FUN=mean, na.rm=TRUE)
  setnames(isteps_interval_mean_wk, "Group.1", "Interval")
  setnames(isteps_interval_mean_wk, "x", "Steps_mean")
  setnames(isteps_interval_mean_wk, "Group.2", "Weekday")
  
  isteps_interval_mean_wk$Steps_mean <- as.integer(isteps_interval_mean_wk$Steps_mean+0.5)
  
  g <- xyplot(Steps_mean ~ Interval | Weekday, data = isteps_interval_mean_wk, type = "l" , xlab="Time of day", ylab="Average steps", 
              scales=list( Steps_mean=list(tick.number=5) ),layout = c(1,2),
              main="Average steps taken per day"
  )