################################################################################
# Functions for dealing with objects of S3 class "eventHistory". Obviously, 
# the idea is that eventHistory objects are only ever used or manipulated via 
# these functions (so "checkEventHistory" might not be essential). 

# This stores everything in vectors, and expands it one "row" at a time. 
# It was found that this is much faster than alternatives such as 
# storing each event-history as basically a data.frame or data.table. This 
# is probably because new events are added to the end of event-histories and 
# this is done many times. 

makeEmptyEventHistory <- function(recordSizes=FALSE) {
	if (!is.logical(recordSizes) || length(recordSizes) != 1)
		stop("recordSizes must be a single logical")
	if (recordSizes) {
		eventHistory <- list(events=character(), times=numeric(),
				trueSizes=numeric(), measuredSizes=numeric())
	} else {
		eventHistory <- list(events=character(), times=numeric())
	}
	class(eventHistory) <- c("eventHistory", "list")
	eventHistory
}

checkIsEventHistory <- function(obj) {
	if (!(inherits(obj, "eventHistory")))
		stop("obj must be an object of class eventHistory")
	# (Obviously this only checks the first element of "class(obj)".) 
}
	
getEvent <- function(eventHistory, eventNumber) {
	checkIsEventHistory(eventHistory)
	if (!is.numeric(eventNumber) || length(eventNumber) > 1)
		stop("eventNumber must be a single numeric")
	numberOfEvents <- length(eventHistory$events)
	if (eventNumber > numberOfEvents)
		stop("eventNumber=", eventNumber, " but eventHistory only contains ", 
				numberOfEvents, " events")
	eventHistory$events[eventNumber] 
}

addEvent <- function(eventHistory, event, time, trueSize=NA, measuredSize=NA) { 
	# Check some things:
	checkIsEventHistory(eventHistory)
	numberOfEvents <- length(eventHistory$events)
	if (numberOfEvents >= 1 && time < eventHistory$times[numberOfEvents]) 
		stop("the time of the new event must be >= all eventHistory times")
	# Add the new event:	
	if ("trueSizes" %in% names(eventHistory)) {
		varNames <- c("event", "time", "trueSize", "measuredSize")
	} else {
		varNames <- c("event", "time")
	}
	for (varName in varNames) {
		colName <- paste0(varName, "s") # e.g. varName="event", colName="events"
		eventHistory[[colName]] <- c(eventHistory[[colName]], get(varName))
	}
	eventHistory
}

recordSize <- function(eventHistory, trueSize, measuredSize) {
	# Record the true and measured sizes in the most recent event's row. 
	checkIsEventHistory(eventHistory)
	if (!("trueSizes" %in% names(eventHistory)))
		stop("eventHistory does not have a trueSizes column")
	if (!is.na(trueSize) && (!is.numeric(trueSize) || length(trueSize) > 1))
		stop("trueSize must be a single numeric (or NA)")
	if (!is.na(measuredSize) && 
			(!is.numeric(measuredSize) || length(measuredSize) > 1))
		stop("measuredSize must be a single numeric (or NA)")
	
	numberOfEvents <- length(eventHistory$events)
	eventHistory$trueSizes[numberOfEvents] <- trueSize
	eventHistory$measuredSizes[numberOfEvents] <- measuredSize
	eventHistory
}
	
checkEventHistory <- function(eventHistory) {
	checkIsEventHistory(eventHistory)
	if (!is.list(eventHistory))
		stop("eventHistory must be a list")
	if (length(eventHistory) != 2 && length(eventHistory) != 4)
		stop("eventHistory must have either 2 or 4 elements")
	if (!identical(names(eventHistory), c("events", "times")) && !identical(
			names(eventHistory), 
			c("events", "times", "trueSizes", "measuredSizes")))
		stop("names(eventHistory) must be \"events\" and \"times\"", 
				" (and, optionally, \"trueSizes\" and \"measuredSizes\"")
	# Other checking could also be done. 
	cat("eventHistory is AOK\n")
}
	
print.eventHistory <- function(x, ...) {
	checkIsEventHistory(x)
	numberOfEvents <- length(x$events)
	if (numberOfEvents == 0) {
		cat("[Empty event-history]\n")
	} else {
		cat("========= EVENT-HISTORY ==========")
		if ("trueSizes" %in% names(x)) cat(" (true / as measured)")
		cat("\n")
		for (i in 1:numberOfEvents) {
			writeTrueSize <- "trueSizes" %in% names(x) && !is.na(x$trueSizes[i])
			writeMeasuredSize <- 
					"measuredSizes" %in% names(x) && !is.na(x$measuredSizes[i])
			if (writeTrueSize && writeMeasuredSize) {
				sizesString <- sprintf("(%.2fcm / %.2fcm)", 
						x$trueSizes[i], x$measuredSizes[i])
			} else if (writeTrueSize) {
				sizesString <- sprintf("(%.2fcm / ---)", x$trueSizes[i])
			} else if (writeMeasuredSize) {
				sizesString <- sprintf("(--- / %.2fcm)", x$measuredSizes[i])
			} else {
				sizesString <- ""
			}
			cat(sprintf("%-27s %6.2f %s\n",
							x$events[i], x$times[i], sizesString))
			# ("aortaDiameterBelowThreshold" is 27 characters)
		}
		cat("==================================\n")
	}
	invisible(x)
}
	
################################################################################

