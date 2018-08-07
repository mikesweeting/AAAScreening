################################################################################
# Utilities and subroutines for SWAN. 
# This contains both completely general functions that might be useful outside 
# SWAN and ones that are definitely only useful in SWAN.

#@ utils.R was previously called swanSubroutines.R. 

################################################################################
# Given the dimension-names (the names of the dimensions as well as the names 
# of the specific "rows"/"columns" for each dimension), make a 
# multi-dimensional array that has properly named dimensions etc. and is full 
# of numeric NAs. This is much cleaner and easier to use than "array".

# If you want to use initialValue, then this has to be a named argument. 
# The user is unable to make there be a dimension with name "initialValue". 

makeArray <- function(..., initialValue) {
	if (missing(initialValue)) initialValue <- NA_real_
	args <- list(...)
	if (length(args)==1) {
		if (!is.list(args[[1]]))
			stop("if there is only one argument apart from initialValue then ", 
					"it must be a list")
		dimNamesList <- args[[1]]
	} else {
		dimNamesList <- args
	}
	#cat("dimNamesList:\n"); show(dimNamesList)
	array(initialValue, dim=sapply(dimNamesList, length), dimnames=dimNamesList)
}

# For examples see testSwanSubroutines.R.

################################################################################
# Generate FALSE or TRUE with probability pr. 
rbernoulli <- function(pr) {
	as.logical(rbinom(n=1, size=1, prob=pr))
}

################################################################################
# Find whether elements of a vector (apart from NAs) are all different or not. 
allDifferent <- function(x) {
	if (!is.vector(x)) stop("x must be a vector")
	x <- na.omit(x)
	length(x) == length(unique(x))
}

################################################################################
# Like get, but this works if you pass it a string like x$y or x[y]. 
# This is only used if v0$showEventHistories, so it is not very important. 
# There might be more proper ways of doing this using expressions etc. 

# Things to do, to make this function better:
# - Make it work with x[[y]] or strings with more than one 
#   $ or [ in. At present, it tests for those and stops if it finds them. 
# - Test that x has a legal form.
# - Make this function work without dealing with environments.
# - If this goes wrong, then it leaves a variable calle  
#   temporaryVariableForGetAnything in parent.frame(), and that means that 
#   when you next run this function it will not work. 
#   Really, I should not use temporaryVariableForGetAnything at all!
#   "temporaryVariableForGetAnything" is hard-coded many times in this 
#   function. This is because otherwise you would have to assign loads of 
#   things in parent.frame() (which is the environment that this function 
#   gets called from). And I am already assigning 
#   temporaryVariableForGetAnything there, which is messy enough. 

getAnything <- function(x) {
	if (!is.character(x) || length(x) != 1) 
		stop("x must be a single character string")
	if (exists("temporaryVariableForGetAnything", envir=parent.frame()))
		stop("parent.frame() already contains a variable called ",
				"temporaryVariableForGetAnything")
	cat("### getAnything(", x, ")\n", sep="")
	
	separateLetters <- strsplit(x, NULL)[[1]]
	
	if (length(which(separateLetters == "$")) +
			length(which(separateLetters == "[")) > 1)
		stop("x is not allowed to contain more than one $ or [")
	
	if ("$" %in% separateLetters) {
		xElements <- regmatches(x, regexec(
				"^(\\w+)\\$(\\w+)$", x))[[1]][-1]
		assign("temporaryVariableForGetAnything", xElements, 
				envir=parent.frame())
		result <- with(parent.frame(), 
				getElement(get(temporaryVariableForGetAnything[1]), 
				temporaryVariableForGetAnything[2]))
		rm(temporaryVariableForGetAnything, envir=parent.frame())
		return(result)	
		
	} else if ("[" %in% separateLetters) {
		xElements <- regmatches(x, regexec(
				"^(\\w+)\\[\"(\\w+)\"\\]$", x))[[1]][-1]
		assign("temporaryVariableForGetAnything", xElements, 
				envir=parent.frame())
		result <- with(parent.frame(), 
				getElement(get(temporaryVariableForGetAnything[1]), 
				temporaryVariableForGetAnything[2]))
		rm(temporaryVariableForGetAnything, envir=parent.frame())
		return(result)	
		
	} else {
		cat("x does not contain $ or [\n")
		return(get(x, envir=parent.frame()))
	}
}

# The following does get(varName), but works if varName is just a plain string 
# or if it is has the form x["y"]: 
## 
## varNameElements <- regmatches(varName, regexec(
##       "^(\\w+?)(?:\\[\"(\\w+)\"\\])?$", varName))[[1]][-1]
## if (varNameElements[1] == varName) {  # varName has no []
##     var <- get(varName)
## } else {  # e.g. varNameElements = c("x", "y")
##     var <- getElement(get(varNameElements[1]), varNameElements[2])

################################################################################
# For debugging parallel code: write text to a file. 
# (In parallel code, you can't use cat, print, browser, etc.)

writeText <- function(...) {
	cat(..., "\n", file=file.path("output", "temp_writeTextOutput.txt"), 
			append=TRUE, sep="")
}

################################################################################
# Calculate the sizes of batches. For example, if numberOfThings = 10,000 and 
# numberOfBatches = 4, return 2500, 2500, 2500, 2500. This does not work
# like it ideally should, but it will do.

calculateBatchSizes <- function(numberOfThings, numberOfBatches) {
	sizeOfMostBatches <- round(numberOfThings / numberOfBatches)
	sizeOfFinalBatch <- numberOfThings - sizeOfMostBatches * (numberOfBatches-1)
	c(rep(sizeOfMostBatches, numberOfBatches - 1), sizeOfFinalBatch)
}

################################################################################
# A replacement for flexsurv::rgompertz, which is slow. 

myRgompertz <- function(n, shape, rate) {
	# This uses the same parameterization as flexsurv::rgompertz.
	# The code is partly based on qgompertz, which is called by rgompertz. 
	# The inverse of the cumulative density function is
	# log(1 - shape/rate * log(x)) / shape
	# But if shape < 0 then Inf has positive probability (see ?rgompertz).
	
	if (class(shape) != "numeric" || class(rate) != "numeric" || 
			length(shape) != 1 || length(rate) != 1 || rate <= 0)
		stop("shape and rate must be single numerics, and rate must be positive")
	if (is.na(shape) || is.na(rate))
		stop("shape and rate must not be NA or NaN")
	
	u <- runif(n)
	result <- numeric(n)
	isImmortal <- shape < 0 & u > 1 - exp(rate/shape)
	result[isImmortal] <- Inf
	result[!isImmortal] <- log1p(-shape/rate * log1p(-u[!isImmortal])) / shape
	# The following check has been removed because the error never arose:
	#if (any(is.nan(result))) 
	#	stop("myRgompertz result contains one or more NaNs")
	result
}

################################################################################
# A replacement for rbeta. If shape1=1 and shape2=0 then R 2.15.3 gives NaN, 
# whereas it should give 1 like R 3.X.X (I think). 
# NB this might not work if n, shape1, and shape2 have different lengths. 
#
# The reason for writing this was to make everything work in R 2, 
# which someone recommended as a way to avoid the segfault. 

myRbeta <- function(n, shape1, shape2) {
	if (R.version$major == "2") {
		shouldBeOne <- shape1==1 & shape2==0
		shouldBeZero <- shape1==0 & shape2==1
		others <- !shouldBeOne & !shouldBeZero
		result <- numeric(n)
		result[shouldBeOne] <- 1
		#result[shouldBeZero] <- 0  # unneeded as they are already zero
		result[others] <- rbeta(sum(others), shape1[others], shape2[others])
		## for (v in c("shouldBeOne", "shouldBeZero", "others")) {
		##     cat(v, ":\n", sep="")
		##     print(get(v))
		## }
		return(result)
	} else {
		return(rbeta(n, shape1, shape2))
	}
}

################################################################################
# Display a number of seconds in a more readable form. 
# A better function might exist elsewhere. 

displayTime <- function(t) {
	days <- floor(t / 86400)
	t2 <-  (t %% 86400)
	hours <- floor(t2 / 3600)
	t2 <- t2 %% 3600
	minutes <- floor(t2 / 60)
	seconds <- t2 %% 60
	
	result <- ""
	if (days > 0)
		result <- paste0(days, " days, ")
	if (days > 0 || hours > 0)
		result <- paste0(result, hours, " hours, ")
	if (days > 0 || hours > 0 || minutes > 0)
		result <- paste0(result, minutes, " minutes, ")
	if (t >= 60) {
		result <- paste0(result, sprintf("%.2f",seconds), " seconds")
		result <- paste0(result, " (", t, " seconds)")
	} else {
		result <- paste0(result, sprintf("%.2f",seconds), " seconds")
	}
	result
}

################################################################################
# A function to set the random seed whether the subsequent code is going to be
# serial or parallel, and display a message about it; and if randomSeed is NULL
# then it does nothing and just displays a message. 

setAndShowRandomSeed <- function(randomSeed, cluster, verbose=TRUE) {
	if (is.null(randomSeed) || is.na(randomSeed)) {
		message <- paste0("Random seed has not been set (because randomSeed=",
				{if (is.null(randomSeed)) "NULL" else randomSeed}, ").")
	} else {
		if (missing(cluster)) {
			# Not parallel, so just use set.seed.
			set.seed(randomSeed, kind="default") 
			message <- paste0("Random seed has been set by set.seed(", 
					randomSeed, ").")
		} else {
			# Parallel, so use clusterSetRNGStream instead.
			if (!inherits(cluster, "cluster")) 
				stop("cluster must be a valid cluster")  # for safety
			clusterSetRNGStream(cl=cluster, iseed=randomSeed)
			message <- paste0(
					"Random seed has been set by clusterSetRNGStream(iseed=", 
					randomSeed, ").")
		}
	}
	if (verbose) cat(message, "\n")
}
# TODO: the functions to set the random seed use default methods; so if someone 
# on a different machine has set the method of generating random numbers to 
# something different, using options, then they will get different results. 
# Ideally, everything would give exactly the same results on any computer.

################################################################################
# Print the contents of a text-file. 

printTextFileContents <- function(fileName) {
	if (!file.exists(fileName)) stop(fileName, " does not exist")
	cat("=============================================================\n")
	cat("======= ", fileName, ": =======\n")
	cat(paste(readLines(fileName), collapse="\n"))
	cat("=============================================================\n")
	invisible(fileName)
}

################################################################################
# Show memory use. This was written while trying to fix the segfault.

showMemoryUseEtc <- function(message="[no message]") {
	timeTaken <- system.time({
		cat("\n### showMemoryUseEtc (", message, ")\n", sep="")
		
		cat("### Largest objects:\n")
		sizes <- numeric()
		# Set sizes[varName] using object.size, but for "missing" objects use NA.
		# ("missing" objects appear in "ls", but you can't use "get" on them.)
		for (varName in ls(parent.frame())) 
			sizes[varName] <- tryCatch(
				object.size(get(varName, envir=parent.frame())),
				error=function(e) { if(grepl(
					"^argument \"\\w+\" is missing, with no default$", 
					e$message)) NA else stop(e$message) })
		print(head(sort(sizes, decreasing=TRUE)))
		
		# Previous attempt: print(head(
		# sort(sapply(mget(ls(), envir=?, object.size), decreasing=T)))
		# The previous line works for the current environment if 
		# envir=as.environment(-1). But I need envir=parent.frame(), which fails. 
		# It probably also fails if ls() is empty. 
		# envir arg to mget is necessary in R 2.15.3 and has no effect in R 3.
		
		cat("### Cstack_info():\n")
		print(Cstack_info())
		cat("### gc(verbose=TRUE):\n")
		sink(file=stdout(), type="message") # divert stderr to stdout ...
		print(gc(verbose=TRUE))  # ... so that the output from this looks normal
		sink(type="message")  # reset the stderr diversion
	})
	cat("### showMemoryUseEtc took ", round(timeTaken["elapsed"], 2), 
			" seconds.\n\n", sep="")
}

################################################################################
# This function was used when trying to debug to fix the segfault:

setJitCompilation <- function(level) {
	if (missing(level) || !(level %in% 0:3)) return(invisible())
	require(compiler, quietly=TRUE)
	enableJIT(level)
	cat("enableJIT(", level, ") has been done.\n", sep="")
}
# TODO: deal with the messages like 
#   Note: no visible binding for global variable ....
#   Note: no visible global function definition for ...
# that appear when you do enableJIT(3). 

################################################################################
# Display basic information such as what version of R is being used. 

showRversionEtc <- function() {
	cat("Information from R.Version() and Sys.info():  Rversion=", 
			paste0(R.Version()$major, ".", R.Version()$minor), "\n", sep="")
	si <- Sys.info()
	cat("  nodename=", si["nodename"], "  sysname=", si["sysname"], 
			"  release=", si["release"], "  machine=", si["machine"], 
			"\n", sep="")
}

################################################################################
# Merge two sorted vectors. This assumes that x and y are sorted.
# You could just do sort(c(x,y)), but that is slow and this is hopefully faster. 
# 
# Each element of x is inserted into y. So it is expected that x is shorter  
# than y, and in fact it is pretty much assumed that x is qalyFactorBoundaries 
# and y is discountTimes. 

# To get a[(b+1):c], but an empty vector if b = c, type: a[seq_len(c - b) + b].

## # A version that uses an explicit loop in R (probably discarded because it 
## # was slower):
## mergeSortedVectors <- function(x, y) {
##     indexes <- findInterval(x, y)
##     for (i in seq_along(indexes)) {
##         index <- indexes[i] + i - 1
##         indexesBeforeInsertion <- seq_len(index)
##         indexesAfterInsertion <- seq_len(length(y) - index) + index
##         y <- c(y[indexesBeforeInsertion], x[i], y[indexesAfterInsertion])
##     }
##     y
## }

# A version that doesn't use an explicit loop in R:
mergeSortedVectors <- function(x, y) {
	if (length(x) == 0) {
		return(y)
	} else if (length(x) == 1) {
		index <- findInterval(x, y)
		firstSection <- y[seq_len(index)]
		lastSection <- y[seq_len(length(y) - index) + index]
		return(c(firstSection, x, lastSection))
	} else {
		indexes <- findInterval(x, y)  # one index for each element of x
		firstSection <- y[seq_len(indexes[1])]
		# middleSection is all elements of x except the last one, plus the 
		# stretches of y that follow them:
		middleSection <- unlist(lapply(X=seq_len(length(x)-1), FUN=function(i) { 
				c(x[i], y[seq_len(indexes[i+1] - indexes[i]) + indexes[i]]) }))
		lastSection <- y[seq_len(length(y) - indexes[length(indexes)]) +
				indexes[length(indexes)]]
		return(c(firstSection, middleSection, x[length(x)], lastSection))
	}
}

################################################################################
# Two crude functions for temporarily hiding objects. Actually they are renamed.
# Example: "hideObjects(a, b, c)"; then later "unhideObjects()".
# 
# There are probably better ways of doing this and these functions should not 
# be relied on.

hideObjects <- function(...) {  # (pass the objects themselves as arguments)
	objects <- list(...)
	names(objects) <- as.list(match.call())[-1]
	for (objectName in names(objects)) {
		newName <- paste0("HIDDEN_OBJECT__", objectName)
		cat("Renamed ", objectName, " to ", newName, ".\n", sep="")
		assign(newName, objects[[objectName]],
				envir=parent.frame())
		rm(list=objectName, envir=parent.frame())
	}
}

unhideObjects <- function() {
	objectNames <- grep("^HIDDEN_OBJECT__", ls(parent.frame()), value=TRUE) 
	if (length(objectNames) == 0) 
		cat("There are no hidden objects to unhide.\n")
	for (objectName in objectNames) {
		newName <- sub("^HIDDEN_OBJECT__", "", objectName)
		assign(newName, get(objectName, envir=parent.frame()), 
				envir=parent.frame())
		rm(list=objectName, envir=parent.frame())
		cat("Renamed ", objectName, " to ", newName, ".\n", sep="")
	}
}

################################################################################
# If x is numeric or a character string that can be converted to numeric, then 
# round it off. Otherwise, return it unchanged. 

roundIfNumeric <- function(x, digits) {
	if (is.numeric(type.convert(as.character(x)))) {
		return(round(as.numeric(x), digits=digits))
	} else {
		return(x)
	}
}

################################################################################
# Given some numbers, paste them together in a string with proper table-like 
# spacing, using an arbitrary format string (e.g. "%10.4f") for each one. 

makeStringFromNumbers <- function(numbers, formatString) {
	formatString <- rep(paste0(formatString, " "), length(numbers))
	paste(sprintf(formatString, numbers), collapse="")
}

################################################################################
# A function for showing how v1other$aortaDiameterThresholds etc. have been 
# changed. This is used by menNewAaaDefinition.R. 

showAortaDiameterThresholdsEtc <- function(v1other) {
	cat("\nAorta diameter thresholds and monitoring intervals:",
			"\n  v1other$aortaDiameterThresholds:          ", 
			paste(v1other$aortaDiameterThresholds, collapse="  "), 
			"\n  v1other$monitoringIntervals:              ", 
			paste(v1other$monitoringIntervals, collapse="  "), 
			"\n  v1other$thresholdForIncidentalDetection:  ",
			{ if(is.null(v1other$thresholdForIncidentalDetection)) "NULL" else 
			v1other$thresholdForIncidentalDetection }, "\n\n", sep="")
}

################################################################################
# Given extremeValue, which is the max or min value that is going to be plotted 
# on a graph, round it up to the nearest sensibleUnit (or down if 
# roundUp=FALSE). The result will be used as one element of xlim or ylim in a 
# call to plot or a similar function.

roundForAxisRange <- function(extremeValue, sensibleUnit, roundUp) {
	# Check the arguments.
	if (missing(extremeValue) || is.null(extremeValue) || is.na(extremeValue) ||
			!is.numeric(extremeValue) || length(extremeValue) != 1)
		stop("extremeValue must be a single numeric")
	if (missing(sensibleUnit) || is.null(sensibleUnit) || is.na(sensibleUnit) ||
			!is.numeric(sensibleUnit) || length(sensibleUnit) != 1)
		stop("sensibleUnit must be a single numeric")
	if (missing(roundUp) || is.null(roundUp) || is.na(roundUp) ||
			!is.logical(roundUp) || length(roundUp) != 1)
		stop("roundUp must be a single logical/boolean")
	
	# Round extremeValue up to the nearest sensibleUnit (down if roundUp=FALSE).
	if (roundUp) {
		return(ceiling(extremeValue / sensibleUnit) * sensibleUnit)
	} else {
		return(floor(extremeValue / sensibleUnit) * sensibleUnit)
	}
}

# Get a sensible range for plotting values. The result will be used as xlim or 
# ylim in a call to plot or a similar function. 
getSensibleRange <- function(values, sensibleUnit, includeZero) {
	# Check the arguments.
	if (missing(values) || is.null(values) || all(is.na(values)) ||
			!is.numeric(values) || length(values) == 0)
		stop("values must be a vector of numerics of length 1 or more")
	if (missing(sensibleUnit) || is.null(sensibleUnit) || is.na(sensibleUnit) ||
			!is.numeric(sensibleUnit) || length(sensibleUnit) != 1)
		stop("sensibleUnit must be a single numeric")
	if (missing(includeZero) || is.null(includeZero) || is.na(includeZero) ||
			!is.logical(includeZero) || length(includeZero) != 1)
		stop("includeZero must be a single logical/boolean")
	
	# Find the range and return it.
	if (includeZero) {
		minValue <- min(0, 
				roundForAxisRange(min(values), sensibleUnit, roundUp=FALSE))
		maxValue <- max(0, 
				roundForAxisRange(max(values), sensibleUnit, roundUp=TRUE))
	} else {
		minValue <- roundForAxisRange(min(values), sensibleUnit, roundUp=FALSE)
		maxValue <- roundForAxisRange(max(values), sensibleUnit, roundUp=TRUE)
	}
	return(c(minValue, maxValue))
}


################################################################################
getFirstChar <- function(string) { substr(string, 1, 1) }

################################################################################
changeFirstLetterToLowerCase <- function(string) {  # e.g. XxxYyy
	firstLetter <- substr(string, 1, 1)
	rest <- substr(string, 2, nchar(string))
	paste0(tolower(firstLetter), rest)             # e.g. xxxYyy
}

changeFirstLetterToUpperCase <- function(string) {  # e.g. xxxYyy
	firstLetter <- substr(string, 1, 1)
	rest <- substr(string, 2, nchar(string))
	paste0(toupper(firstLetter), rest)             # e.g. XxxYyy
}

################################################################################
hasNames <- function(x) {
	return(!is.null(names(x)))
}

################################################################################
# Check that x is a single numeric (and not missing, NA, etc.). 

checkIsSingleNumeric <- function(x, insistOnNoName=FALSE) {
	if (!isSingleNumeric(x)) {
		cat("x must be a single numeric, but x=")
		print(x)
		stop("x must be a single numeric; ",
				"use traceback() to find the problem")
	}
	if (insistOnNoName && hasNames(x))
		stop("x must be unnamed, but names(x)=", names(x))
}

# TODO: use checkIsSingleNumeric in more places, to decrease 
# the amount of code. Search for is.numeric. 

isSingleNumeric <- function(x) {
	!missing(x) && !is.null(x) && length(x) == 1 && is.numeric(x) && !is.na(x)
}

# TODO: use checkIsMultipleNumeric in more places, to decrease 
# the amount of code. Search for is.numeric. 

isMultipleNumeric <- function(x) {
  !missing(x) && !is.null(x) && length(x) > 1 && is.numeric(x) && !is.na(x)
}


################################################################################
# Two very simple functions that calculate 0.025 and 0.975, if they are
# given 95. 

getLowerQuantile <- function(ciPercent) {
	if (missing(ciPercent) || is.null(ciPercent) || is.na(ciPercent) || 
			!is.numeric(ciPercent) || length(ciPercent) != 1 || 
			ciPercent < 1 || ciPercent > 100)
		stop("ciPercent must be a percentage, e.g. 95 (and greater than 1)")
	(100 - ciPercent) / 200
}

getUpperQuantile <- function(ciPercent) {
	1 - getLowerQuantile(ciPercent)
}

################################################################################



################################################################################
# A function for generating a time to non-AAA death when a person is 
# contraindicated. This just generates an exponential random variable. 

generateTimeToNonAaaDeathFromContraindication <- function(
		rateOfNonAaaDeathAfterContraindication) {
	rexp(n=1, rate=rateOfNonAaaDeathAfterContraindication)
}

# TODO: maybe most or all instances of rexp in the whole SWAN program should 
# be replaced by myRexp (see below), which gives NA if rate=0 (rexp gives NaN 
# and a warning if rate=0). 
#    This would mean that several functions might return NA (e.g.
# generatePostSurgeryAaaDeathTime). This might be OK if the results of those 
# functions are definitely only put into elements of eventTimes, 
# in generateEventHistory, since generateEventHistory can cope with eventTimes 
# having NA elements and simply ignores them when choosing the next event (see 
# "which.min" in generateEventHistory). 
#     But it would be necessary to check carefully that numbers produced by 
# rexp are never used in any way that would lead to an error or something
# unintended happening. 


################################################################################
# A function for generating a dropout time. This is just monitoringStartTime 
# plus an exponential random variable. 

generateDropoutTime <- function(rateOfDropoutFromMonitoring, 
		monitoringStartTime) {
	monitoringStartTime + rexp(n=1, rate=rateOfDropoutFromMonitoring)
}

################################################################################
# Set some elements of v0 to default values, if they have not already 
# been specified in v0. This is used by processPersons.

# (namesOfQuantities is stored in v0 so that it can easily be passed on 
# to processOnePair.)

setUnspecifiedElementsOfv0 <- function(v0) {
	require(parallel, quietly=TRUE)  # might not be necessary here any more
	# Create a list that contains the default values. 
	defaults <- list(
			treatmentGroups=c("noScreening", "screening"),
			namesOfQuantities=c("lifeYears", "qalys", "cost", 
					"discountedLifeYears", "discountedQalys", "discountedCost"),
			showEventHistories=FALSE,
			returnMeanQuantities=TRUE,
			returnEventHistories=FALSE,
			returnAllPersonsQuantities=FALSE,
			recordSizes=FALSE,
			method="parallel",  
			verbose=TRUE,
			randomSeed=2,
			numberOfPersons=1e3,
			numberOfParameterIterations=5
			# The default values of method and verbose are intended for 
			# processPersons; in psa it might be better to use different values
	)
	# Assign the default values, for all elements that are in defaults but not 
	# yet in v0.
	for (i in 1:length(defaults)) {
		varName <- names(defaults)[i]
		if (!(varName %in% names(v0)))
			v0[[varName]] <- defaults[[i]]
	}
	# Deal with numberOfProcesses separately, and only if v0$method is now not 
	# "serial". For example, if setUnspecifiedElementsOfv0 is being called by 
	# processPersons, and processPersons has been called by psa, then 
	# processPersons should be using v0$method="serial" and there is no need 
	# for v0$numberOfProcesses to be set. (The point of only setting 
	# v0$numberOfProcesses when it is going to be used, like this, is that it 
	# means getRecommendedNumberOfProcesses does not need to be included in 
	# functionsToExport, which is safer.) 
	if (v0$method != "serial" && !("numberOfProcesses" %in% names(v0)))
		v0$numberOfProcesses <- getRecommendedNumberOfProcesses()
	return(v0)
}

################################################################################
# Get all functions in the global environment. The output of this is passed to 
# parLapply, if v0$method="parallel", or foreach, if v0$method="foreach".
# 
# (It is a bit wasteful to pass functions to the parallel threads that are not 
# going to be used by them, but this is easier than manually maintaining the 
# list functionsToExport, which is what I used to do.)

getAllFunctionsAndStringsInGlobalEnv <- function() {
	result <- character()
	for (objectName in ls(envir=globalenv())) {
		obj <- get(objectName)
		if (is.function(obj) || is.character(obj))
			result <- c(result, objectName)
	}
	return(result)
}

## # OLDER:
## # Functions to export. This is used by processPersons, when 
## # method="parallel" or "foreach", and by psa. 
## 
## functionsToExport <- c("generatePersonAortaParameters",
##         "generateTimeTillNonAaaDeathFromSurvProbs",
##         "generateTimeTillNonAaaDeathWithNonIntegerStartAge",
##         "generateTimeTillNonAaaDeath",
##         "rbernoulli", "generateEventHistory", "makeEmptyEventHistory",
##         "allDifferent", "addEvent", "recordSize",
##         "randomTime", "getTimeAtGivenDiameter", 
##         "generateIncidentalDetectionTime", "generateDropoutTime",
##         "generateTimeToNonAaaDeathFromContraindication", 
##         "getAortaMeasurement", "getExactAortaMeasurement", 
##         "getExactInitialAortaSize",
##         "calculateHealthEconomicQuantities", 
##         "adjustLifeYears",
##         "processOnePair",
##         "myRgompertz", "myRbeta",
##         "compactList", "writeText", "checkIsEventHistory",
##         "convertThreeMonthProbToRate", "processPersons",
##         "setUnspecifiedElementsOfv0", "getMassSurvivalProbabilities",
##         "makeArray",
##         "untransformAortaGrowthPars",
##         "mergeSortedVectors", 
##         "checkArgs",
##         "generatePostSurgeryAaaDeathTime",
##         "convertMortalityRatesToSurvProbs",
##         "generateV2", "setAndShowRandomSeed",
##         "changeFirstLetterToLowerCase", "changeFirstLetterToUpperCase",
##         "getBinaryVariable", "calculateProbFromLogisticModel",
##         "checkIsSingleNumeric", "isSingleNumeric", "hasNames",
##         "setType", "getType"
## )

################################################################################
# Get the recommended number of processes. 

getRecommendedNumberOfProcesses <- function() {
	return(20)  # for reproducibility
	# TODO: consider changing this to 32, given the new Cardio queuing system
	# You could use detectCores(), but that varies on different computers and 
	# therefore breaks the reproducibility.
}

################################################################################
# In Kim et al 2007, the numbers in the third column of Table 1 are 
# probabilities. For the ones that have been converted from rates (see p34 of 
# Kim 2006), it is necessary to convert back to rates, since this individual 
# patient / discrete event simulation uses rates where appropriate. 

# Convert from a three-month transition probability to a rate.
# This is used in parsForMen4yearsValidation.R and parsForMen30yearsUpdated.R. 
# This is the inverse of the first equation on p34 of Kim 2006, with lambda
# replaced by lambda/4 as described in the sentence after that equation.  

convertThreeMonthProbToRate <- function(prob) {
	-4 * log(1 - prob)
}

################################################################################
# Functions to do with graphics, if you are saving them rather than displaying 
# them. On remote high-performance clusters you will normally have to 
# save them. The idea is that you run intializeSavingGraphics before any 
# lines that produce graphics and terminateSavingGraphics after the last one. 
# 
# png cannot be multi-page, but pdf obviously can. I think 
# intializeSavingGraphics copes OK with this by inserting numbers into 
# the file-names (by means of "_%d").  

intializeSavingGraphics <- function(runIdentifier, 
		deviceType=c("pdf", "svg", "png", "tiff")) {
	deviceType <- match.arg(deviceType)
	if (missing(runIdentifier))	runIdentifier <- "XXXXX"
	graphicsFileName <- file.path("output", paste0(runIdentifier, 
			{ if(deviceType == "pdf") "" else "_%d" }, "_graphics_", 
			format(Sys.time(), "%Y%m%d_%H%M%S"), ".", deviceType))
	# If you want to produce a PDF with 10+ pages, replace _%d with _%02d.
	switch(deviceType,
			pdf=pdf(file=graphicsFileName, width=9, height=12),
			svg=svg(file=graphicsFileName, width=9, height=12),
			png=png(file=graphicsFileName, width=900, height=1200),
			tiff=tiff(file=graphicsFileName, width=900, height=1200))
	cat("Graphics (if any) will be saved in ", graphicsFileName, "\n", sep="")
	invisible(NULL)
}

terminateSavingGraphics <- function() {
	tryCatch(invisible(dev.off()), error=function(e) { 
			cat("terminateSavingGraphics was run but did nothing.\n") })
}

################################################################################
# TODO: make sure that there is a consistent policy for how to deal with 
# parts of the overall model where there is a choice of sub-model. At present: 
# - v1other$electiveSurgeryAaaDeathMethod can be "instantDeathOnly" or 
#   "survivalModel"
# - v1other$nonAaaDeathMethod can be "mass", "onsIntegerStart", or 
#   "onsNonintegerStart";
# - generateV2 gets the "type" attribute of v1distributions$someElement, which 
#   might be "pars for betaThenConvertThreeMonthProbToRate" etc., and uses that 
#   to decide how to generate the relevant element of v2 (maybe this is 
#   different as it is not something chosen by the user of processPersons/psa);
# - there are probably one or two other sub-models as well.

################################################################################
# Save output from processPersons and psa. The purpose of this is so that 
# the main scripts can save just one file rather than two.
# 
# This decides what to save reasonably intelligently and gives objects sensible
# names so they are easy to identify when the .RData file is loaded. 
# For example if PSA was done then it will typically save 
# processPersonsResult, psaResult, v0processPersons, v0psa, v1other, 
# v1distributions, and v2.
# 
# The disadvantages of this are that the objects have to be renamed and checked 
# before saving, and this extra function is needed rather than just save(...).

saveMainAndPsaObjects <- function(processPersonsObjectsToSave, 
		psaObjectsToSave, runIdentifier, extraText) {
	# Check runIdentifier and create fileName. 
	if (missing(runIdentifier)) stop("runIdentifier must be given")
	fileName <- file.path("output", paste0(runIdentifier, "_",  
			{if (!missing(extraText)) paste0(extraText, "_")},
			format(Sys.time(), "%Y%m%d_%H%M%S"), ".RData"))

	# Get things from processPersonsResult.
	processPersonsResult <- processPersonsObjectsToSave$result
	v0 <- processPersonsObjectsToSave$v0
	v1other <- processPersonsObjectsToSave$v1other
	v2 <- processPersonsObjectsToSave$v2
	
	# Define savePsaObjectsToo and if it is true then create psaResult. 
	savePsaObjectsToo <- 
			!missing(psaObjectsToSave) && !is.null(psaObjectsToSave) 
	if (savePsaObjectsToo) psaResult <- psaObjectsToSave$psaResult
	
	# Decide whether to save a smaller file as well. The reason for this is that
	# if processPersonsResult contains eventHistories or allPersonsQuantities
	# then the file is likely to be very big and desktop computers will 
	# not be able to cope with the .RData file. 
	objectWhoseSizeToTest <- list(processPersonsObjectsToSave, 
			{if (savePsaObjectsToo) psaObjectsToSave else NULL})
	saveSmallerFileToo <- 
			object.size(objectWhoseSizeToTest) > 5e8 &&  # 5e8 = 500MB
			("eventHistories" %in% names(processPersonsResult) || 
			"allPersonsQuantities" %in% names(processPersonsResult) ||
			(savePsaObjectsToo && "eventHistoryLists" %in% names(psaResult)))
	if (saveSmallerFileToo) 
		# For smallerFileName just insert "_smaller" before ".RData":
		smallerFileName <- sub("\\.RData$", "_smaller\\.RData", fileName)
		
	# Save the file or files. 
	if (!savePsaObjectsToo) {
		# Make a vector of the names of the objects to save. 
		# This will be passed as the "list" argument to "save".
		namesOfObjectsToSave <- c("processPersonsResult", "v0", "v1other")

	} else {
		# processPersons and psa almost certainly used different v0 and may 
		# have used different v1other etc. as well.
		
		# Make appropriately named objects to save. 
		v0_usedByProcessPersons <- processPersonsObjectsToSave$v0
		v0_usedByPsa <- psaObjectsToSave$v0
		v1other_usedByProcessPersons <- processPersonsObjectsToSave$v1other
		v1other_usedByPsa <- psaObjectsToSave$v1other
		
		# See whether processPersons and psa used the same v1other, etc.
		# If they used the same v1other, then just save that as "v1other".
		# Otherwise, save them as "v1other_usedByProcessPersons" and the other.
		if (identical(v1other_usedByProcessPersons, v1other_usedByPsa)) {
			v1other <- v1other_usedByProcessPersons
			namesOfObjectsToSave <- c("processPersonsResult", "psaResult", 
					"v0_usedByProcessPersons", "v0_usedByPsa", "v1other")
		} else {
			cat("WARNING: processPersons and psa used different v1other.\n")
			namesOfObjectsToSave <- c("processPersonsResult", "psaResult", 
					"v0_usedByProcessPersons", "v0_usedByPsa", 
					"v1other_usedByProcessPersons", "v1other_usedByPsa")
		}
		
		# v1distributions is not used by processPersons, so:
		v1distributions <- psaObjectsToSave$v1distributions
		namesOfObjectsToSave <- c(namesOfObjectsToSave, "v1distributions")
	} 
	
	# Append v2 to namesOfObjectsToSave and save the file(s).
	namesOfObjectsToSave <- c(namesOfObjectsToSave, "v2")
	save(list=namesOfObjectsToSave, file=fileName)
	if (saveSmallerFileToo) {
		processPersonsResult$eventHistories <- NULL
		processPersonsResult$allPersonsQuantities <- NULL
		save(list=namesOfObjectsToSave, file=smallerFileName)
	}
	
	# Display a message or messages. 
	cat("Saved processPersons ", if (savePsaObjectsToo) "and psa ", 
			"output to ", fileName, "\n", sep="")
	if (saveSmallerFileToo) {
		cat("Also saved smaller file: ", smallerFileName, "\n", sep="")
		cat("The two files are identical except that the smaller one does not",
				"\n contain large elements such as ",
				"processPersonsResult$eventHistories.\n", sep="")
	}

}

################################################################################
# Functions for dealing with "type" attributes. 

getType <- function(x) {
	result <- attr(x, "type")
	if (is.null(result)) stop("attr(x, \"type\") is NULL")
	return(result)
}


setType <- function(x, typeAttr) {
	if (missing(typeAttr)) stop("typeAttr must not be missing")
	attr(x, "type") <- typeAttr
	return(x)
}

# If x already exists then you have to do x <- setType(x, y), which is almost 
# as long as attr(x, "type") <- y. (NB obviously you cannot just do 
# setType(x, "aaa"), as that will not achieve anything.) 
# But if x does not yet exist then you can create it in one line with 
# x <- setType(0.3, "probability")


################################################################################
# A replacement for rexp that gives NA if rate=0. (rexp gives NaN and a warning)

myRexp <- function(n, rate=1) {
	if (length(rate) != 1) stop("rate must have length 1")
	if (rate == 0) {
		return(rep(NA, n))
	} else {
		return(rexp(n, rate))
	}
}

################################################################################
namesOfProbVarsForSurvivalModel <- 
		c("probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery",
		"probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery",
		"probOfAaaDeathInInitialPeriodAfterEmergencyOpenSurgery",
		"probOfAaaDeathInInitialPeriodAfterEmergencyEvarSurgery")

################################################################################

