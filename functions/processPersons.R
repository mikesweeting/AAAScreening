################################################################################
# processPersons - generate a set of persons and analyze them. 
# This is the "main analysis". It creates and 
# analyzes a large number of persons, either in parallel or serially.
# You (the user) can run this, or you can run psa, which runs this many times. 

#@ processPersons.R was previously called processPatients.R. 

# The arguments to processPersons are all the global variables, both fixed 
# (v1distributions and v1other) and uncertain (v0) (for the uncertain ones, 
# the arguments obviously have to be specific values), as well as the "user 
# parameters" that are stored in v0. 

# This returns a list that contains up to three elements, namely
# meanQuantities, eventHistories, and allPersonsQuantities: 
# 
# - "meanQuantities" is a matrix that contains the mean life-years and other 
#   health-economic quantities for each treatment-group. This is the main output 
#   of processPersons and will be used when this function is called by psa. 
# 
# - "eventHistories" is a list that contains one element for each person; each 
#   of those elements is in turn a list with two elements (which are event-
#   histories), one for each treatment-group. This is used for validation of 
#   the model like in Kim & Thompson 2010.
# 
# - "allPersonsQuantities" is also a list that contains one element for each 
#   person, where each of those elements is in turn a list with two elements, 
#   one for each treatment-group. Each of those elements is a named vector that 
#   contains the quantities for that person and that treatment-group.  It is 
#   not certain whether this will be used. (Previous versions of this function 
#   stored all these quantities in a 3D array, called "personsQuantities",
#   instead of a list.) 
# 
# To specify which elements are included, use the logical/boolean variables 
# v0$returnMeanQuantities, v0$returnEventHistories, and 
# v0$returnAllPersonsQuantities.
# For example, if you want to see how many people die of AAA for a 
# particular set of global parameters, or ask any other question like that, 
# then you can set returnEventHistories=TRUE and then extract this information 
# from the thing that is returned. 

################################################################################
processPersons <- function(v0, v1other, v2, updateProgress=NULL) {
	
	
	cat("processPersons\n")
	cat("probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery:\n")
	print(v2$probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery)
	
	# Load packages, etc.
	suppressWarnings(suppressMessages(require(doParallel)))
	# suppressMessages is for the Revolution R message and suppressWarnings 
	# is for the warning that "foreach" was built under R version something.
	# doParallel loads parallel and foreach.
	
	# Set unspecified elements of v0 to default values, if necessary. 
	v0 <- setUnspecifiedElementsOfv0(v0)
	
	# Check the arguments.
	checkArgs(v0=v0, v1other=v1other, v2=v2)
	
	# Display some messages. 
	if (v0$verbose) {
		cat("Running processPersons on ", Sys.info()["nodename"], 
			" with:\n  numberOfPersons=", v0$numberOfPersons, 
			", method=", v0$method, { if(v0$method=="serial") "" else paste0(
			", numberOfProcesses=", v0$numberOfProcesses) }, "\n", sep="")
		if ("generateCensoringTime" %in% names(v0))
			cat("Censoring is being used, so life-years etc. will be calculated",
					"up to censoring times.\n")
		# Display all of v0, v1other, and v2 (so that the 
		# input pars appear in the same file as the output and you can easily 
		# see what analysis was done):
		cat("\n########## v0 ##########\n")
		print(v0)
		cat("########## v1other ##########\n")
		print(v1other)
		cat("########## v2 ##########\n")
		print(v2)
		cat("########################\n\n")
	}
	
	# Create v1other$nonAaaSurvProbs or nonAaaMortalityRates. 
	# (See also checkArgs.R.)
	if (v1other$nonAaaDeathMethod == "mass") {
		v1other$nonAaaSurvProbs <- getMassSurvivalProbabilities()
	} else if (v1other$nonAaaDeathMethod == "onsIntegerStart") { 
		v1other$nonAaaSurvProbs <- convertMortalityRatesToSurvProbs(
				v1other$startAge, v1other$nonAaaMortalityRatesFileName)
	} else if (v1other$nonAaaDeathMethod == "onsNonintegerStart") {
		v1other$nonAaaMortalityRates <- 
				readMortalityRatesFromFile(v1other$nonAaaMortalityRatesFileName)
	} else {
		stop("v1other$nonAaaDeathMethod=", v1other$nonAaaDeathMethod, 
				" is illegal")
	}
	
	# Copy v2$sigmaW into v2$ultrasoundMeasurementErrorSD.
	# This removes the need for v2$ultrasoundMeasurementErrorSD to be defined 
	# in the "input" files and also ensures that it has the appropriate 
	# distribution in PSA, i.e. the same value as v2$sigmaW. 
	v2$ultrasoundMeasurementErrorSD <- v2$sigmaW
	
	# Change the prevalence, if v2$prevalence exists.
	if ("prevalence" %in% names(v2)) {
		v2$baselineDiametersWithDesiredPrevalence <- 
			changePrevalence(baselineDiameters=v1other$baselineDiameters, 
			threshold=v1other$prevalenceThreshold, prevalence=v2$prevalence)
		if (v0$verbose)
			cat("Prevalence (in v2$baselineDiametersWithDesiredPrevalence) ",
				"has been\n changed to ", v2$prevalence, 
				", using threshold=v1other$prevalenceThreshold=",
				v1other$prevalenceThreshold, ".\n", sep="")
	} else {
		v2$baselineDiametersWithDesiredPrevalence <- v1other$baselineDiameters
		if (v0$verbose) cat("v2$prevalence does not exist, so \n",
				" v2$baselineDiametersWithDesiredPrevalence is just a copy of",
				" v1other$baselineDiameters.\n", sep="")
	}
	v2$baselineDiametersWithDesiredPrevalence <- setType(
			v2$baselineDiametersWithDesiredPrevalence, 
			"baseline diameters with desired prevalence")
	# NB if the user specifies v2$prevalence, then changePrevalence is done 
	# using threshold=v1other$aortaDiameterThresholds[1], not
	# v1other$thresholdForIncidentalDetection. (For the exact meaning of 
	# "threshold" see changePrevalence, though it is fairly obvious).
	# If you think that changePrevalence should be done using threshold=
	# v1other$thresholdForIncidentalDetection, or if you think that the user should 
	# be allowed or forced to specify threshold when they specify 
	# v2$prevalence, then the code above and elsewhere will need to change. 
	
	# Set v1other$thresholdForIncidentalDetection to 
	# v1other$aortaDiameterThresholds[1], if the former was not set. 
	if (!("thresholdForIncidentalDetection" %in% names(v1other))) {
		v1other$thresholdForIncidentalDetection <- 
				v1other$aortaDiameterThresholds[1]
		if (v0$verbose) cat("v1other$thresholdForIncidentalDetection was not ",
				"provided and so\n has been set to ",
				"v1other$aortaDiameterThresholds[1]=", 
				v1other$aortaDiameterThresholds[1], ".\n", sep="")
	}
	
	# Make a list to store the output in. 
	result <- list()
	if (v0$returnMeanQuantities)
		result$meanQuantities <- NA
	if (v0$returnEventHistories) result$eventHistories <- 
			lapply(X=1:v0$numberOfPersons, FUN=function(x) list())
	if (v0$returnAllPersonsQuantities) result$allPersonsQuantities <- 
			lapply(X=1:v0$numberOfPersons, FUN=function(x) list())
	# The lapply lines each make a list of length numberOfPersons, in which 
	# each element is an empty list.
	
	# Create and analyze the persons. Pass all variables to processOnePair 
	# as arguments, and for parallel methods make functions available to that 
	# function by exporting them explicitly. 
	if (v0$method == "serial") {
		# Do it using lapply. 
		setAndShowRandomSeed(v0$randomSeed, verbose=v0$verbose)
		resultForEachPerson <- lapply(X=1:v0$numberOfPersons, 
				FUN=processOnePair, v0, v1other, v2, updateProgress)
		
	} else if (v0$method == "parallel") {
		# Do it using parLapply. 
		cluster <- makeCluster(v0$numberOfProcesses)  # previously: outfile=""
		clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
		setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
				verbose=v0$verbose)
		resultForEachPerson <- parLapply(cl=cluster, X=1:v0$numberOfPersons,
				fun=processOnePair, v0, v1other, v2, updateProgress)
		stopCluster(cluster) 
		
	} else if (v0$method == "foreach") {
		if (!is.null(v0$randomSeed)) 
			stop("v0$randomSeed does not yet work with v0$method=\"foreach\"")
		# Do it using foreach and %dopar%. 
		registerDoParallel(cores=v0$numberOfProcesses)
		resultForEachPerson <- foreach(personNumber=1:v0$numberOfPersons,
				.export=getAllFunctionsAndStringsInGlobalEnv()) %dopar% {
			processOnePair(personNumber, v0, v1other, v2, updateProgress) 
		}
		stopImplicitCluster()  # (seems unreliable)	
		
	} else if (v0$method == "parallelBatches") {
		# Do it using parLapply and batches. 
		# This has not been used for some time and might not work! 
		# Work out the batch sizes:
		if (!("numberOfBatches" %in% names(v0))) 
			v0$numberOfBatches <- max(round(v0$numberOfPersons/1500),1)
		batchSizes <- calculateBatchSizes(
				v0$numberOfPersons, v0$numberOfBatches)
		if (v0$verbose) 
			cat("numberOfBatches=", v0$numberOfBatches, ", batchSizes=", 
					paste(head(batchSizes), collapse=" "), "\n", sep="")
		# Do the parallel computation:
		cluster <- makeCluster(v0$numberOfProcesses)
		clusterExport(cluster, getAllFunctionsAndStringsInGlobalEnv())
		setAndShowRandomSeed(randomSeed=v0$randomSeed, cluster=cluster, 
				verbose=v0$verbose)
		# OLD CODE
		#	resultForEachBatch <- parLapply(cl=cluster, X=1:v0$numberOfBatches,
		#fun=processBatchOfPersons, v0, v1other, v2, batchSizes, updateProgress)
		# NEW CODE FOR SHINY PROGRESS
		resultForEachBatch <- lapply(X=1:v0$numberOfBatches,
		                             FUN=processBatchOfPersons, v0, v1other, v2, batchSizes, updateProgress, cluster)
		
		# The Xth task is for processBatchOfPersons to analyze a batch of 
		# batchSizes[X] persons. 
		stopCluster(cluster)
		# Copy everything into resultForEachPerson:
		resultForEachPerson <- vector("list", v0$numberOfPersons)
		i <- 1
		timeSpentCopyingFromBatches <- system.time(
			for (j in seq_along(resultForEachBatch)) { 
				for (k in seq_along(resultForEachBatch[[j]])) {
					# Copy person number k in batch number j:
					resultForEachPerson[[i]] <- resultForEachBatch[[j]][[k]]
					i <- i + 1
				}
			}
		)["elapsed"]
		if (v0$verbose)
			cat("Time spent copying resultForEachBatch to resultForEach", 
				"Person: ", timeSpentCopyingFromBatches, " seconds\n", sep="")
	
	} else {
		stop("v0$method=", v0$method, " is illegal")
	}
	# TODO: if returnMeanQuantities=TRUE and the other two are FALSE, it might 
	# be better to put the mean quantities in a three-dimensional array straight
	# away, i.e. use parSapply(..., simplify="array") instead of parLapply. 
	
	# Get event-histories and person-specific quantities from 
	# resultForEachPerson and put it in result, if required. The lines with 
	# "<-" both get the information for both treatment-groups. 
	if (v0$returnEventHistories) 
		for (i in 1:v0$numberOfPersons) 
			result$eventHistories[[i]] <- 
					resultForEachPerson[[i]]$eventHistories
	if (v0$returnAllPersonsQuantities)
		for (i in 1:v0$numberOfPersons)
			result$allPersonsQuantities[[i]] <- 
					resultForEachPerson[[i]]$personQuantities
	
	# Calculate the means, if required. 
	if (v0$returnMeanQuantities) {
		result$meanQuantities <- makeArray(treatmentGroup=v0$treatmentGroups, 
				quantity=v0$namesOfQuantities)
		for (treatmentGroup in v0$treatmentGroups) {
			totals <- rep(0, length(v0$namesOfQuantities))
			for (i in 1:v0$numberOfPersons)
				totals <- totals + resultForEachPerson[[i]]$
						personQuantities[[treatmentGroup]]
			result$meanQuantities[treatmentGroup, ] <- 
					totals / v0$numberOfPersons
		}
	}
	
	if (v0$verbose) cat("processPersons is about to return an object of size ",
			format(object.size(result), unit="auto"), ".\n", sep="")
	return(result)
}

################################################################################
# Generate and analyze a batch of persons. 
# The idea of this function is that it might make the parallel computation 
# faster. 

processBatchOfPersons <- function(
		batchNumber, v0, v1other, v2, batchSizes, updateProgress, cluster) {
  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- "Calculating differences between groups"
    num <-batchNumber/(length(batchSizes))
    updateProgress(value=num,detail = text)
  }
	numberOfPersonsInThisBatch <- batchSizes[batchNumber]
	parLapply(cl=cluster, X=1:numberOfPersonsInThisBatch, fun=processOnePair, 
			v0, v1other, v2, updateProgress=NULL)
}

################################################################################




