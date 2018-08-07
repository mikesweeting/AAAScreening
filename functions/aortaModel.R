################################################################################
# Models for aorta growth and rupture. 
# This contains functions only. 

# This model samples baseline diameter from a distribution, which may or may 
# not be weighted, then generates from a model with random slope. 
# It is a linear mixed model for log-diameter.

#@ See also aortaParsForMen4yearsValidation.R or similar files and 
#@ ISM_growth_and_rupture_inputs_080715.docx, attached to e-mail on 
#@ 2015-10-21. 

#@ Newer model, where generatePersonAortaParameters generates b1 conditional 
#@ on b0: see section 2 on page 3 of
#@ "Performance of linear mixed model based on weighting and unweighting2.docx",
#@ attached to e-mail from MS Wed 2016-12-07 09:39. 
#@ If generatePersonAortaParameters uses rnorm then probably this method is 
#@ being used; if it uses mvrnorm then it is not being used. 


# Generate person-specific aorta growth and rupture parameters. This should be 
# called from processOnePair, and the numbers returned by it should be 
# stored in v3. 

################################################################################
generatePersonAortaParameters <- function(v1other, v2) {  # (person not persons)
	# Check things.
	if (!("baselineDiametersWithDesiredPrevalence" %in% names(v2)))
		stop("v2 must contain baselineDiametersWithDesiredPrevalence")
	if (!("weight" %in% names(v2$baselineDiametersWithDesiredPrevalence))) 
		stop("v2$baselineDiametersWithDesiredPrevalence must have a ",
				"\"weight\" column")
	
	# Generate y0 (step 1 in Word doc).
	y0 <- sample(x=v2$baselineDiametersWithDesiredPrevalence$size, size=1, 
			prob=v2$baselineDiametersWithDesiredPrevalence$weight)
	
	## MS ADDED 05/02/17
	## b0 is set to log(y0) for y0<3
	## b1 is generated from a conditional 
	## distribution given b0 using rnorm 
	if(y0<3){
	  b0 <- log(y0)
	  mu <- v2$beta1 + v2$rho * v2$sigma1 / v2$sigma0 * (b0 - v2$beta0) ## mean of b1 given b0
	  sigma <- sqrt((1 - v2$rho ^ 2) * v2$sigma1 ^ 2) ## SD of b1 given b0
	  b1 <- rnorm(n=1, mean=mu, sd=sigma)
	} else {
	
	  # # Calculate muOfB and v (steps 2-3 in Word doc). 
	  sigmaSum <- v2$sigma0 ^ 2 + v2$sigmaW ^ 2
	  muOfB <- rbind(v2$beta0, v2$beta1) +
	    rbind(v2$sigma0^2, v2$rho * v2$sigma0 * v2$sigma1) *
	    (log(y0) - v2$beta0) / sigmaSum
	  v <- matrix(c(
	    v2$sigma0^2 * v2$sigmaW^2,
	    v2$rho * v2$sigma0 * v2$sigma1 * v2$sigmaW^2,
	    v2$rho * v2$sigma0 * v2$sigma1 * v2$sigmaW^2,
	    v2$sigma0^2 * v2$sigma1^2 * (1-v2$rho^2) + v2$sigma1^2 * v2$sigmaW^2
	  ), nrow=2) / sigmaSum
	  # (v is the same for all persons, so it might be better to calculate it
	  # just once, in processPersons, and store it in v2.)
	  
	  # # Generate b0 and b1. 
	  require(MASS, quietly=TRUE)
	  b <- mvrnorm(n=1, mu=muOfB, Sigma=v)
	  b0 <- b[1]
	  b1 <- b[2]
	  
	}
	
	# Generate the rupture-time. 
	ruptureTime <- myRgompertz(n=1, shape=v2$alpha * b1, 
			rate=exp(v2$gamma + v2$alpha * b0))
	
	# If v1other$zeroGrowthDiameterThreshold exists, and if the initial diameter 
	# is below this, then set b1 to zero and ruptureTime to NA (e.g. if your
	# initial diameter is below 2.0 cm, your aorta never grows or ruptures). 
	if ("zeroGrowthDiameterThreshold" %in% names(v1other) &&
			getExactInitialAortaSize(b0, b1) < 
			v1other$zeroGrowthDiameterThreshold) {
		b1 <- 0 	# see also generateIncidentalDetectionTime, which has to 
		# deal with b1=0 as a special case
		ruptureTime <- NA
	}
	
	#cat(b0, ", ", b1, ", ", ruptureTime, "\n", sep="", 
	#		file="personAortaParameters.txt", append=TRUE)
	return(list(b0=b0, b1=b1, ruptureTime=ruptureTime, 
			initialAortaSizeAsMeasured=y0))
	# y0 is returned because we are now using that as the measurement that is 
	# found when they are screened, if they are screened and don't have 
	# non-visualization
}


# The reason for putting aorta growth and rupture together in the same function 
# is that it makes it possible to pass around v1other, v2, etc., rather than things 
# like b0 and b1. The advantage of this is that if the model for aorta growth 
# and rupture changes then less code will have to change. The disadvantage is 
# that if you want to generate aorta growth parameters and rupture-time 
# separately, then this function will need to be split in two. 
# It might be better to extract generateY0 and one or two other functions from 
# generatePersonAortaParameters, but keep them in aortaModel.R. 


# Generate an aorta measurement - which can be made by ultrasound, ct, or 
# "exact". 
# This needs to be passed v3 (from which it uses b0 and b1), time, and method. 
# Depending on "method" it may also require the measurement-error standard 
# deviation (e.g. v2$ultrasoundMeasurementErrorSD, ctMeasurementErrorSD) and 
# v1distributions$extraDiameterForCtScan. 
getAortaMeasurement <- function(v3, time, measurementErrorSD, 
		method=c("ultrasound", "ct", "exact"), extraDiameterForCtScan) {
	# method must be "ultrasound" (the default), "ct", or "exact".
	# NB the precise meaning of measurementErrorSD depends on the method. 
	method <- match.arg(method) 
	if (xor(method=="ct", !missing(extraDiameterForCtScan)))
		stop("extraDiameterForCtScan must be given iff method=\"ct\"")
	if (method=="exact" && !missing(measurementErrorSD))
		stop("measurementErrorSD must not be given if method=\"exact\"")

	if (method=="ultrasound") {
		return(getExactAortaMeasurement(v3, time) *
				exp(rnorm(n=1, sd=measurementErrorSD)))
	} else if (method=="ct") {
		return(getExactAortaMeasurement(v3, time) +
				rnorm(n=1, sd=measurementErrorSD) +
				extraDiameterForCtScan)
	} else { 
		return(getExactAortaMeasurement(v3, time))
	}
	# TODO: getAortaMeasurement seems to return a number with the same 
	# name (e.g. "electiveSurgeryOpen") as the time argument. 
}

# Only to be used by getAortaMeasurement and generatePersonAortaParameters:
getExactAortaMeasurement <- function(v3, time) { 
	exp(v3$b0 + v3$b1 * time)
}

# getExactInitialAortaSize is used by generatePersonAortaParameters if 
# v1other$zeroGrowthDiameterThreshold exists. It might also be used by 
# generateEventHistory, depending on how generatePersonAortaParameters works.
getExactInitialAortaSize <- function(b0, b1) {
	# This just returns exp(b0) (unless getExactAortaMeasurement has changed)
	getExactAortaMeasurement(v3=list(b0=b0, b1=b1), time=0)
}

################################################################################
# Two subroutines that are used by generateIncidentalDetectionTime.
# getTimeAtGivenDiameter is the inverse of getExactAortaMeasurement:
getTimeAtGivenDiameter <- function(v3, diameter) {
	(log(diameter) - v3$b0) / v3$b1
}
# Generate time till incidental detection, given that the person is aneurysmal:
randomTime <- function(rateOfIncidentalDetection) {
	rexp(n=1, rate=rateOfIncidentalDetection)
}

# generateIncidentalDetectionTime. 
# This assumes the linear model for log-diameter for each person. 
# Previously, "thresholdForIncidentalDetection" was called 
# "thresholdForMonitoring" and was the same as aortaDiameterThresholds[1].
# Now it is not necessarily the same. 
generateIncidentalDetectionTime <- function(currentTime, 
		thresholdForIncidentalDetection, v3, rateOfIncidentalDetection) {
	# If v1other$zeroGrowthDiameterThreshold is used, b1 might be exactly zero:
	if (v3$b1 == 0) return(NA)  # see "getExactInitialAortaSize(...)" line above
	# Otherwise ...
	timeAtThreshold <- 
			getTimeAtGivenDiameter(v3, thresholdForIncidentalDetection)
	# There are four possibilities:
	if (timeAtThreshold <= currentTime && v3$b1 <= 0) 
		return(NA)
	if (timeAtThreshold <= currentTime && v3$b1 > 0) 
		return(currentTime + randomTime(rateOfIncidentalDetection))
	if (timeAtThreshold > currentTime && v3$b1 <= 0) {
		incidentalDetectionTime <- 
				currentTime + randomTime(rateOfIncidentalDetection)
		if (incidentalDetectionTime < timeAtThreshold) {
			return(incidentalDetectionTime)
		} else {
			return(NA)
		}
	}
	if (timeAtThreshold > currentTime && v3$b1 > 0) 
		return(timeAtThreshold + randomTime(rateOfIncidentalDetection))
}

#@ TODO: check the mathematical formulas in 
#@ ISM_growth_and_rupture_inputs_080715.docx, just to be safe (this means  
#@ the general matrix equations, which we use simplified scalar versions 
#@ of). [However, this might have been superseded, now that we are using 
#@ the "unweighted" aorta model i.e. the one based on the unmanipulated MASS 
#@ data.]

################################################################################
# Adjust the prevalence in a baseline diameter distribution. 
# "prevalence" means the proportion of people above "threshold". 
#@ For formulas etc. see e-mail 2016-06-02 17:23 and files attached to that. 
#@ But also see e-mail from EJ 2016-11-22 10:25 and comments below.

changePrevalence <- function(baselineDiameters, threshold, prevalence) {
	# Check baselineDiameters. 
	if (!identical(names(baselineDiameters), c("size", "weight")))
		stop("baselineDiameters must have names \"size\" and \"weight\"")
	if (length(unique(round(diff(baselineDiameters$size), 6))) != 1)
		stop("baselineDiameters$size must be evenly spaced")
	
	# Check prevalence. 
	if (missing(prevalence) || is.na(prevalence) || !is.numeric(prevalence) || 
			length(prevalence) != 1 || prevalence < 0 || prevalence > 1)
		stop("prevalence must be a single numeric between 0 and 1")
	
	# Check threshold.
	if (missing(threshold) || is.na(threshold) || !is.numeric(threshold) || 
				length(threshold) != 1 || threshold < 0)
		stop("threshold must be a single positive numeric")
	if (!(threshold %in% baselineDiameters$size))
		stop("for now, threshold must be in baselineDiameters$size;\n", 
				"  if you want to change this, edit changePrevalence")
	
	# Calculate the new weights.
	
	sizeIsAboveThreshold <- baselineDiameters$size >= threshold
	originalPrevalence <-  sum(baselineDiameters$weight[sizeIsAboveThreshold])
	
	sizeTimesWeight <- baselineDiameters$size * baselineDiameters$weight
	b <- (originalPrevalence - prevalence) / 
			(originalPrevalence * sum(sizeTimesWeight) - 
				sum(sizeTimesWeight[sizeIsAboveThreshold]))
	a <- 1 - b * sum(sizeTimesWeight)
	
	# new weight = (a + b * size) * old weight, or zero if that is negative
	baselineDiameters$weight <- pmax(0, 
			(a + b * baselineDiameters$size) * baselineDiameters$weight)
	
	# Now fix the prevalence by multiplying weights for diameters >= threshold 
	# by a factor. This is to deal with the problem that the prevalence might 
	# not yet be right, because negative weights have been set to zero. 
	#@ See e-mail from EJ 2016-11-22 10:25, including e-mails that are quoted 
	#@ at the bottom of that. But this code is slightly more correct than what 
	#@ we talked about in those e-mails. In the e-mails we said multiply the 
	#@ weights above 3.0cm by the desired prevalence divided by the current 
	#@ prevalence. But when you do that the weights will no longer add up to 
	#@ one, so you won't achieve the desired prevalence. The more correct way 
	#@ is in the "factorToUse <- ..." line below.
	#@ This does not manually set negative weights to 1e-7 either. (See e-mail.)
	sumOfWeightsBelow <- 
	        sum(baselineDiameters$weight[baselineDiameters$size < threshold])
	sumOfWeightsAbove <- 
	        sum(baselineDiameters$weight[baselineDiameters$size >= threshold])
	factorToUse <- prevalence / (1 - prevalence) * 
	        sumOfWeightsBelow / sumOfWeightsAbove
	baselineDiameters$weight[baselineDiameters$size >= threshold] <- 
	        baselineDiameters$weight[baselineDiameters$size >= threshold] * 
	        factorToUse
	
	# Make them add up to 1, for safety.
	baselineDiameters$weight <- 
			baselineDiameters$weight / sum(baselineDiameters$weight)
	
	return(baselineDiameters)
}

# Test checkPrevalence - see tests/testAortaModel.R.

################################################################################
# Functions to process Stata output to get parameters for the aorta growth
# model, including PSA. 
# 
# For the main analysis (processPersons, not psa),
# readTransformedAortaGrowthPars is intended to be used once, on the file of 
# transformed betas, and the output then passed to untransformAortaGrowthPars, 
# and the output of that should then be pasted into a file like 
# aortaModelParametersForPhaseX.R. 
# 
# For psa, readTransformedAortaGrowthPars is intended to be used twice, once 
# for the betas and once for the covariance matrix, and those outputs (the 
# mean and covariance) should then be pasted into a file like 
# aortaModelParametersForPhaseX.R; then psa will get the hard-coded mean and 
# covariance from the file, generate the transformed parameters, and finally  
# use untransformAortaGrowthPars on those. 
# 
# (I am using "transformed" to refer to things like atanRho. Previously, 
# in psa, I was referring to them as "untransformed".) 

# Read one of the two CSV files that are produced by the Stata script. 
# This works on the CSV file of betas or the CSV file of the covariance.
readTransformedAortaGrowthPars <- function(fileName) {
	columnsToKeep <- c("logsizecm.time", "logsizecm._cons", "lns1_1_1._cons", 
			"lns1_1_2._cons", "atr1_1_1_2._cons", "lnsig_e._cons")
	newColumnNames <- c("beta1", "beta0", "logSigma1", "logSigma0", "atanhRho", 
			"logSigmaW") 
	dataFrame <- read.csv(fileName)[, columnsToKeep]
	if (ncol(dataFrame) != length(columnsToKeep))
		stop("betas has the wrong dimensions")
	names(dataFrame) <- newColumnNames
	if (nrow(dataFrame) == 1) {
		return(unlist(dataFrame))  # return a named vector
	} else {
		return(as.matrix(dataFrame))  # return a matrix
	}
}

# Untransform the vector of betas,  i.e. convert from atanhRho to rho etc.
untransformAortaGrowthPars <- function(transformedPars) {
	# Define transformedNames and make sure transformerPars is a numeric 
	# vector with the right names. (When a numeric vector has an attribute 
	# such as "type", it fails is.vector. So create
	# transformedParsWithAttrsRemoved and use is.vector on that instead.)
	transformedNames <- c("beta1", "beta0", "logSigma1", "logSigma0", 
			"atanhRho", "logSigmaW")
	transformedParsWithAttrsRemoved <- transformedPars
	attributes(transformedParsWithAttrsRemoved) <- NULL
	if (!is.vector(transformedParsWithAttrsRemoved) || 
			!is.numeric(transformedPars) || 
			!identical(names(transformedPars), transformedNames))
		stop("transformedPars is illegal")
	result <- c(
		beta1 = transformedPars["beta1"], 
		beta0 = transformedPars["beta0"],
		sigma1 = exp(transformedPars["logSigma1"]),
		sigma0 = exp(transformedPars["logSigma0"]),
		rho = tanh(transformedPars["atanhRho"]),
		sigmaW = exp(transformedPars["logSigmaW"])
	)
	# result now has names like "sigma1.logSigma1", so cut ".logSigma1" etc.:
	names(result) <- sub("(\\w+)\\.\\w+", "\\1", names(result))
	return(result)
}

# The following lines show how to get and untransform the aorta model 
# parameters that are produced by 
# "SWAN_mixed_model_parameter_generation, slightly edited.do", and 
# maybe newer Stata files too. 
if (FALSE) {
	setwd("C:/MyDocs/AAA/")
	for (parameterSet in c("phaseIold", "phaseInew", "phaseIb2ii", 
			"dec2016men", "dec2016women")) {
		if (parameterSet == "phaseIold") {
			# Old aorta parameters, as of 2016-06-10 (e.g. sigma0=0.21325151):
			betaFileName <- "new baseline distribution etc, 2016-02-04/model_parameters_growth.csv"
			covFileName <- "new baseline distribution etc, 2016-02-04/model_parameters_growth_vcov.csv"
			
		} else if (parameterSet =="phaseInew") {
			# New aorta parameters for Phase I (e.g. sigma0=0.21656815):
			betaFileName <- "model_parameters_growth_PhaseIa.txt"
			covFileName <- "model_parameters_growth_vcov_PhaseIa.txt"
			
		} else if (parameterSet =="phaseIb2ii") {
			# New aorta parameters for Phase IB 2ii (e.g. sigma0=0.14881773):
			betaFileName <- "model_parameters_growth_PhaseIbii.txt"
			covFileName <- "model_parameters_growth_vcov_PhaseIbii.txt"
			
		} else if (parameterSet =="dec2016men") {
			#@ See e-mail from MS 2016-12-07 17:43.
			setwd("W:/Studies/RESCAN/model estimates/growth/men")
			betaFileName <- "model_parameters_growth_unwgt.csv"
			covFileName <- "model_parameters_growth_unwgt_vcov.csv"
		
		} else if (parameterSet =="dec2016women") {
			#@ See e-mail from MS 2016-12-07 17:43.
			setwd("W:/Studies/RESCAN/model estimates/growth/women")
			betaFileName <- "model_parameters_growth_unwgt.csv"
			covFileName <- "model_parameters_growth_unwgt_vcov.csv"
			
		} else {
			stop("unknown parameterSet")
		}
		# NB: the files for for phaseIold (see above) are created by 
		# "mixed_analyses___270515, edited for CMPC203 etc.txt"
		# and the files for phaseI and phaseIb2ii are created by 
		# "SWAN_mixed_model_parameter_generation, slightly edited.do".
		
		# Get the transformed betas and from those get the untransformed betas:
		transformedPars <- readTransformedAortaGrowthPars(betaFileName)
		untransformedPars <- untransformAortaGrowthPars(transformedPars)
		
		# Get the covariance matrix (which refers to the transformed parameters):
		transformedCov <- readTransformedAortaGrowthPars(covFileName)
		
		# View things. 
		cat("\n\n", parameterSet, ":\n\n", sep="")
		print(transformedPars)    
		print(untransformedPars) 
		print(transformedCov)  # (I think this shows all the decimal places)
	}
	
	# How to hard-code one of these sets of numbers in 
	# aortaModelParametersForPhaseX.R:
	# - Write out untransformedPars as v1distributions$rho etc. These will be 
	#   used by processPersons. 
	# - Write out transformedPars as v1distributions$meanForGrowthParameters 
	#   (a vector that contains atanhRho etc.). This will be used by psa.
	# - Write out transformedCov as v1distributions$covarianceForGrowthParame
	#   ters, a matrix. This will be used by psa. 
	
	# TODO: (maybe) don't hard-code numbers, but instead make the main R 
	# programs, i.e. processPersons and psa, get the numbers directly from the 
	# CSV files produced by Stata and manipulate them as necessary.
	# This might not be worth it as we are not going to use a lot of
	# different sets of parameters or baseline size distributions. 
}

################################################################################


