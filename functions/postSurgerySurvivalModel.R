################################################################################
# Post-surgery survival model, based on data from the EVAR-1 clinical trial.
# This is about AAA death following evar and open surgery, for Phase IB 2(ii). 
# For now this model is just for elective surgery. 
# 
# See also main/postSurgerySurvival_exploratory.R 

# This is an alternative to the model (or sub-model) used in the validation, 
# which was just instant death with a certain probability. 

###############################################################################
# Generate from the "survival model" for peri/post-operative AAA death. 
# 
# The model is: 
# 
# - instantaneous death, with probability 
#   probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery (or same with 
#   Emergency and/or Evar);
#
# - zero probability of death between zero and postSurgeryInitialPeriod;
#
# - after that, an exponential distribution with parameter 
#   rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod (or same with 
#   Emergency and/or Evar).
# 
# This and other code assumes that v1other$postSurgeryInitialPeriod is the 
# same for all the four different kinds of surgery.

generatePostSurgeryAaaDeathTime <- function(v1other, v2, v3, eventTime, 
		surgeryType, surgeryAdmissionMode) {
	# v1other is needed because postSurgeryInitialPeriod is in that.
	# eventTime is needed because getBinaryVariable will use that if 
	# it needs to use a logistic model in which age is one of the covariates.
	# Similarly v3 is needed if aortaSize is a covariate. 
	
	if (v1other$electiveSurgeryAaaDeathMethod != "survivalModel")
		stop("generatePostSurgeryAaaDeathTime must only be used if ",
				"v1other$electiveSurgeryAaaDeathMethod is \"survivalModel\"")
	
	# Check surgeryType and surgeryAdmissionMode.
	if (!(surgeryType %in% c("open", "evar")))
		stop("surgeryType must be open or evar")
	if (!(surgeryAdmissionMode %in% c("elective", "emergency")))
		stop("surgeryAdmissionMode must be elective or emergency")
	
	# Make the two variable names that are needed. 
	surgeryType <- changeFirstLetterToUpperCase(surgeryType)
	surgeryAdmissionMode <- changeFirstLetterToUpperCase(surgeryAdmissionMode) 
	probVarName <- paste0("probOfAaaDeathInInitialPeriodAfter",
			surgeryAdmissionMode, surgeryType, "Surgery")
	rateVarName <- paste0("rateOfAaaDeathAfter", 
			surgeryAdmissionMode, surgeryType, "SurgeryAndInitialPeriod")
	
	# Generate the AAA death time. 
	if (getBinaryVariable(probVarName, v1other, v2, v3, eventTime)) {
	#if (rbernoulli(v2[[probVarName]])) {
		return(0)
	} else {
		return(v1other$postSurgeryInitialPeriod + 
				rexp(n=1, rate=v2[[rateVarName]]))
	}
	
	# Previously (at two slightly different stages) this used 
	# v2$probOfAaaDeathInInitialPeriodAfterElectiveSurgery[surgeryType] and
	# v2$postElectiveSurgeryExpDistPar[surgeryType].
}

# In the future we might use similar models for other time-to-events. In that 
# case, it might be best to rewrite this function and rename it as 
# generateTimeFromTwoPartModel (or some better name), with arguments 
# initialSurvProb, initialPeriod, and expDistPar.

################################################################################
# Define and calculate the parameters for the model.
#@ See Word doc attached to e-mail 2016-07-05 15:58.

# The following code produces the parameters for Glover 2014 / Phase IB 2(ii),  
# using EVAR-1 data. If parameters need to be produced from another source, 
# then it will need to be adapted in a suitable way. 

if (FALSE) {  # comment this out so it doesn't run every time

drawGraphics <- FALSE
	
# Get the data. Use time since operation and ITT. 
library(foreign, quietly=TRUE)
library(survival, quietly=TRUE)
evar1 <- read.dta("C:/MyDocs/AAA/evar1_swan__2016-07-04_Stata12.dta")
#levels(evar1$treat) <- c("open", "evar", "noIntervention") # treatment group
levels(evar1$optype) <- c("evar", "open")   # actual type of operation received
evar1$event <- evar1$eventoptoaaadeath == "Died from AAA causes"
evar1$time <- evar1$timeoptoaaadeath

# Exclude certain people, and set optype to "open" if the operation was 
# "converted"/changed to open surgery part-way through.   
noOp <- is.na(evar1$event) 
emergencyOp <- evar1$opstatus == "emergency AAA repair"  
abandonedOp <- evar1$opevent == "Procedure abandoned"
convertedOp <- evar1$opevent == "Conversion to open repair"
sum(noOp); sum(emergencyOp); sum(abandonedOp); sum(convertedOp)
evar1$optype[convertedOp] <- "open"
evar1 <- evar1[!noOp & !emergencyOp & !abandonedOp,]
table(evar1$optype, useNA="always")  
#@ should be 622, 584 according to Word doc attached to e-mail 2016-07-05 15:58

# Define endOfInitialPeriod. E.g. 30 / 365.25 or 0.25.
postSurgeryInitialPeriod <- 30 / 365.25 # a.k.a. endOfInitialPeriod

# Create empty vectors to store the parameters in. 
probOfAaaDeathInInitialPeriodAfterElectiveSurgery <- numeric()  
rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriod <- numeric() 

# Parameters for PSA. 
probOfAaaDeathInInitialPeriodAfterElectiveSurgeryParameters <- list()
rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriodParameters <- list()

if (drawGraphics) {
	par(mfcol=c(2,1), oma=c(0,0,2,0))
	title("KM curve and survival curve of the model", outer=TRUE)
}

# Fit models etc. to get the necessary parameters for our model. 
for (opType in unique(evar1$optype)) {
	# Get the data for this operation type. 
	evar1thisOpType <- evar1[evar1$optype==opType,]
	
	##### INITIAL PERIOD ####
	
	# Find the estimated survival probability at endOfInstantDeathPeriod, using 
	# both men and women, and using logistic regression.
	evar1thisOpType$eventDuringInitialPeriod <- 
			evar1thisOpType$time < postSurgeryInitialPeriod 
	if (any(evar1thisOpType$eventDuringInitialPeriod & !evar1thisOpType$event))
		stop("there are events before postSurgeryInitialPeriod, ",
				"so logistic regression is not correct")
	glmObject <- glm(eventDuringInitialPeriod ~ 1, 
			family=binomial(link="logit"), data=evar1thisOpType)
	odds <- exp(glmObject$coefficients)
	probOfAaaDeathInInitialPeriodAfterElectiveSurgery[opType] <- 
			odds / (1 + odds)
	
	# For now, make a beta distribution on this, for PSA. 
	probOfAaaDeathInInitialPeriodAfterElectiveSurgeryParameters[[opType]] <- 
			c(alpha=sum(evar1thisOpType$eventDuringInitialPeriod), 
			beta=sum(!evar1thisOpType$eventDuringInitialPeriod))
	
	# OLD: Find probOfAaaDeathInInitialPeriodAfterElectiveSurgery using KM curve 
	#survfitObj <- survfit(Surv(time, event) ~ 1, data=evar1subset)
	#survProb <- summary(survfitObj, 
	#		times=postSurgeryInitialPeriod)$surv
	
	##### LATTER PERIOD ####
	
	# Make a new time-variable and a new data-frame. 
	newTime <- evar1thisOpType$time - postSurgeryInitialPeriod
	evar1thisOpType$timeAfterInitialPeriod <- 
			{ if(newTime > 0) newTime else NA }
	evar1thisOpTypeMenOnly <- evar1thisOpType[evar1thisOpType$sex=="Male",]
	
	# Fit a parametric model from postSurgeryInitialPeriod on, 
	# using men only. (Time zero in survregObj corresponds to time 
	# endOfInitialPeriod in the overall model.) 
	survregObj <- survreg(Surv(timeAfterInitialPeriod, event) ~ 1, 
			dist="exponential", data=evar1thisOpTypeMenOnly)
	# (rows with timeAfterInitialPeriod=NA will be ignored by survreg)

	# Store the parameter. (See http://stats.stackexchange.com/a/215563/117456)
	rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriod[opType] <- 
			exp(-survregObj$coefficients["(Intercept)"])
	# Now you can do rexp(n=1, rate=postSurgeryExpDistPar["evar"]) etc.
	# Alternatively, keep survregObj and do this instead of rexp:
	# rsurvreg(n=1, mean=survregObj$linear.predictors[1], 
	#	scale=survregObj$scale, distribution=distribution)

	# For now, make a gamma distribution on this, for PSA. 
	rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriodParameters[[opType]] <- 
			c(shape=sum(evar1thisOpTypeMenOnly$event[
			evar1thisOpTypeMenOnly$timeAfterInitialPeriod > 0], na.rm=TRUE), 
			scale=1 / sum(evar1thisOpTypeMenOnly$timeAfterInitialPeriod, 
			na.rm=TRUE))

	# Plot the KM curve and the fitted model. 
	# [This probably won't work, as dataset object names etc. have changed.] 
	if (drawGraphics) {
		maxTime <- 15
		minSurvProb <- 0.8
		plot(survfitObj, mark.time=FALSE, conf.int=TRUE, 
				lty=c("solid", "dotted", "dotted"), main=opType,
				xlab="time (years)", ylab="survival probability", 
				xlim=c(0, maxTime), ylim=c(minSurvProb, 1))
		abline(h=seq(0, 1, by=0.02), v=0:maxTime, col="grey90", lty="dotted")
		legend(x="topright", bg="white", legend=c("non-parametric", 
				"fitted parametric model"), lty=c("solid", "dashed"))
		lines(x=c(0, 0, postSurgeryInitialPeriod), y=c(1, 
				rep(probOfAaaDeathInInitialPeriodAfterElectiveSurgery[opType], 
				2)), lty="dashed")
		timeValues <- 
				seq(postSurgeryInitialPeriod, maxTime, by=0.01)
		survivalValues <- 
				probOfAaaDeathInInitialPeriodAfterElectiveSurgery[opType] * 
				pexp(q=timeValues - postSurgeryInitialPeriod, 
				rate=rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriod[opType],
				lower.tail=FALSE)
		lines(timeValues, survivalValues, lty="dashed")
	}
}

# Rename things so that PSA can generate parameters using existing code with 
# grep etc. (The main disadvantage of this is that 
# generatePostSurgeryAaaDeathTime has to manipulate text to recreate 
# these variable-names.)
# [The "Parameters" at the end of some of these variable names is 
# out of date.] 
probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery <- 
		probOfAaaDeathInInitialPeriodAfterElectiveSurgery["open"]
probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery <- 
		probOfAaaDeathInInitialPeriodAfterElectiveSurgery["evar"]
probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryParameters <- 
		probOfAaaDeathInInitialPeriodAfterElectiveSurgeryParameters$open
probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryParameters <- 
		probOfAaaDeathInInitialPeriodAfterElectiveSurgeryParameters$evar
rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod <- 
		rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriod["open"]
rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod <- 
		rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriod["evar"]
rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriodParameters <- 
		rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriodParameters$open
rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriodParameters <- 
		rateOfAaaDeathAfterElectiveSurgeryAndInitialPeriodParameters$evar


# Display the numbers, for checking  
#@ These numbers should match the Word doc attached to e-mail 2016-07-05 15:58
#@ (numbers from the Word doc are in comments):
postSurgeryInitialPeriod
probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgery  # 0.0411
probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgery  # 0.0161
probOfAaaDeathInInitialPeriodAfterElectiveOpenSurgeryParameters  # 24, 560
probOfAaaDeathInInitialPeriodAfterElectiveEvarSurgeryParameters  # 10, 612
rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriod  # 0.00070
rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriod  # 0.00766
rateOfAaaDeathAfterElectiveOpenSurgeryAndInitialPeriodParameters  # 3, 1/4291.1
rateOfAaaDeathAfterElectiveEvarSurgeryAndInitialPeriodParameters  # 34, 1/4436.3

# Now store the above five variables in parsForMen30yearsUpdated.R or the 
# appropriate place (done).

}

# Notes on PSA distributions for the parameters of the peri/post-operative 
# AAA death model:
# - If we later use logistic regression for the PSA distribution of 
#   probOfAaaDeathInInitialPeriodAfterElectiveSurgery, then also create 
#   seOfprobOfAaaDeathInInitialPeriodAfterElectiveSurgeryIntercept, or whatever 
#   other  parameters are necessary. [Maybe useful code: seOfPostElective
#   SurgeryInitialSurvProbIntercept[opType] <- sqrt(vcov(glmObject)[1,1]).] 
# - If we later use a normal distribution for what survreg calls "(Intercept)" 
#   for the post-30-day exponential model, then also create 
#   seOfPostElectiveSurgeryExpDistIntercept or whatever is needed. 
# - Actually I think all these should be included in 
#   probOfAaaDeathInInitialPeriodAfterElectiveSurgery(Parameters) etc. 

###############################################################################



