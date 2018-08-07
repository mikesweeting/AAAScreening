################################################################################
# processControlOnly
# used in Shiny app to get events for control group only
# events for intervention group will be obtained from the differences

processControlOnly <- function(personNumber, v0, v1other, v2, updateProgress) {
	# personNumber is not used (except if showEventHistories). It is just 
	# needed because lapply and parLapply force you to pass the number into 
	# the function that they call. 
	
  # If we were passed a progress update function, call it
  if (is.function(updateProgress)) {
    text <- "Calculating absolute event numbers in not invited group"
    num <-personNumber/(2*v0$numberOfPersons)
    updateProgress(value=num,detail = text)
  }
  
  
	# Check things. 
	if (!("namesOfQuantities" %in% names(v0)))
		stop("processOnePair needs v0$namesOfQuantities to exist;",
				"\nthis is normally done by processPersons or psa")
	if (v1other$nonAaaDeathMethod %in% c("mass", "onsIntegerStart") && 
			!("nonAaaSurvProbs" %in% names(v1other)))
		stop("when v1other$nonAaaDeathMethod is \"", v1other$nonAaaDeathMethod, "\", ",
				"processOnePair needs \nv1other$nonAaaSurvProbs ",
				"to exist; this is normally done by processPersons")
	if (v1other$nonAaaDeathMethod == "onsNonintegerStart" &&
			!("nonAaaMortalityRates" %in% names(v1other)))
		stop("when v1other$nonAaaDeathMethod is , \"onsNonintegerStart\", ",
				"processOnePair needs \nv1other$nonAaaMortalityRates ",
				"to exist; this is normally done by processPersons")
	
	# Make result, which will be returned from this function. 
	result <- list(personQuantities=
			sapply(v0$treatmentGroups, function(x) NULL)) 
	if (v0$returnEventHistories) result$eventHistories <- 
			sapply(v0$treatmentGroups, function(x) NULL)
	
	# Generate this person's characteristics and natural events. 
	aortaGrowthParameters <- generatePersonAortaParameters(v1other, v2)
	v3 <- compactList(
		# Characteristics:
		b0=aortaGrowthParameters$b0, 
		b1=aortaGrowthParameters$b1,
		# Natural events:
		ruptureTime=aortaGrowthParameters$ruptureTime,
		nonAaaDeathTime=generateTimeTillNonAaaDeath(v0, v1other),

		# Initial diameter as measured (this is "y0" in 
		# generatePersonAortaParameters and is only used if they attend 
		# screening and do not have non-visualization):
		initialAortaSizeAsMeasured=
		aortaGrowthParameters$initialAortaSizeAsMeasured
	)
	
	
	
	# Generate this person's boolean variables such as requireReinvitation and 
	# decideOnElectiveSurgery. Go through the elements of v2 and for those that 
	# have type "probability", generate a boolean element of v3. (If it has 
	# type "logistic model for probability" then deal with it elsewhere. 
	# Also exclude ones that are in namesOfProbVarsForSurvivalModel.)
	for (elementName in names(v2)) {
		if (elementName %in% namesOfProbVarsForSurvivalModel) next
		v2element <- v2[[elementName]]
		if (getType(v2element) == "probability")
			v3[[elementName]] <- setType(rbernoulli(v2element), "boolean")
	}

	# Generate this person's censoring-time, if v0$generateCensoringTime exists.
	if ("generateCensoringTime" %in% names(v0)) {
		v3$censoringTime <- v0$generateCensoringTime()
	}
	
	# Put the person through the different treatments. 
	for (treatmentGroup in "noScreening") {
		
		# Create this person's full list of events. Record everything that will
		# be necessary for calculating this person's costs, QALYs, etc. The 
		# possible event-types can be got from the big flowchart for the 
		# previous model. (Previously, generateEventHistory's last argument was 
		# v4 = list(treatmentGroup=treatmentGroup), but that was pointless.) 
		eventHistory <- 
				generateEventHistory(v0, v1other, v2, v3, treatmentGroup)
		
		# If showEventHistories, then display the person's event-history.
		# This was mostly for the early stages of development or debugging.
		# If you want to show their age at baseline as well, change the line 
		# "nonAaaDeathTime=...". At present, initial age is not stored. 
		if (v0$showEventHistories) {
			varNames <- c("personNumber", "treatmentGroup", "v3$b0", 
					"v3$b1", "v3$ruptureTime", "v3$nonAaaDeathTime")
			for (varName in varNames) {
				var <- getAnything(varName)
				if (varName != "personNumber" && is.numeric(var)) 
					var <- sprintf("%.2f", var)
				cat(varName, "=", var, "  ", sep="")
				#if (k %% 4 == 0 && k < length(varNames)) cat("\n")	
			}
			cat("\n")
			print(eventHistory)
			cat("\n")
		}
		
		# If returnEventHistories, then store the event-history. 
		if (v0$returnEventHistories) 
			result$eventHistories[[treatmentGroup]] <- eventHistory	
		# MS added. Store individual's b0 and b1 so that aorta size of the population can be assessed at later ages
      result$eventHistories[[treatmentGroup]]$b0 <- v3$b0
      result$eventHistories[[treatmentGroup]]$b1 <- v3$b1
      result$eventHistories[[treatmentGroup]]$initialAortaSizeAsMeasured <- v3$initialAortaSizeAsMeasured
		# From eventHistory (and nothing else that is specific to the person),
		# calculate their life-years, QALYs, 
		# total cost, etc., and store these in outputs. The health-economic
		# quantities have to be calculated here, when you definitely have all 
    # the event-times to hand (in eventHistory), because the discounting 
    # calculations require all the specific event-times. 
    result$personQuantities[[treatmentGroup]] <- 
				calculateHealthEconomicQuantities(
				eventHistory, v0$namesOfQuantities, v2$costs, v1other)
	}
	
	return(result)
}

################################################################################

# TODO: maybe get rid of v0$showEventHistories, because if the user wants to 
# see all the event-histories then they can just set v0$returnEventHistories
# to be true and then look at result$eventHistories. 
# But I will leave it for now, because showEventHistories makes it show some 
# extra information as well as the event-histories themselves. 





