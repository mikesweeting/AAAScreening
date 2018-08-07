################################################################################
# For a single person, given their treatmentGroup, ruptureTimes, 
# nonAaaDeathTime, and so on, generate their full event-history. 

generateEventHistory <- function(v0, v1other, v2, v3, treatmentGroup) {
	
	# Create an empty event-history. 
	eventHistory <- makeEmptyEventHistory(recordSizes=v0$recordSizes)
	
	# Make a vector for storing the times of the next "scheduled" events. 
	# Usually, many of these will be NA, which means they are not scheduled. 
	# Whichever of these times is lowest will be the one that actually happens. 
	scheduledEvents <- numeric()   
	
	# Store some times in scheduledEvents. 
	scheduledEvents["rupture"] <- v3$ruptureTime
	scheduledEvents["nonAaaDeath"] <- v3$nonAaaDeathTime
	if (treatmentGroup=="screening") 
		scheduledEvents["inviteToScreen"] <- 0
	if (treatmentGroup=="noScreening")
		scheduledEvents["incidentalDetection"] <- 
				generateIncidentalDetectionTime(0, 
				v1other$thresholdForIncidentalDetection, v3, 
				v2$rateOfIncidentalDetection)
	if ("censoringTime" %in% names(v3))
		scheduledEvents["censored"] <- v3$censoringTime
	
	repeat {
		# Make sure that all the scheduled times are different (exclude NAs).
		# I think this is the best policy. ("which.min" only finds the first  
		# one that has the minimum value, and it ignores NAs.)
		if (!allDifferent(scheduledEvents)) {
			print(scheduledEvents)
			stop("scheduledEvents times must all be different")
		}
		
		# Work out what happens next by looking at the "scheduled" events and 
		# seeing which has the lowest time, and store this event in eventHistory.  
		eventType <- names(scheduledEvents)[which.min(scheduledEvents)] 
		# (which.min never returns an NA)
		eventTime <- scheduledEvents[eventType] 
		if (is.na(eventTime)) {  # DEBUGGING
			print(scheduledEvents)
			stop("eventTime is NA") 
		}
		eventHistory <- addEvent(eventHistory, eventType, eventTime)
		scheduledEvents[eventType] <- NA 
	
		# Update scheduledEvents as appropriate based on what eventType is.
		
		# Policy on where to record and schedule events. 
		# If in the repeat loop it is decided that someone dies, then the death 
		# should just be recorded in eventHistory and the loop should be exited. 
		# If it is decided that there is an "immediate" event (an event that
		# happens straight away, e.g. non-visualization or contraindication), 
		# and that event does not require any non-trivial code, then that  
		# event should similarly just be recorded in eventHistory. 
		# All other events have to be "scheduled" i.e. recorded in 
		# scheduledEvents. This includes immediate non-trivial events such as 
		# screen and emergencySurgeryOpen. 
		# The goal of this policy is just to make the code readable and 
		# manageable. (The previous policy was that scheduledEvents should 
		# only contain events that are "scheduled" for the future but might not 
		# happen. But I think the newer policy is better.)  
		
		if (eventType=="inviteToScreen") {
			if (v3$probOfRequireReinvitation) {
				eventHistory <- addEvent(eventHistory, "requireReinvitation", 0)
			} 
			if (v3$probOfAttendScreen) {
				scheduledEvents["screen"] <- 0   # ("screen" means they attend)
			} else {
				eventHistory <- addEvent(eventHistory, "failToAttendScreen", 0)
				scheduledEvents["incidentalDetection"] <- 
						generateIncidentalDetectionTime(
						eventTime, v1other$thresholdForIncidentalDetection, v3, 
						v2$rateOfIncidentalDetection)
			}
		# NB: the code with invite, reinvite, attend, etc. does not correspond
		# to reality and will need to be rewritten if the model is changed so 
		# that they don't all happen at time zero. See memos elsewhere.
		
		# It is necessary to to separate out the two reasons for failing to 
		# attend screening (i.e. failing to respond and failing to attend), 
		# because reinvitation incurs a cost. 
	
		} else if (eventType=="screen") {
			# Enough code is different between screen and monitor that it is 
			# definitely better to have them separate. 
	
			aortaSize <- v3$initialAortaSizeAsMeasured
			if (is.null(aortaSize) || is.na(aortaSize))
				stop("aortaSize is NA or NULL")

			# If you are not using v3$initialAortaSizeAsMeasured, then the 
			# following line might be appropriate instead.
			#aortaSize <- getExactInitialAortaSize(v3$b0, v3$b1)  # = exp(v3$b0)
			
			# If you are using a quantity directly sampled from 
			# v2$baselineDiametersWithDesiredPrevalence as the initial aorta 
			# size as measured (for example, if you you are returning 
			# initialAortaSizeAsMeasured from generatePersonAortaParameters 
			# and using "aortaSize <- v3$initialAortaSizeAsMeasured"), then 
			# it is highly possible that aortaSize exactly equals one of 
			# v1other$aortaDiameterThresholds. If this happens then it will be 
			# regarded as being in the higher of the two size groups (see 
			# aortaSizeGroup below).
	
			# Previously: 
			#aortaSize <- getAortaMeasurement(v3, eventTime, 
			#		v2$ultrasoundMeasurementErrorSD, method="ultrasound")
	
	
			# The following line will need to change depending on how 
			# generatePersonAortaParameters works.
			# It should record true size iff that is known.
			# (You could record the size even if v3$probOfNonvisualization, but that 
			# would be strange.)
			if ("trueSizes" %in% names(eventHistory) && !v3$probOfNonvisualization)  
				# The true size is unknown, so set that to NA. The measured 
				# size is aortaSize, so record that.
				eventHistory <- recordSize(eventHistory, NA, aortaSize)
			
			# If generatePersonAortaParameters returns b0 and b1 and the 
			# initial measurement is generated anew from those (like in August 
			# 2016 or so), then the following code will probably be 
			# appropriate:
			#if ("trueSizes" %in% names(eventHistory)) {
			#	trueSize <- getAortaMeasurement(v3, eventTime, method="exact")
			#	eventHistory <- recordSize(eventHistory, trueSize, aortaSize)
			#}
			
			# If they have nonvisualization, then record that event. Code just 
			# below will schedule incidental detection.
			if (v3$probOfNonvisualization) 
				eventHistory <- addEvent(eventHistory, "nonvisualization", 
						eventTime)
			
			# Find what interval aortaSize is in & schedule events accordingly.
			aortaSizeGroup <- findInterval(aortaSize, 
					v1other$aortaDiameterThresholds)
			if (aortaSizeGroup == 0 || v3$probOfNonvisualization) { 
				# aortaSizeGroup==0 means non-aneurysmal i.e. "normal"
				scheduledEvents["incidentalDetection"] <- 
						generateIncidentalDetectionTime(
						eventTime, v1other$thresholdForIncidentalDetection, v3, 
						v2$rateOfIncidentalDetection)
			} else if (aortaSizeGroup < length(v1other$aortaDiameterThresholds)) {
				scheduledEvents["monitor"] <- 
						eventTime + v1other$monitoringIntervals[aortaSizeGroup]
				scheduledEvents["dropout"] <- generateDropoutTime( 
						v2$rateOfDropoutFromMonitoring, eventTime)
			} else {
				scheduledEvents["consultation"] <- 
						eventTime + v1other$waitingTimeToConsultation
			}
			
		} else if (eventType=="monitor") {
			# For now, assume non-visualization never happens in monitoring. 
			aortaSize <- getAortaMeasurement(v3, eventTime, 
					v2$ultrasoundMeasurementErrorSD, method="ultrasound")
			if ("trueSizes" %in% names(eventHistory)) {
				trueSize <- getAortaMeasurement(v3, eventTime, method="exact")
				eventHistory <- recordSize(eventHistory, trueSize, aortaSize)
			}
			aortaSizeGroup <- findInterval(aortaSize, 
					v1other$aortaDiameterThresholds)
			if (aortaSizeGroup == 0) {  # they are sub-aneurysmal
				eventHistory <- addEvent(eventHistory, 
						"aortaDiameterBelowThreshold", eventTime)
				scheduledEvents["monitor"] <- 
						eventTime + v1other$monitoringIntervals[1]
			} else if (aortaSizeGroup < length(v1other$aortaDiameterThresholds)) {
				scheduledEvents["monitor"] <- 
						eventTime + v1other$monitoringIntervals[aortaSizeGroup]
			} else {
				scheduledEvents["consultation"] <- 
						eventTime + v1other$waitingTimeToConsultation
				scheduledEvents["dropout"] <- NA
			}
			
		} else if (eventType=="consultation") {
			# Measure the aorta diameter using CT, and if the result is less
			# than thresholdForSurgery then return to monitoring.  
			aortaSize <- getAortaMeasurement(v3, eventTime, 
					v2$ctMeasurementErrorSD, method="ct", 
					extraDiameterForCtScan=
					v2$extraDiameterForCtScan)
			# Record the size: (previously this was just after 
			# addEvent(..., "decideOnReturnToMonitoring",...) a few lines below)
			if ("trueSizes" %in% names(eventHistory)) {
				trueSize <- getAortaMeasurement(v3,eventTime,method="exact")
				eventHistory <- recordSize(eventHistory, trueSize,aortaSize)
			}
			aortaSizeGroup <- findInterval(aortaSize, 
					v1other$aortaDiameterThresholds)
			## Added by MJS 09/03/17 to allow probOfContraindication be defined by a logistic model
			contraindication <- getBinaryVariable("probOfContraindication", v1other, v2, v3, eventTime)
			
			if (aortaSizeGroup < length(v1other$aortaDiameterThresholds)) {
				# they are below the threshold for surgery
				eventHistory <- addEvent(eventHistory, 
						"decideOnReturnToMonitoring", eventTime)
				scheduledEvents["monitor"] <- 
						eventTime + v1other$monitoringIntervals[aortaSizeGroup]
				scheduledEvents["dropout"] <- generateDropoutTime( 
						v2$rateOfDropoutFromMonitoring, eventTime) 

			} else if (contraindication) {
				# Contraindication needs to be stored as an event, as it is one 
				# of the "validation variables" in Table 1 of Kim 2010. 
				eventHistory <- addEvent(eventHistory, 
						"contraindicated", eventTime)
				if(!is.null(v2$rateOfNonAaaDeathAfterContraindication)){
				  scheduledEvents["nonAaaDeath"] <- eventTime + 
				    generateTimeToNonAaaDeathFromContraindication(
				      v2$rateOfNonAaaDeathAfterContraindication)  
				}
				
		
				# Post-contraindication monitoring. 
				if ("monitoringIntervalFollowingContraindication" %in% 
						names(v1other))
					scheduledEvents["monitorFollowingContraindication"] <- 
							eventTime + 
							v1other$monitoringIntervalFollowingContraindication
		
			} else {  # decide on elective surgery
				eventHistory <- addEvent(eventHistory, 
						"decideOnElectiveSurgery", eventTime)
				# electiveSurgeryIsOpen can be based on either beta or logistic 
				# model, so use getBinaryVariable, not just the simple booleans
				# v3$probOfElectiveSurgeryIsOpenVia...:
				electiveSurgeryIsOpen <- getBinaryVariable(
						"probOfElectiveSurgeryIsOpen", v1other, v2, v3, eventTime)
				surgeryEvent <- if (electiveSurgeryIsOpen) {
					"electiveSurgeryOpen" } else { "electiveSurgeryEvar" }
				scheduledEvents[surgeryEvent] <- 
						eventTime + v1other$waitingTimeToElectiveSurgery
			} 
			
		} else if (eventType=="monitorFollowingContraindication") {
			scheduledEvents["monitorFollowingContraindication"] <- 
					eventTime + v1other$monitoringIntervalFollowingContraindication
			
		} else if (eventType=="electiveSurgeryOpen" || 
				eventType=="electiveSurgeryEvar") {
			# The code is mostly the same for elective open and elective evar. 
			surgeryType <- 
					{ if(eventType=="electiveSurgeryOpen") "open" else "evar" }
			# But there are two different models for AAA death:
			if (v1other$electiveSurgeryAaaDeathMethod == "instantDeathOnly") {
				if (surgeryType=="evar")
					stop("surgeryType=\"evar\" should be impossible when ",
						"v1other$electiveSurgeryAaaDeathMethod=\"instantDeathOnly\"")
				dieFromElectiveSurgery <- { if(
						"incidentalDetection" %in% eventHistory$events)
						v3$probOfDieFromElectiveSurgeryViaIncidentalDetection
						else
						v3$probOfDieFromElectiveSurgeryViaScreeningDetection }
				if (dieFromElectiveSurgery) { 
					eventHistory <- addEvent(eventHistory, "aaaDeath",eventTime)
					break
				}
				# Survived surgery. All that can happen now is nonAaaDeath.
			} else if (v1other$electiveSurgeryAaaDeathMethod == "survivalModel") { 
				scheduledEvents["aaaDeath"] <- eventTime + 
						generatePostSurgeryAaaDeathTime(v1other, v2, v3, 
						eventTime, surgeryType, "elective")
				if (is.na(scheduledEvents["aaaDeath"]))
					stop("generatePostSurgeryAaaDeathTime returned NA")
			} else {
				stop("v1other$electiveSurgeryAaaDeathMethod=", 
						v1other$electiveSurgeryAaaDeathMethod, " is illegal")
			}
			scheduledEvents["rupture"] <- NA
			
			# Reinterventions. Define surgeryTime, get the appropriate rates 
			# and time-boundaries, and schedule the first reintervention. 
			# TODO: there is duplicated code for this stuff under other events.
			surgeryTime <- eventTime
			if (eventType == "electiveSurgeryOpen") {
				reinterventionRates <- v2$reinterventionRatesAfterElectiveOpen
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterElectiveOpen
				reinterventionEventType <- "reinterventionAfterElectiveOpen"
			} else if (eventType == "electiveSurgeryEvar") {
				reinterventionRates <- v2$reinterventionRatesAfterElectiveEvar
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterElectiveEvar
				reinterventionEventType <- "reinterventionAfterElectiveEvar"
			} else {
				stop("eventType is illegal")
			}
			scheduledEvents[reinterventionEventType] <- 
					generateReinterventionTime(
					rates=reinterventionRates, 
					timeBoundaries=reinterventionTimeBoundaries,
					surgeryTime=surgeryTime, currentTime=eventTime, 
					postSurgeryInitialPeriod=v1other$postSurgeryInitialPeriod)
	
			# Post-surgery monitoring. 
			if (eventType == "electiveSurgeryOpen") {
				scheduledEvents["monitorFollowingOpenSurgery"] <- eventTime + 
						v1other$timeToMonitoringFollowingOpenSurgery
			} else {
				scheduledEvents["monitorFollowingEvarSurgery"] <- eventTime + 
						v1other$timeBetweenMonitoringFollowingEvarSurgery
			}
				
		} else if (eventType=="rupture") {
			if ("trueSizes" %in% names(eventHistory)) {
				trueSize <- getAortaMeasurement(v3, eventTime, method="exact")
				eventHistory <- recordSize(eventHistory, trueSize, NA)
			}
			if (v3$probOfEmergencySurgeryIfRupture) {

			  emergencySurgeryIsOpen <- getBinaryVariable(
			    "probOfEmergencySurgeryIsOpen", v1other, v2, v3, eventTime)
				surgeryEvent <- { if(emergencySurgeryIsOpen)
						"emergencySurgeryOpen" else "emergencySurgeryEvar" }
				scheduledEvents[surgeryEvent] <- eventTime
			} else {
				eventHistory <- addEvent(eventHistory, "aaaDeath", eventTime)
				break
			}
			eventsToCancel <- c("monitor", "dropout", "incidentalDetection", 
				"electiveSurgeryOpen", "electiveSurgeryEvar", "consultation")
			# consultation needs to be cancelled now that it doesn't happen 
			# immediately when monitoring finds >= 5.5. (Does this make sense?
			# Anyway cancelling consultation certainly does no harm.)
			scheduledEvents[eventsToCancel] <- NA
			
		} else if (eventType=="emergencySurgeryOpen" || 
				eventType=="emergencySurgeryEvar") {
			surgeryType <- 
					{ if(eventType=="emergencySurgeryOpen") "open" else "evar" }
			
			if (v1other$emergencySurgeryAaaDeathMethod == "instantDeathOnly") {
				if (surgeryType=="evar")
					stop("code for v1other$emergencySurgeryAaaDeathMethod=",
							"\"instantDeathOnly\" and surgeryType=\"evar\" ",
							"has not been written")
				# There is only one probability of death, because for 
				# emergency surgery it makes no difference whether there has 
				# been incidental detection or not. 
				if (v3$probOfDieFromEmergencySurgery) {
					eventHistory <- 
							addEvent(eventHistory, "aaaDeath", eventTime)
					break
				}				
				# Survived surgery. All that remains now is nonAaaDeath. 
				
			} else if (v1other$emergencySurgeryAaaDeathMethod == 
					"survivalModel") { 
				scheduledEvents["aaaDeath"] <- eventTime + 
						generatePostSurgeryAaaDeathTime(v1other, v2, v3, 
						eventTime, surgeryType, "emergency")
				if (is.na(scheduledEvents["aaaDeath"]))
					stop("generatePostSurgeryAaaDeathTime returned NA")
				
			} else {
				stop("v1other$emergencySurgeryAaaDeathMethod=", 
						v1other$emergencySurgeryAaaDeathMethod, " is illegal")
			}

			# Reinterventions. Define surgeryTime, get the appropriate rates 
			# and time-boundaries, and schedule the first reintervention. 
			surgeryTime <- eventTime
			if (eventType == "emergencySurgeryOpen") {
				reinterventionRates <- v2$reinterventionRatesAfterEmergencyOpen
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterEmergencyOpen
				reinterventionEventType <- "reinterventionAfterEmergencyOpen"
			} else if (eventType == "emergencySurgeryEvar") {
				reinterventionRates <- v2$reinterventionRatesAfterEmergencyEvar
				reinterventionTimeBoundaries <- 
						v1other$reinterventionTimeBoundariesAfterEmergencyEvar
				reinterventionEventType <- "reinterventionAfterEmergencyEvar"
			} else {
				stop("eventType is illegal")
			}
			scheduledEvents[reinterventionEventType] <- 
					generateReinterventionTime(
					rates=reinterventionRates, 
					timeBoundaries=reinterventionTimeBoundaries,
					surgeryTime=surgeryTime, currentTime=eventTime, 
					postSurgeryInitialPeriod=v1other$postSurgeryInitialPeriod)

			# Post-surgery monitoring. 
			if (eventType == "emergencySurgeryOpen") {
				scheduledEvents["monitorFollowingOpenSurgery"] <- eventTime + 
						v1other$timeToMonitoringFollowingOpenSurgery
			} else {
				scheduledEvents["monitorFollowingEvarSurgery"] <- eventTime + 
						v1other$timeBetweenMonitoringFollowingEvarSurgery
			}
	
		} else if (eventType %in% c("aaaDeath", "nonAaaDeath", "censored")) {
			break
		
		} else if (eventType == "incidentalDetection") {
			if (getAortaMeasurement(v3, eventTime, method="exact") < 
					v1other$thresholdForIncidentalDetection)
				stop("incidentalDetection happened when aorta was below ",
						v1other$thresholdForIncidentalDetection, "cm")
			scheduledEvents["monitor"] <- eventTime
			scheduledEvents["dropout"] <- generateDropoutTime( 
					v2$rateOfDropoutFromMonitoring, eventTime)
		
		} else if (eventType == "dropout") {
			scheduledEvents["monitor"] <- NA
			scheduledEvents["incidentalDetection"] <- 
					generateIncidentalDetectionTime(
					eventTime, v1other$thresholdForIncidentalDetection, v3, 
					v2$rateOfIncidentalDetection)
	
		} else if (eventType %in% c(
				"reinterventionAfterElectiveOpen", 
				"reinterventionAfterElectiveEvar", 
				"reinterventionAfterEmergencyOpen", 
				"reinterventionAfterEmergencyEvar")) {
			# Just schedule another event of the same type. 
			# This uses surgeryTime, reinterventionRates, and 
			# reinterventionTimeBoundaries, which should have all been created 
			# previously in generateEventHistory. 
			scheduledEvents[eventType] <- 
					generateReinterventionTime(
					rates=reinterventionRates, 
					timeBoundaries=reinterventionTimeBoundaries,
					surgeryTime=surgeryTime, currentTime=eventTime, 
					postSurgeryInitialPeriod=v1other$postSurgeryInitialPeriod)
	
		} else if (eventType == "monitorFollowingOpenSurgery") {
			# Do nothing; this only happens once.
			
		} else if (eventType == "monitorFollowingEvarSurgery") {
			# Schedule the next one.
			scheduledEvents["monitorFollowingEvarSurgery"] <- eventTime + 
					v1other$timeBetweenMonitoringFollowingEvarSurgery
					
		} else {
			stop("INTERNAL ERROR: unknown event-type ", eventType)
		}
	}
	
	# Return the event-history.
	return(eventHistory)
}

################################################################################

# TODO: [not all accurate but still 50% necessary] 
# add code for reinterventions and post-surgery monitoring; for now, 
# make generateReinterventionTime always return Inf or NA, and make 
# monitoringIntervalsPostSurgery be NA or Inf, so that these events 
# never actually happen but men4yearsValidation.R etc. still worok. 
# (However, see http://www.extremeprogramming.org/rules/early.html)


