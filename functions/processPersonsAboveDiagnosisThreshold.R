### MJS. 07/03/17 This function will get very accurate estimates of incremental effects and costs with fewer indivduals
### It assumes that there are zero incremental effects below the diagnosis threshold between invited and non-invited groups
### It cannot be used to get total numbers of events in the population but can be used for PSA analyses

processPersonsAboveDiagnosisThreshold <- function(v0, v1other, v2, 
		threshold=3.0, updateProgress=NULL) {
	
  ## MS CHANGED FOR SHINY APP
  #v0$returnMeanQuantities <- TRUE
	#v0$returnEventHistories <- FALSE  
	
	## ADDED BY MJS 02/03/17 TO DO WEIGHTING OF BASELINE DISTRIBUTION OUTSIDE OF PROCESSPERSONS
	## BUT MUST NAME NEW OBJECT BASELINEDIAMETERS AND SET PREVALENCE TO NULL TO AVOID WEIGHTING A 2ND TIME
	# Change the prevalence, if v2$prevalence exists.
	if ("prevalence" %in% names(v2)) {
	  v1other$baselineDiameters <- 
	    changePrevalence(baselineDiameters=v1other$baselineDiameters, 
	                     threshold=v1other$prevalenceThreshold, prevalence=v2$prevalence)
	 if (v0$verbose)
	   cat("Prevalence",
	       "has been\n changed to ", v2$prevalence, 
	       ", using threshold=v1other$prevalenceThreshold=",
	       v1other$prevalenceThreshold, ".\n", sep="")
	  v2$prevalence<-NULL ## TO AVOID PROCESSPERSON REWEIGHTING
	} else {
	  v1other$baselineDiameters <- v1other$baselineDiameters
	  if (v0$verbose) cat("v2$prevalence does not exist, so \n",
	                      " v2$baselineDiameters left as is",
	                      sep="")
	}
	
	
	# Make v1bd and find the true proportion who are below the threshold.
	v1bd <- v1other$baselineDiameters
	trueProportionBelowThreshold <- 
			sum(v1bd$weight[v1bd$size < threshold]) / sum(v1bd$weight)
	
	
	# Create v0over and v1over, for the run of processPersons in which all 
	# baseline diameters are greater or equal to the threshold.
	v0over <- v0
	v1otherOver <- v1other
	v1otherOver$baselineDiameters$weight[v1bd$size < threshold] <- 0
	
	# Run processPersons just once based on distribution of diameters greater or equal to the threshold.
	resultOver <- processPersons(v0over, v1otherOver, v2,updateProgress)
	# Obtain incremental effects and costs for this population, weighted by the proportion of people >= diagnosis threshold
	result<-list(incrementalMeanQuantities=(1-trueProportionBelowThreshold)*
	               (resultOver$meanQuantities["screening",]-resultOver$meanQuantities["noScreening",]))
	
	# Calculate incremental costs for those below the threshold
	# This is just the screening costs for the screened group
	screening.costs<-v2$costs[c("inviteToScreen","requireReinvitation","screen")]
	mean.screening.costs.in.screened.normals<- screening.costs["inviteToScreen"]+v2$probOfRequireReinvitation*screening.costs["requireReinvitation"]+
	  v2$probOfAttendScreen*screening.costs["screen"]
	
	result$incrementalMeanQuantities["cost"]<-result$incrementalMeanQuantities["cost"]+trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	# Discounted screening costs are the same as undiscounted as they all happen at time zero.
	result$incrementalMeanQuantities["discountedCost"]<-result$incrementalMeanQuantities["discountedCost"]+trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	
	
	if(v0$returnAllPersonsQuantities){
	   temp <- apply(sapply(X=resultOver$allPersonsQuantities, FUN=function(x) {
	     (1-trueProportionBelowThreshold)*(x$screening - x$noScreening)}),1,function(i){cumsum(i)/seq_along(i)})
	   temp[,"cost"]<-temp[,"cost"]+trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	   temp[,"discountedCost"]<-temp[,"discountedCost"]+trueProportionBelowThreshold*mean.screening.costs.in.screened.normals
	   result$incrementalCumMean<-temp
	 }
	
  if(v0$returnEventHistories){

    ## As above the difference in the event numbers is a the difference in numbers from the sampled over the threshold model * proportion over threshold
    #events<-eventsandcosts(resultOver)
    #events2<-data.frame(event=events$event,incrementalEvents=(1-trueProportionBelowThreshold)*(events[,"screening.n",drop=F]-events[,"noScreening.n"]))
    events<-tab.events(resultOver,v0=v0,v1other=v1other)[,c(1,2,5)]
    events[,"Difference"]<-(1-trueProportionBelowThreshold)*events[,"Difference"]
    result$incrementalEvents<-events
    
    result$eventHistories<-resultOver$eventHistories
  }	
	
	result$trueProportionBelowThreshold<-trueProportionBelowThreshold
	
	return(result)
}

################################################################################
