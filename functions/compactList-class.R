################################################################################
# Functions for dealing with an S3 class called compactList, 
# so that v1other, v2, etc. can be displayed more compactly. 
# 
# Lists nested inside x are also displayed as compactLists, except that if they 
# are lists (not compactLists) then it says "list with ..." instead of 
# "compactList with ..."; see the lines with class(x). Nested lists are also 
# indented, using indentSize. 
# 
# print.compactList is used by processPersons.R, if v0$verbose is TRUE, to 
# show all of v0, v1other, and v2. 

compactList <- function(...) {
	# If there is only one argument, and it is a list, then just convert that  
	# list to a compactList. Otherwise, create a new list. 
	result <- list(...)
	if (length(result) == 1 && is.list(result[[1]])) 
		result <- result[[1]]  # this is the list that was the sole argument
	class(result) <- "compactList"
	result
}

print.compactList <- function(x, ...) {
	if (!is.list(x)) stop("x must be a list")
	
	# Make "spaces". 
	if ("indentSize" %in% names(list(...))) {
		indentSize <- list(...)$indent
	} else {
		indentSize <- 0
	}
	spaces <- paste0(rep(" ", indentSize), collapse="")
	
	# Deal with the case that it is an empty list. 
	if (length(x) == 0) {
		cat("[empty ", class(x), "]\n", sep="")
		return(invisible(x))
	}
	
	# Make elementNames. 
	elementNames <- names(x)
	if (is.null(elementNames))  # (this happens if no elements have names)
		elementNames <- rep("", length(x))
	for (i in 1:length(x)) 
		if (elementNames[i] == "")
			elementNames[i] <- paste0("[[", i, "]]")

	# Display the main output. 
	#cat(class(x), "with", length(x), "elements: ", elementNames, "\n")
	for (i in 1:length(x)) {
		cat(spaces, elementNames[i], " = ", sep="")
		element <- x[[i]]
		typeAttr <- attr(element, "type")
		if (is.data.frame(element)) {
			# Display the data-frame in a compact way. 
			cat("data.frame with ",  ncol(element), " columns:\n", sep="")
			numberOfElementsToShow <- 8
			for (colName in names(element)) 
				cat("  ", colName, " = ", paste(head(element[,colName], 
						numberOfElementsToShow), collapse=" "), 
						{ if(length(element[,colName]) <= 
						numberOfElementsToShow) "" else " ..." }, "\n", sep="")
		} else if (is.list(element)) {
			# If all elements have length 1 then print on one line:
			if (all(sapply(element, FUN=function(el) { length(el)==1 }))) {
				cat(class(element), ": ", sep="")
				for (i in seq_along(element))
					cat(names(element)[i], "=", element[[i]], " ", sep="")
				if (!is.null(typeAttr))
					cat("[type=", typeAttr, "] ", sep="")
				cat("\n")
			} else {
				cat(class(element), "with", length(element), "elements:\n")
				print.compactList(element, indentSize=indentSize + 5)
			}
			# Notes: if element is a list and one element of it is a named  
			# vector, this will probably not yet show what you would want ....
			# A data.frame is also a list.
		} else if (length(element) < 100 && 
				(class(element) %in% c("numeric", "logical", "character"))) {
			if (length(element) == 0) {
				cat(class(element), "(0)", sep="")
			} else if (is.null(names(element))) {
				cat(element)
				cat(" ")
			} else {
				for (i in seq_along(element)) {
					thisName <- names(element)[i]
					cat({ if(thisName=="") "UNNAMED" else thisName }, "=", 
							element[i], " ", sep="")
				}
			}
			if (!is.null(typeAttr))
				cat("[type=", typeAttr, "] ", sep="")
			cat("\n")
		} else if (is.null(element)) {
			cat("NULL\n")
		} else {
			cat("\n")
			print(element)
			#cat("\n")			
		}
	}
	invisible(x)
}

################################################################################
