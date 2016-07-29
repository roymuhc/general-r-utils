#################################################################################################
## This program contains miscelaneous utility functions that can prove useful in cleaning data ##
#################################################################################################

####
## Contents:
## ---------
## is.equal() -- Function replaces the idiom is_x <- (y==x) to avoid outputing NA's when y is NA
## check.env() -- Function checks environment for missing objects and stops program if any are missing
## check.ID() -- Function checks the IDs in a table and stops if there are issues with the IDs in the data
## fmt.dates() -- Function checks, corrects, and formats dates
## fmt.category() -- Function formats categorical variables to have only one value for missing
####

####
## Function replaces the idiom is_x <- (y==x) to avoid outputing NA's when y is NA
####
is.equal <- function(var, value)
# Returns TRUE if var = value and FALSE otherwise
# var: variable to be checked
# value: value to check for equality with variable
{
	if (length(value) == 1) {
		return( (var==value & !is.na(var)) )
	} else {
		return( (var %in% value & !is.na(var)) )
	}
}

####
## Function checks environment for missing objects and stops program if any are missing
####
check.env <- function(x)
# Stops program if env objects are missing
# x: vector of object names in quotes
{
	## Loop through all the objects and stop program once a missing one is found
	for (var in x)
	{
		if (!exists(var))
		{
			stop(paste(var, "not in env", sep=" "))
		}
	}
}

####
## Function checks the IDs in a table and stops if there are issues with the IDs in the data
####
check.ID <- function(x)
# Stops program if there are any issues with the IDs in the data
# x: data.frame object
{
	##
	if ( (unique(nchar(x[["ID"]])) != 4) | 
		 (length(intersect(unique(substr(x[["ID"]],1,1)), c("0","1","2","3","4","5","6","7","8","9"))) != 0) | 
		 is.na(sum(as.numeric(unique(substr(x[["ID"]],2,4)))))
	   ) 
	{
		stop("ID variable contains invalid records. Please verify data.")
	}
}

####
## Function checks, corrects, and formats dates
####
fmt.dates <- function(x, dates)
# Function returns a data frame with checked, corrected, and formated dates setting the time origin and yyyy-mm-dd format
# x: data.frame object
# dates: a vector date variables in quotes
{
	## Loop through the dates
	for (date in dates)
	{
		## Format the date variable
		x[[date]] <- as.Date(x[[date]], format="%Y-%m-%d", origin=TIMEORIGIN)
		## Replace 1900 dates with NA
		if (length(x[ !is.na(x[[date]]) & (x[[date]] == as.Date("1900-01-01", format="%Y-%m-%d", origin=TIMEORIGIN)), ][[date]]) != 0)
		{
			x[ !is.na(x[[date]]) & (x[[date]] == as.Date("1900-01-01", format="%Y-%m-%d", origin=TIMEORIGIN)), ][[date]] <- NA
		}
	}
	## Return data frame with corrections
	return(x)
}

####
## Function formats categorical variables to have only one value for missing
####
fmt.category <- function(x, vars, missing)
# Function returns a data frame with categories representing missing all coded as NA
# x: data.frame abject
# vars: a vector of variable names in quotes
# missing: a vector of numerical values representing missing values
{
	## Loop through the variables
	for (var in vars)
	{
		## Set values of var equal to NA when they are among the values considered as missing
		if (length(x[ !is.na(x[[var]]) & (x[[var]] %in% missing), ][[var]]) != 0)
		{
			x[ !is.na(x[[var]]) & (x[[var]] %in% missing), ][[var]] <- NA
		}
	}
	## Return data frame with corrections
	return(x)
}
