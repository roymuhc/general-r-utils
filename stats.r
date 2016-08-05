#########################################################################################################
## This program contains utility functions for summarizing data and performing basic statistical tests ##
#########################################################################################################

####
## Contents:
## ---------
## fn.summarize.1w.bin() -- Sub-routine for the function "summarize.1w.disc" defined below (1-way analysis)
## fn.summarize.1w.cat() -- Sub-routine for the function "summarize.1w.disc" defined below (1-way analysis)
## summarize.1w.disc() -- Function calls the above two subroutines to summarize discrete variables (1-way analysis)
## summarize.1w.cont() -- Function summarizes continuous variables (1-way analysis)
## fn.format.pval() -- Function formats p-values according to accepted standards
## summarize.2w.cont() -- Function summarizes continuous variables (2-way analysis) and computes p-values
## fn.robust.fisher() --Function performs a robust version of the Fisher test function by handeling errors and returning NAs instead of crashing
## fn.summarize.2w.bin() -- Sub-routine for the function "summarize.2w.disc" defined below (2-way analysis)
## fn.summarize.2w.cat() -- Sub-routine for the function "summarize.2w.disc" defined below (2-way analysis)
## summarize.2w.disc() -- Function calls the above two subroutines to summarize discrete variables (2-way analysis)
####

####
## NOTE: In order to successfully export these tables to CSV, you need to call write table with these options:
## |> write.table(x=table, file=paste(OUTPATH, "table.csv", sep="/"), sep=",", eol="\n", dec=".", quote=TRUE, col.names=TRUE, row.names=FALSE)
####

####
## Sub-routine for the function "summarize.1w.disc" defined below (1-way analysis)
####
fn.summarize.1w.bin <- function(x, vars, digits=0)
# Function counts TRUE instances and reports them as a percentage of all instances
# Returns a table containing the statistics in a form that can be linked in Excel
# x: data frame object
# vars: vector containing list of logical variables
# digits: number of digits to display for percentage (default=0)
{
	## Initialize the table
	table <- data.frame()
	## Loop over the variables
	for (var in vars)
	{
		## Summarize binary variable
		tmp <- data.frame(list(REF=var,
							   CAT=var,
							   FREQ=format(sum(as.numeric(x[[var]])), big.mark=" "),
							   PERC=paste0("(",
										   format(round(100*(sum(as.numeric(x[[var]]))/length(as.numeric(x[[var]]))), digits), nsmall=digits),
										   "%)")
							  ),
						  stringsAsFactors=FALSE
						 )
		## Merge freq and perc into one column
		tmp <- within( tmp, FREQ_PERC <- paste(FREQ, PERC, sep=" ") )
		tmp["FREQ"] <- NULL
		tmp["PERC"] <- NULL
		## Append to final table
		table <- rbind(table, tmp)
		rm(tmp)
	}
	return(table)
}

####
## Sub-routine for the function "summarize.1w.disc" defined below (1-way analysis)
####
fn.summarize.1w.cat <- function(x, vars, digits=0)
# Function counts number of instances in all distinct categories and reports them as a percentage of all instances
# Returns a table containing the statistics in a form that can be linked in Excel
# x: data frame object
# vars: vector containing list of factor variables
# digits: number of digits to display for percentage (default=0)
{
	## Initialize the table
	table <- data.frame()
	## Loop over the variables
	for (var in vars)
	{
		## Summarize categorical variable
		tmp <- data.frame(list(FREQ=summary(x[[var]]),
							   PERC=paste0("(", format(round(100*(summary(x[[var]])/sum(summary(x[[var]]))), digits), nsmall=digits), "%)")
							  ),
						  stringsAsFactors=FALSE
						 )
		## Copy row names to first column and drop row names
		CAT <- rownames(tmp)
		tmp <- cbind(CAT, tmp)
		rownames(tmp) <- NULL
		## Make FREQ col into a character variable with comma format
		tmp <- within( tmp, FREQ <- format(FREQ, big.mark=" ") )
		## Merge freq and perc into one column
		tmp <- within( tmp, FREQ_PERC <- paste(FREQ, PERC, sep=" ") )
		tmp["FREQ"] <- NULL
		tmp["PERC"] <- NULL
		## Add a reference column for vlookups
		tmp <- within( tmp, REF <- paste(var, CAT, sep="_") )
		## Move it to front
		tmp <- tmp[c("REF","CAT","FREQ_PERC")]
		## Add a sub-header for each variable (optional)
		tmp <- rbind(data.frame(list(REF=var, CAT="", FREQ_PERC=""), stringsAsFactors=FALSE), tmp)
		## Append to final table
		table <- rbind(table, tmp)
		rm(tmp, CAT)
	}
	return(table)
}

####
## Function calls the above two subroutines to summarize discrete variables (1-way analysis)
####
summarize.1w.disc <- function(x, vars, digits=0)
# Function performs analyses defined in the two associated sub-routines and returns a table as described above
# x: data frame object
# vars: vector containing list of discrete variables (factor and logical)
# digits: number of digits to display for percentage (default=0)
{
	## Initialize categorical list
	catlist <- c()
	## Initialize binary list
	binlist <- c()
	
	## Loop through the variables and check if they are factors (categorical) of logical (binary)
	for (var in vars)
	{
		if (class(x[[var]]) == "factor") catlist <- c(catlist, var)
		else if (class(x[[var]]) == "logical") binlist <- c(binlist, var)
	}
	
	## Summarize categorical vars and binary vars and return results
	t1 <- fn.summarize.1w.cat(x=x, vars=catlist, digits=digits)
	t2 <- fn.summarize.1w.bin(x=x, vars=binlist, digits=digits)
	
	## Add a row with the total sample count
	t0 <- data.frame(list(REF="NOBS", CAT="", FREQ_PERC=format(nrow(x), big.mark=" ")), stringsAsFactors=FALSE)
	
	table <- rbind(t0, t1, t2)
	return(table)
}

####
## Function summarizes continuous variables (1-way analysis)
####
summarize.1w.cont <- function(x, vars, sigdig=3, digits=2)
# Function computes summary statistics for a list of continuous variables
# Returns a table containing the statistics in a form that can be linked in Excel
# x: data frame object
# vars: vector containing list of numeric variables (non-logical, non-factor)
# sigdig: number of significant digits to display for summary statistics (default=3)
# digits: number of digits to display after decimal point for summary statistics (default=2)
{
	## Initialize the table
	table <- data.frame()
	## Loop over the variables
	for (var in vars)
	{
		## Summarize continuous variable
		tmp <- data.frame(list(REF = var,
							   MEAN_SD_MD = paste(format(round(mean(x[[var]], na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
												  " \U00B1 ",
												  format(round(sd(x[[var]], na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
												  " [",
												  format(round(median(x[[var]], na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
												  "]",
												  sep=""),
							   MEDIAN_IQR = paste(format(round(median(x[[var]], na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
												  " (",
												  format(round(as.numeric(quantile(x[[var]], probs=0.25, na.rm=TRUE)), digits), scientific=FALSE, big.mark=" "),
												  " ; ",
												  format(round(as.numeric(quantile(x[[var]], probs=0.75, na.rm=TRUE)), digits), scientific=FALSE, big.mark=" "),
												  ")",
												  sep=""),
							   MEAN = format(signif(mean(x[[var]], na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" "),
							   SD = format(signif(sd(x[[var]], na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" "),
							   MIN = format(signif(min(x[[var]], na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" "),
							   Q1 = format(signif(as.numeric(quantile(x[[var]], probs=0.25, na.rm=TRUE)), sigdig), scientific=FALSE, big.mark=" "),
							   MEDIAN = format(signif(as.numeric(quantile(x[[var]], probs=0.5, na.rm=TRUE)), sigdig), scientific=FALSE, big.mark=" "),
							   Q3 = format(signif(as.numeric(quantile(x[[var]], probs=0.75, na.rm=TRUE)), sigdig), scientific=FALSE, big.mark=" "),
							   MAX = format(signif(max(x[[var]], na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" "),
							   N = format(length(x[!is.na(x[[var]]),][[var]]), big.mark=" "),
							   NAs = format(length(x[is.na(x[[var]]),][[var]]), big.mark=" ")
							  ),
						  stringsAsFactors=FALSE
						 )
		## Append to final table
		table <- rbind(table, tmp)
		rm(tmp)
	}
	## Add a row with the total sample count
	t0 <- data.frame(list(REF="NOBS", MEAN_SD_MD=format(nrow(x), big.mark=" "),MEDIAN_IQR="",MEAN="",SD="",MIN="",Q1="",MEDIAN="",Q3="",MAX="",N="",NAs=""), stringsAsFactors=FALSE)
	table <- rbind(t0, table)
	return(table)
}

####
## Function formats p-values according to accepted standards
####
fn.format.pval <- function(x, digits=3)
# Returns a formated p-value
# x: input numeric p-value
# digits: precision of p-value
{
	## If the p-value is greater than the maximum precision, display as is at that level of precision
	if (!is.na(x) & x >= 1/(10^digits))
	{
		return(format(round(x, digits), scientific=FALSE))
	}
	## Else if the p-value is smaller than the maximum precision, display as ">" the max precision
	else if (!is.na(x) & x < 1/(10^digits))
	{
		return(paste("<" , 1/(10^digits), sep=" "))
	}
	## Else there is an error
	else return("-")
}

####
## Function summarizes continuous variables (2-way analysis) and computes p-values
####
summarize.2w.cont <- function(x, vars, byvar, sigdig=3, digits=2, digits.p=3)
# Function computes summary statistics for a list of continuous variables and performs NHST using Wicoxon/Kruskal-Wallis as appropriate
# Returns a table containing the statistics in a form that can be linked in Excel
# x: data frame object
# vars: vector containing list of numeric variables (non-logical, non-factor)
# byvar: name of the stratifying variable (logical or factor)
# sigdig: number of significant digits to display for summary statistics (default=3)
# digits: number of digits to display after decimal point for summary statistics (default=2)
# digits.p: number of significant digits to display for p-values (default=3)
{
	## Initialize the table
	table <- data.frame()
	## Loop over the list of variables
	for (var in vars)
	{
		## Define aggregation functions
		myfun <- function(x)
		{
			c(MEAN_SD_MD = ifelse(length(x[!is.na(x)])==0, "-", ## Return "-" if there are no observations to summarise
						   paste(format(round(mean(x, na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
							     " \U00B1 ",
								 format(round(sd(x, na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
								 " [",
								 format(round(median(x, na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
								 "]",
								 sep="")),
			  MEDIAN_IQR = ifelse(length(x[!is.na(x)])==0, "-", ## Return "-" if there are no observations to summarise
						   paste(format(round(median(x, na.rm=TRUE), digits), scientific=FALSE, big.mark=" "),
								 " (",
								 format(round(as.numeric(quantile(x, probs=0.25, na.rm=TRUE)), digits), scientific=FALSE, big.mark=" "),
								 " ; ",
								 format(round(as.numeric(quantile(x, probs=0.75, na.rm=TRUE)), digits), scientific=FALSE, big.mark=" "),
								 ")",
								 sep="")),
			  ## As above, return "-" if there are no observations to summarise
			  MEAN = ifelse(length(x[!is.na(x)])==0, "-", format(signif(mean(x, na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" ")),
			  SD = ifelse(length(x[!is.na(x)])==0, "-", format(signif(sd(x, na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" ")),
			  MEDIAN = ifelse(length(x[!is.na(x)])==0, "-", format(signif(median(x, na.rm=TRUE), sigdig), scientific=FALSE, big.mark=" ")),
			  N = format(length(x[!is.na(x)]), big.mark=" "),
			  NAs = format(length(x[is.na(x)]), big.mark=" ")
			 )
		}
		
		## Summarize
		tmp <- aggregate(as.formula(paste0(var,"~",byvar)), data=x, FUN=myfun, na.action=na.pass) ## Here we want to pass NAs so that we can count N vs. NAs, but we will remove 
																								  ## them when we compute means, std. devs., medians, etc.
		
		## Transform/reshape
		tmp[["ID"]] <- 1
		tmp <- reshape(tmp, v.names=var, idvar="ID", timevar=byvar, direction="wide")
		tmp[["ID"]] <- NULL
		
		## Add column with variable name
		tmp <- data.frame(REF=var, tmp)
		
		## Rename columns so we can stack records for different variables properly
		names(tmp) <- substr(names(tmp), regexpr(".", names(tmp), fixed=TRUE) + 1, stop=nchar(names(tmp)))
		
		## Perform Wilcoxon's test / Kruskal Wallis test
		## If only 2 levels, use Wiloxon
		if (length(unique(x[[byvar]])) == 2)
		{
			test <- wilcox.test(as.formula(paste0(var,"~",byvar)), data=x, conf.int = TRUE)
			tmp.test <- data.frame(list(REF = var,
									    BY = byvar,
										DIFF = paste(format(signif(test[["estimate"]][[1]], digits), scientific=FALSE, big.mark=" "),
													 " (",
													 format(signif(test[["conf.int"]][1], digits), scientific=FALSE, big.mark=" "),
													 " ; ",
													 format(signif(test[["conf.int"]][2], digits), scientific=FALSE, big.mark=" "),
													 ")",
													 sep=""),
										pval = format(signif(test[["p.value"]], digits.p), scientific=FALSE),
										pval_fmt = fn.format.pval(test[["p.value"]], digits=digits.p),
										ALT = test[["alternative"]],
										TEST = test[["method"]]
										),
								   stringsAsFactors=FALSE
								  )
		}
		## Else if >2 levels, use Kruskal Wallis
		else if (length(unique(x[[byvar]])) > 2)
		{
			test <- kruskal.test(as.formula(paste0(var,"~",byvar)), data=x)
			tmp.test <- data.frame(list(REF = var,
									    BY = byvar,
										DIFF = "-",
										pval = format(signif(test[["p.value"]], digits.p), scientific=FALSE),
										pval_fmt = fn.format.pval(test[["p.value"]], digits=digits.p),
										ALT = "-",
										TEST = test[["method"]]
										),
								   stringsAsFactors=FALSE
								  )
		}
		else stop("Invalid number of levels in the stratifying byvar variable.")
		
		## Merge
		tmp <- merge(x=tmp, y=tmp.test, by="REF") ## Inner join
	
		## Append to final table
		table <- rbind(table, tmp)
		rm(tmp, test, tmp.test)
	}
	return(table)
}

####
## Function performs a robust version of the Fisher test function by handeling errors and returning NAs instead of crashing
####
fn.robust.fisher <- function(x, var, byvar, B)
# Function returns the results of the Fisher test if there are not errors, and returns NAs if there are errors
# The most common error is when either "var" or "byvar" have < 2 levels, leading to the impossibility of performing the test
# x: data frame object
# var: name of logical/factor variables
# byvar: name of the stratifying variable (logical or factor)
# B: Number of bootstrap replicates for Fisher approx.
{
	tryCatch(fisher.test(x[[var]], x[[byvar]], simulate.p.value=TRUE, B=B),
			 error=function(e) {list(p.value=NA, alternative="Error", method="Need >1 level per variable")}
			)
}

####
## Sub-routine for the function "summarize.2w.disc" defined below (2-way analysis)
####
fn.summarize.2w.bin <- function(x, vars, byvar, digits=0, digits.p=3, B=1e3)
# Function counts TRUE instances and reports them as a percentage of all instances, but stratified
# Returns a table containing the statistics in a form that can be linked in Excel
# x: data frame object
# vars: vector containing list of logical variables
# byvar: name of the stratifying variable (logical or factor)
# digits: number of digits to display for percentage (default=0)
# digits.p: number of digits to display for p-values (default=3)
# B: Number of bootstrap replicates for Fisher approx. (default=1000)
{
	## Initialize the table
	table <- data.frame()

	## Loop over the strata in the byvar variable of interest
	strata <- unique(x[[byvar]])
	for (stratum in strata)
	{
		## Summarize data for this stratum
		tmp <- fn.summarize.1w.bin(x=x[x[[byvar]]==stratum,], vars=vars, digits=digits)
		## Add the byvar to the table for reference
		tmp[["BY"]] <- byvar
		## Rename column of interest to reflect the stratum
		names(tmp)[names(tmp)=="FREQ_PERC"] <- paste(stratum, "FREQ_PERC", sep=".")
		## Start merging in the columns
		## If the table is empty, re-initialize it
		if (all(dim(table)==c(0,0)))
		{
			table <- tmp
		}
		## Otherwise, merge the new column in
		else
		{
			table <- merge(x=table, y=tmp, by=c("REF","CAT","BY"), all.x=TRUE, all.y=TRUE) ## Full join
		}
		rm(tmp)
	}
	
	## Now, we generate p-values and merge them in
	## Initialize the p-value table
	ptable <- data.frame()
	## Loop over the list of variables
	for (var in vars)
	{
		## Perform Fisher's exact test for var1 vs. var2
		test <- fn.robust.fisher(x=x, var=var, byvar=byvar, B=B) ## Only uses simulation-based approx. when table is larger than 2 by 2
		## Format results
		tmp <- data.frame(list(CAT = var,
							   BY = byvar,
							   pval = format(signif(test[["p.value"]], digits.p), scientific=FALSE),
							   pval_fmt = fn.format.pval(test[["p.value"]], digits=digits.p),
							   ALT = test[["alternative"]],
							   TEST = test[["method"]]
							   ), 
						  stringsAsFactors=FALSE
						 )
		## Append to final table
		ptable <- rbind(ptable, tmp)
		rm(test, tmp)
	}
	
	## Merge p-value to main table
	table <- merge(x=table, y=ptable, by=c("CAT","BY"), all.x=TRUE) ## Left join
	## Reorder cols to have the ref column first
	col_idx <- grep("REF", names(table))
	table <- table[, c(col_idx, (1:ncol(table))[-col_idx])]
	return(table)
}

####
## Sub-routine for the function "summarize.2w.disc" defined below (2-way analysis)
####
fn.summarize.2w.cat <- function(x, vars, byvar, digits=0, digits.p=3, B=1e3)
# Function counts number of instances in all distinct categories and reports them as a percentage of all instances, but stratified
# Returns a table containing the statistics in a form that can be linked in Excel
# x: data frame object
# vars: vector containing list of factor variables
# byvar: name of the stratifying variable (logical or factor)
# digits: number of digits to display for percentage (default=0)
# digits.p: number of digits to display for p-values (default=3)
# B: Number of bootstrap replicates for Fisher approx. (default=1000)
{
	## Initialize the table
	table <- data.frame()

	## Loop over the strata in the byvar variable of interest
	strata <- unique(x[[byvar]])
	for (stratum in strata)
	{
		## Summarize data for this stratum
		tmp <- fn.summarize.1w.cat(x=x[x[[byvar]]==stratum,], vars=vars, digits=digits)
		## Add the byvar to the table for reference
		tmp[["BY"]] <- byvar
		## Rename column of interest to reflect the stratum
		names(tmp)[names(tmp)=="FREQ_PERC"] <- paste(stratum, "FREQ_PERC", sep=".")
		## Start merging in the columns
		## If the table is empty, re-initialize it
		if (all(dim(table)==c(0,0)))
		{
			table <- tmp
		}
		## Otherwise, merge the new column in
		else
		{
			table <- merge(x=table, y=tmp, by=c("REF","CAT","BY"), all.x=TRUE, all.y=TRUE) ## Full join
		}
		rm(tmp)
	}
	
	## Now, we generate p-values and merge them in
	## Initialize the p-value table
	ptable <- data.frame()
	## Loop over the list of variables
	for (var in vars)
	{
		## Perform Fisher's exact test for var1 vs. var2
		test <- fn.robust.fisher(x=x, var=var, byvar=byvar, B=B) ## Only uses simulation-based approx. when table is larger than 2 by 2
		## Format results
		tmp <- data.frame(list(REF = var,
							   BY = byvar,
							   pval = format(signif(test[["p.value"]], digits.p), scientific=FALSE),
							   pval_fmt = fn.format.pval(test[["p.value"]], digits=digits.p),
							   ALT = test[["alternative"]],
							   TEST = test[["method"]]
							   ), 
						  stringsAsFactors=FALSE
						 )
		## Append to final table
		ptable <- rbind(ptable, tmp)
		rm(test, tmp)
	}
	
	## Merge p-value to main table
	table <- merge(x=table, y=ptable, by=c("REF","BY"), all.x=TRUE) ## Left join
	## Reorder cols to have the ref column first
	col_idx <- grep("REF", names(table))
	table <- table[, c(col_idx, (1:ncol(table))[-col_idx])]
	return(table)
}

####
## Function calls the above two subroutines to summarize discrete variables (2-way analysis)
####
summarize.2w.disc <- function(x, vars, byvar, digits=0, digits.p=3, B=1e3)
# Function performs analyses defined in the two associated sub-routines and returns a table as described above
# x: data frame object
# vars: vector containing list of discrete variables (factor and logical)
# byvar: name of the variable by which we want to stratify
# digits: number of digits to display for percentage (default=0)
# digits.p: number of digits to display for p-value (default=3)
# B: number of bootstrap replications for larger than 2 by 2 tables (Fisher's approx. test, default=1000)
{
	## Initialize categorical list
	catlist <- c()
	## Initialize binary list
	binlist <- c()
	
	## Loop through the variables and check if they are factors (categorical) of logical (binary)
	for (var in vars)
	{
		if (class(x[[var]]) == "factor") catlist <- c(catlist, var)
		else if (class(x[[var]]) == "logical") binlist <- c(binlist, var)
	}
	
	## Summarize categorical vars and binary vars and return results
	t1 <- fn.summarize.2w.cat(x=x, vars=catlist, byvar=byvar, digits=digits, digits.p=digits.p, B=B)
	t2 <- fn.summarize.2w.bin(x=x, vars=binlist, byvar=byvar, digits=digits, digits.p=digits.p, B=B)
	
	## Count number of observations in each stratum (uses rownames vector as basis, by default. To check if it can lead to issues. Prefered this to using "ID")
	t0 <- as.data.frame(t(tapply(rownames(x), x[[byvar]], length)))
	## Format frequencies (and drop columns with length = NA)
	for (name in names(t0))
	{
		## If length is NA, drop column
		if (is.na(t0[[name]])) t0[[name]] <- NULL
		## Else, format value
		else t0[[name]] <- format(t0[[name]], big.mark=" ")
	}
	## Change names to match with above col names
	names(t0) <- paste(names(t0),"FREQ_PERC",sep=".")
	## Add columns to match format as well
	t0 <- data.frame(list(REF="NOBS", BY=byvar, CAT=""), t0, list(pval="", pval_fmt="", ALT="", TEST=""), stringsAsFactors=FALSE)
	
	table <- rbind(t0, t1, t2)
	return(table)
}
