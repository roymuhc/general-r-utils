###############################################################################################
## This program contains utility functions for computing measures based on clinical/lab data ##
###############################################################################################

####
## Contents:
## ---------
## fn.eGFR.params() -- Function to return appropriate parameters for eGFR computation
## fn.eGFR.compute() -- Function to compute eGFR using the CKD-EPI equation (calls fn.eGFR.params)
####

####
## Function to return appropriate parameters for eGFR computation
####
fn.eGFR.params <- function(blk, sex, scr)
# Returns list of parameters to be used in the computation of eGFR
# race: whether the subject is black or non-black
# sex: whether the subject is male or female (male=1, female=2)
# scr: serum creatinine level in mg/dL
{
	## Prepare parameters (based on Table 2 of Levey et al 2009)
	if (blk==1 & sex==2)
	{
		mult <- 166
		denom <- 0.7
		
		if (scr <= 0.7)
		{
			expon <- -0.329
		}
		else
		{
			expon <- -1.209
		}
	}
	else if (blk==1 & sex==1)
	{
		mult <- 163
		denom <- 0.9
		
		if (scr <= 0.9)
		{
			expon <- -0.411
		}
		else
		{
			expon <- -1.209
		}
	}
	else if (blk==0 & sex==2)
	{
		mult <- 144
		denom <- 0.7
		
		if (scr <= 0.7)
		{
			expon <- -0.329
		}
		else
		{
			expon <- -1.209
		}	
	}
	else if (blk==0 & sex==1)
	{
		mult <- 141
		denom <- 0.9
		
		if (scr <= 0.9)
		{
			expon <- -0.411
		}
		else
		{
			expon <- -1.209
		}
	}
	else
	{
		mult <- NA
		denom <- NA
		expon <- NA
	}
	
	## return list
	return( list(mult=mult, denom=denom, expon=expon) )
}

####
## Function to compute eGFR using the CKD-EPI equation
####
fn.eGFR.compute <- function(blk, sex, scr, age)
# Computes estimated GFR baed on the CKD-EPI equation
# race: whether the subject is black or non-black
# sex: whether the subject is male or female (male=1, female=2)
# scr: serum creatinine level in mg/dL
# age: age at the time the blood sample was taken
# -----------------------------------------------
# Example:
# output <- within( input, eGFR <- Vectorize(fn.eGFR.compute)(blk=black, sex=gender, scr=creat_mgdL, age=age) )
# ---------------------------------------------------------------------------------------------------------
{
	## Retrieve parameters
	params <- fn.eGFR.params(blk, sex, scr)
	
	## Perform computation
	eGFR <- round(params$mult * ( (scr / params$denom) ^ (params$expon) ) * (0.993^age), 1)
	
	## Return results
	return(eGFR)
}
