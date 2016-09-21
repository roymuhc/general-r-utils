#########################################################################################
## This program contains utility functions for creating exploratory graphical displays ##
#########################################################################################

####
## Contents:
## ---------
## fn.credplot() -- Sub-routine for other functions such as forest.or.3way defined below (3-way analysis)
## forest.or.3way() -- Function computes odds ratios stratified by a 3rd variable and plots results as a forest plot
####

####
## Function uses output from another function and lattice graphics to plot a forest plot of estimates with 95% CI
####
fn.credplot <- function (x, cen = NA, ...)
## Taken from a blog post by BioStatMatt and slightly modified
## Link: http://biostatmatt.com/wiki/r-credplot
# credplot - a type of forest plot
# for credible (and other) intervals 
# m   - estimates
# mlo - lower bound
# mhi - upper bound
# cen - if(!is.na(cen)) abline(v=cen)
{
	namelist <- x[["LEVEL"]]
	m <- x[["OR"]]
	mlo <- x[["LCL"]]
	mhi <- x[["UCL"]]

    require(lattice)
    if(is.null(namelist))
        names(m) <- as.character(1:length(m))
	else
		names(m) <- namelist
    rge <- range(c(mlo, mhi))
    rge[1] <- rge[1] - 0.05 * diff(rge)
    rge[2] <- rge[2] + 0.05 * diff(rge)
    dotplot(m, mlo = mlo, mhi = mhi, cen = cen, xlim = rge, ..., 
        panel = function(x, y, mlo = mlo, mhi = mhi, cen = cen, 
            horizontal, levels.fos, ...) {
            if (missing(levels.fos)) {
                if (horizontal) 
                    levels.fos <- unique(y)
                else 
                    levels.fos <- unique(x)
            }
            if (horizontal) 
                panel.segments(x0 = mlo, y0 = levels.fos,
                  x1 = mhi, y1 = levels.fos)
            else 
                panel.segments(x0 = levels.fos, y0 = mlo, 
                  x1 = levels.fos, y1 = mhi)
            if (!is.na(cen)) {
                if (horizontal) 
                    panel.abline(v = cen, lty = 2)
                else 
                    panel.abline(h = cen, lty = 2)
            }
            panel.dotplot(x, y, horizontal, 
                    levels.fos = levels.fos, lty=0,...)
        })
}

####
## Function computes odds ratios stratified by a 3rd variable and plots results as a forest plot
####
forest.or.3way <- function(x, var, byvar, stratvar, cen=1, digits=2)
# x: data frame object
# var: name of variable 1
# byvar: name of variable 2
# stratvar: name of variable 3 (the one used to stratify the odds ratios)
# cen: Null line (default = 1)
# digits: Number of digits to report in table for OR and CI (default = 2)
{
	## First, check if stratvar is a factor/logical variable or not
	if ( class(x[[stratvar]]) %in% c("factor","logical") )
	{
		## If so, coerce to factor
		x[[stratvar]] <- factor(x[[stratvar]])
	}
	## Otherwise, stratify into quartiles and coerce to factor
	else
	{
		x[["contvar"]] <- x[[stratvar]]
		q <- c( quantile(x[["contvar"]], 0.25, na.rm=TRUE),
			    quantile(x[["contvar"]], 0.50, na.rm=TRUE),
			    quantile(x[["contvar"]], 0.75, na.rm=TRUE),
			    quantile(x[["contvar"]], 1.00, na.rm=TRUE) )
		x[[stratvar]] <- factor(ifelse(x[["contvar"]] <= q[1], "Q1", 
								  ifelse(x[["contvar"]] <= q[2], "Q2",
								    ifelse(x[["contvar"]] <= q[3], "Q3", "Q4"))))
		x[["contvar"]] <- NULL
	}
	
	## Second, loop through strata and compute odds ratio and 95% CI
	table <- data.frame()
	levels <- sort(unique(x[[stratvar]]))
	for (level in levels)
	{
		## use Fisher's test to compute stats for the data in the level
		x.sub <- x[x[[stratvar]] == level,]
		or.tmp <- fisher.test(x.sub[[var]], x.sub[[byvar]])
		
		## Format results into a nice table
		tmp <- data.frame(list(STRATVAR = stratvar,
							   LEVEL = level,
							   OR = round(as.numeric(or.tmp[["estimate"]]), digits),
							   LCL = round(as.numeric(or.tmp[["conf.int"]][1]), digits),
							   UCL = round(as.numeric(or.tmp[["conf.int"]][2]), digits)))
		
		## Append to final table
		table <- rbind(table, tmp)
		rm(tmp, x.sub, or.tmp)
	}
	
	## Third, plot the graph
	print(fn.credplot(x=table, cen=cen, main="Forest Plot: Stratified Odds Ratios", ylab=stratvar, xlab=paste("Odds Ratio:", var, "vs.", byvar)))
	
	## return final table to see numbers as well
	return(table)
}
