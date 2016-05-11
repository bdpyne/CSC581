################################################################################
#        Name: 
# Description: 
################################################################################
setup <- function(outfile) {

	library(e1071)

    # Read the original file from the download.
    df <- read.csv("original.csv")

	# Removing because correlation is not being tested for them
	df$dteday     <- NULL
    df$casual     <- NULL
    df$registered <- NULL
	df$season     <- NULL
	df$yr         <- NULL
	df$mnth       <- NULL
	df$casual     <- NULL
	df$registered <- NULL

	# Remove day-related columns to focus on weather
#	df$holiday    <- NULL
#	df$weekday    <- NULL
#	df$workingday <- NULL

	# Remove weather-related columns to focus on day
#	df$weathersit <- NULL
#	df$temp       <- NULL
#	df$atemp      <- NULL
#	df$hum        <- NULL
#	df$windspeed  <- NULL


    # Computer the median of total riders per day.
    med <- median(df$cnt)

    # Use the median to create a binary, dependent variable.
    df <- df[, -1]
    df <- within(df, ridersup <- ifelse(cnt > med, "UP", "NOTUP"))

    # Write the modified data set to a file.
    write.csv(df, outfile)
}



###############################################################################
#
###############################################################################
runLinearKernel <- function(riders) {

	# Allocate a blank data frame
	results <- data.frame()

	# Index for adding rows. Allow for the column names as the first row.
	j            <- 1



	# Only 700+ rows, so a cost of 100 is probably the highest to consider.
	# Run as a batch: analyze in a separate function.
	for (i in 0:3) {

		cost    <- 10^i
		model   <- svm(ridersup~., data=riders, type="C-classification", cost=cost, kernel="linear")
		predict <- fitted(model)
		cm      <- table(riders$ridersup, predict)
		err     <- (cm[1, 2] + cm[2, 1] / length(predict) * 100)

		results[j,1] <- cost
		results[j,2] <- cm[1,1]
		results[j,3] <- cm[1,2]
		results[j,4] <- cm[2,1]
		results[j,5] <- cm[2,2]
		results[j,6] <- err

		j <- j + 1
	}

	# Make the headers a little more meaningful
	colnames(results) <- c("Cost","TP","FN","FP","TN","Error")

	return(results)
}


###############################################################################
#
###############################################################################
runPolynomialKernel <- function(riders) {

	# Allocate a blank data frame
	results <- data.frame()

	# Index for adding rows. Allow for the column names as the first row.
	j            <- 1



	# Only 700+ rows, so a cost of 100 is probably the highest to consider.
	# Run as a batch: analyze in a separate function.
	for (i in 0:3) {
		for (j in 1:3) {

		    # Run it once with these deg and gam free variable settings.
		    cost    <- 10^i
		    deg     <- j
		    model   <- svm(ridersup~., data=riders, type="C-classification", cost=cost, kernel="polynomial", degree=deg)
		    predict <- fitted(model)
		    cm      <- table(riders$ridersup, predict)
		    err     <- (cm[1, 2] + cm[2, 1] / length(predict) * 100)

		    results[j,1] <- cost
		    results[j,2] <- cm[1,1]
		    results[j,3] <- cm[1,2]
		    results[j,4] <- cm[2,1]
		    results[j,5] <- cm[2,2]
		    results[j,6] <- err
		    results[j,7] <- deg
		}
	}

	# Make the headers a little more meaningful
	colnames(results) <- c("Cost","TP","FN","FP","TN","Error","Degree")

	return(results)
}

###############################################################################
#
###############################################################################
runRadialKernel <- function(riders) {

	# Allocate a blank data frame
	results <- data.frame()

	# Index for adding rows. Allow for the column names as the first row.
	j            <- 1
	g            <- c(0.1,0.2,0.3,0.4,0.5,0.7,0.8,0.9)


	# Only 700+ rows, so a cost of 100 is probably the highest to consider.
	# Run as a batch: analyze in a separate function.
	for (i in 0:3) {
		for (j in 1:3) {
			# Run it once with these deg and gam free variable settings.
			cost    <- 10^i
			gam     <- g[j]
			model   <- svm(ridersup~., data=riders, type="C-classification", cost=cost, kernel="radial", gamma=gam)
			predict <- fitted(model)
			cm      <- table(riders$ridersup, predict)
			err     <- (cm[1, 2] + cm[2, 1] / length(predict) * 100)

			results[j,1] <- cost
			results[j,2] <- cm[1,1]
			results[j,3] <- cm[1,2]
			results[j,4] <- cm[2,1]
			results[j,5] <- cm[2,2]
			results[j,6] <- err
			results[j,7] <- gam
		}
	}

	# Make the headers a little more meaningful
	colnames(results) <- c("Cost","TP","FN","FP","TN","Error","Gamma")

	return(results)
}

################################################################################
#        Name: run
# Description: 
################################################################################
analyze <- function(dfile) {

	data.file  <- dfile 
	universe   <- read.csv(data.file)

    no.rows    <- nrow(universe)
	cat("Universe size: ", no.rows, "\n")

	# Create a simplistic partition. Can't just divide the
	# data frame in half because the "yr" column has no
	# uniqueness and causes an error.
	p1  <- universe[which(universe$X %% 2 == 0),]
	cat("Training partition size: ", nrow(p1), "\n")

	# The index is not relevant at this point
	p1$X  <- NULL


	p2  <- universe[which(universe$X %% 2 != 0),]
	cat("Testing partition size: ", nrow(p2), "\n")

	# The index is not relevant at this point
	p2$X  <- NULL


	###############################################################################
	# Now that the partitions are setup, time to start evaluating.
	###############################################################################

	cat("**********************************\n")
	cat("         Top 2\n")
	cat("**********************************\n")

	# Linear
	linear.trained <- runLinearKernel(p1)
	linear.tested  <- runLinearKernel(p2)
	linear.best    <- evaluate.models(linear.trained, linear.tested)

	printBest("Linear", linear.tested[linear.best,])

	# Polynomial
	poly.trained <- runPolynomialKernel(p1)
	poly.tested  <- runPolynomialKernel(p2)
	poly.best    <- evaluate.models(poly.trained, poly.tested)

#	printBest("Polynomial", poly.tested[poly.best,])

	# Radial
	radial.trained <- runRadialKernel(p1)
	radial.tested  <- runRadialKernel(p2)
	radial.best    <- evaluate.models(radial.trained, radial.tested)

	printBest("Radial", radial.tested[radial.best,])

	###############################################################################
	# Calculate confidence interval.
	###############################################################################

#	linear.err <- bootstrap(p1, linear.tested[linear.best,])

#	cat("Linear upper bound is ", linear.err[195,], "\n")
#	cat("Linear lower bound is ", linear.err[5,], "\n")

	radial.err <- bootstrap(p1, radial.tested[radial.best,])

	cat("Radial upper bound is ", radial.err[195,], "\n")
	cat("Radial lower bound is ", radial.err[5,], "\n")
}

################################################################################
#        Name: 
# Description: 
################################################################################
run.linear.once <- function(df, cost) {

		model   <- svm(ridersup~., data=df, type="C-classification", cost=cost, kernel="linear")
		predict <- fitted(model)
		cm      <- table(df$ridersup, predict)
		err     <- (cm[1, 2] + cm[2, 1] / length(predict) * 100)

		return(err)
}

################################################################################
#        Name: 
# Description: 
################################################################################
run.radial.once <- function(df, cost, gam) {

		model   <- svm(ridersup~., data=df, type="C-classification", cost=cost, kernel="radial", gamma=gam)
		predict <- fitted(model)
		cm      <- table(df$ridersup, predict)
		err     <- (cm[1, 2] + cm[2, 1] / length(predict) * 100)

		return(err)
}

################################################################################
#        Name: 
# Description: 
################################################################################
bootstrap  <- function(df, best) {

	# Collect the err's in here
	err       <- data.frame()

	no.rows   <- nrow(df)
	tr1.size  <- floor(no.rows/2)
	tr2.start <- tr1.size + 1

	# Take a subset of the training set for hold out
	tr1      <- df[1:tr1.size,]

	hold.out <- df[tr2.start:no.rows,]

	for (i in 1:200) {
		bs  <- sample(x=tr1, size=no.rows, replace=TRUE)
#		err[i,1]  <- run.linear.once(bs, best$Cost)
		err[i,1]  <- run.radial.once(bs, best$Cost, best$Gamma)
	}

	order(err)

	return(err)
}


################################################################################
#        Name: 
# Description: 
################################################################################
evaluate.models <- function(trained, tested) {

    # Put the variable into function scope and default to 1
	rownum <- 1

	# Keep track of the previous best
	prev   <- 1


	# The 2 data frames should have the same number of rows. So, use either to
	# get the number of rows.
	for (i in 1:nrow(trained)) {
	    if ((trained[i,]$Error >= tested[i,]$Error) &
		    (trained[i,]$Error < trained[prev,]$Error)) {
			rownum <- i
		}   
	}   

    return(rownum)
}

################################################################################
#        Name: 
# Description: 
################################################################################
printBest <- function(krnl, bst) {
	cat(krnl, "\n")
	cat("=====================================\n")
	print(bst)
	cat("\n")
}
