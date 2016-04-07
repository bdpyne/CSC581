###############################################################################
#
###############################################################################
run <- function() {

	data(iris)
	df    <- as.data.frame(iris)
	rec   <- as.data.frame(df[137,])

	print(sprintf("Label to find: %s", rec$Species))

#	class <- classify(df, rec)
#	print(class)

	class <- my.test(df, rec)
	print(sprintf("Label found is: %s", class))
}



###############################################################################
#
###############################################################################
get.row.num <- function(ds, rec)  {

	idx <- 0

	l.rec <- as.data.frame(ds[ (ds$Sepal.Length == rec$Sepal.Length) & (ds$Sepal.Width  == rec$Sepal.Width) & (ds$Petal.Length == rec$Petal.Length)  & (ds$Petal.Width  == rec$Petal.Width), ])

	idx <- l.rec$Id

	# When a record doesn't exist in the set, idx is assigned something of length 0.
	# Need an integer returning always.
	ifelse(length(idx) == 0, idx <- 0, idx)

	return(idx)
}

###############################################################################
#
###############################################################################
classify <- function(df, pnt) {

	# Convert for convenience
	rec <- as.data.frame(pnt)	

	# Get the point from the data frame - force it to be a vector
	rec.label <- rec$Species

	# Initialize just to let the interpreter know the data type
	class <- ""

	# Get the unique labels from the dependent column
	labels <- unique(df[,ncol(df)])

	# How many labels?
	labels.cnt <- length(labels)

	# Use the vector allocator to create a vector of the same length as the 
	# number of labels
	# Doing so also initializes the points to 0
	cnt <- integer(labels.cnt)

	p.max <- labels.cnt - 1

	for (p in 1:p.max) {
		
		print(p)

		q.min <- p + 1

		for (q in q.min:labels.cnt) {

			print(q)

			# Get the label values 
			l1 <- labels[p]
			l2 <- labels[q]

			# Get a new frame that is the union of # the rows matching 
			# l1 and l2
			ds <- df[ df[,ncol(df)] == l1 | df[,ncol(df)] == l2 ,]

			# Create a duplicate of the ds data frame and add a row number
			ds.with.index    <- data.frame(ds)
            ds.with.index$Id <- seq.int(nrow(ds.with.index))


            # Get row number of the rec in the data set.
			# If none exists, then the value is 0.
			row.num <- get.row.num(ds.with.index, rec)


			print(row.num)	
			print(is.null(row.num))
			print(is.integer(row.num))

			# Is in this pair of sets
			if (row.num > 0) {
				


			###########################################################
			# Train the data using svm() in this section
			###########################################################
			model <- svm(Species~., data=ds, type="C-classification", cost=1, kernel="linear")

			###########################################################
			# Find out how it did.
			###########################################################
			predict <- as.data.frame(fitted(model))

			# ******  Need to find rec in the prediction set ******

			predict.label <- predict[row.num,1]

#			print(predict)

			# *****************************************************


		    if (predict.label == rec.label) {
				cnt[p] <- cnt[p] + 1
			}
			else {
				cnt[q] <- cnt[q] + 1
			}

			print(cnt)

			###########################################################
			###########################################################
			}
		}


	}

	# Get the maximum label count
	max.cnt <- max(cnt)

	# Now its index
	max.cnt.idx <- match(max.cnt, cnt)

	# Now get the label it's attached to
	class <- labels[max.cnt.idx]


	return(class)
}

my.test <- function(dfIn, rec) {

	# Get the point from the data frame - force it to be a vector
	rec.label <- rec$Species[1]

	# Initialize just to let the interpreter know the data type
	class <- ""

	# Get the unique labels from the dependent column
	labels <- unique(df$Species)

#	print(labels)

	# How many labels?
	labels.cnt <- length(labels)

	# Use the vector allocator to create a vector of the same length as the 
	# number of labels
	# Doing so also initializes the points to 0
	cnt <- integer(labels.cnt)

	# Get the upper index
	p.max <- labels.cnt - 1

	for (p in 1:p.max) {
		
#		cat("p: ", p, "\n")

		l1    <- labels[p]
#		print(l1)

		# Get the lower index
		q.min <- p + 1

		for (q in q.min:labels.cnt) {
#			cat("q: ", q, "\n")

			# Get the label values 
			l2 <- labels[q]
			
#			print(l2)

			# Get a new frame that is the union of # the rows matching 
			# l1 and l2
			ds <- df[ df[,ncol(df)] == l1 | df[,ncol(df)] == l2 ,]

			# Create a duplicate of the ds data frame and add a row number
			ds.with.index    <- data.frame(ds)
            ds.with.index$Id <- seq.int(nrow(ds.with.index))

#			print(ds.with.index)

            # Get row number of the rec in the data set.
			# If none exists, then the value is 0.
			row.num <- get.row.num(ds.with.index, rec)

#			print(row.num)

			# Is in this pair of sets
			if (row.num > 0) {
				
			    ###########################################################
			    # Train the data using svm() in this section
			    ###########################################################
			    model <- svm(Species~., data=ds, type="C-classification", cost=1, kernel="linear")

			    ###########################################################
			    # Find out how it did.
			    ###########################################################
			    predict <- as.data.frame(fitted(model))


			    # ******  Need to find rec in the prediction set ******
			    predict.label <- predict[row.num,1]

		        if (predict.label == rec.label) {
				    cnt[p] <- cnt[p] + 1
			    }
			    else {
				    cnt[q] <- cnt[q] + 1
			    }
            }
		}
	}

	# Get the maximum label count
	max.cnt <- max(cnt)

	# Now its index
	max.cnt.idx <- match(max.cnt, cnt)

	# Now get the label it's attached to
	class <- labels[max.cnt.idx]


	return(class)
}
