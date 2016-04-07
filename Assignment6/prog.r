###############################################################################
#
###############################################################################
run <- function() {

	data(iris)
	df    <- as.data.frame(iris)
	rec   <- as.data.frame(df[137,])

	class <- classify(df, rec)
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


classify <- function(df, rec) {

	# Get the point from the data frame
	rec.label <- rec$Species[1]

	print(sprintf("Label to find is: %s", rec.label))

	# Initialize just to let the interpreter know the data type
	class <- ""

	# Get the unique labels from the dependent column
	labels <- unique(df$Species)


	# How many labels?
	labels.cnt <- length(labels)

	# Allocate a 3 column data frame to hold the totals
	cnt <- data.frame()
	cnt[1,1] <- 0
	cnt[1,2] <- 0
	cnt[1,3] <- 0
	colnames(cnt) <- c(labels[1],labels[2],labels[3])

#	septosis.cnt   <- 0
#	versicolor.cnt <- 0
#	virginica.cnt  <- 0

	# Get the upper index
	p.max <- labels.cnt - 1

	for (p in 1:p.max) {

		p.label    <- labels[p]

		print(sprintf("Debug 1"))

		# Get the lower index
		q.min <- p + 1

		print(sprintf("Debug 2"))

		for (q in q.min:labels.cnt) {

			# Get the label values 
			q.label <- labels[q]

			# Get a new frame that is the union of # the rows matching 
			# p.label and q.label
			ds <- df[ df[,ncol(df)] == p.label | df[,ncol(df)] == q.label ,]

			# Create a duplicate of the ds data frame and add a row number
			ds.with.index    <- data.frame(ds)
            ds.with.index$Id <- seq.int(nrow(ds.with.index))

			print("Debug 3")

            # Get row number of the rec in the data set.
			# If none exists, then the value is 0.
			row.num <- get.row.num(ds.with.index, rec)

			print(sprintf("Row num is: %d", row.num))

			print("Debug 4")

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

				print("Debug 5")

		        if (predict.label == rec.label) {
					tot <- cnt[1,p.label]
					tot <- tot + 1
				    cnt[1,p.label] <- tot 
					print(sprintf("p count is %d", cnt[1,p.label]))
			    }
			    else {
					tot <- cnt[1,q.label]
					tot <- tot + 1
				    cnt[1,q.label] <- tot 
					print(sprintf("q count is %d", cnt[1,q.label]))
			    }
				print("Debug 6")
            }
		}
	}

	
	print(sprintf("%s: %d, %s: %d, %s: %d", labels[1], cnt[1,1], labels[2], cnt[1,2], labels[3], cnt[1,3]))

	# Get the maximum label count
	max.cnt <- max(cnt)

	# Now its index
	max.cnt.idx <- match(max.cnt, cnt)

	# Now get the label it's attached to
	class <- labels[max.cnt.idx]

	print(sprintf("Label found is: %s", class))

	return(class)
}
