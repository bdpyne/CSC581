###############################################################################
#
###############################################################################
run <- function(pnt) {

	data(iris)
	df    <- as.data.frame(iris)
	data.size <- nrow(df)
	print(sprintf("Data set size is: %d", data.size))

    if ((pnt > data.size) | (pnt < 1)) {
		stop(sprintf("Sorry, needs to stay within 1 and %d.", data.size))
	}

	rec   <- as.data.frame(df[pnt,])

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
#	cnt <- data.frame()
#	cnt[1,1] <- 0
#	cnt[1,2] <- 0
#	cnt[1,3] <- 0
#	colnames(cnt) <- c(labels[1],labels[2],labels[3])

	setosa.cnt     <- 0
	versicolor.cnt <- 0
	virginica.cnt  <- 0

	# Get the upper index
	p.max <- labels.cnt - 1

	for (p in 1:p.max) {

		p.label    <- labels[p]

		# Get the lower index
		q.min <- p + 1

		for (q in q.min:labels.cnt) {

			# Get the label values 
			q.label <- labels[q]

			# Get a new frame that is the union of # the rows matching 
			# p.label and q.label
			ds <- df[ df[,ncol(df)] == p.label | df[,ncol(df)] == q.label ,]

			# Create a duplicate of the ds data frame and add a row number
			ds.with.index    <- data.frame(ds)
            ds.with.index$Id <- seq.int(nrow(ds.with.index))


            # Get row number of the rec in the data set.
			# If none exists, then the value is 0.
			row.num <- get.row.num(ds.with.index, rec)


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


#		        if (predict.label == rec.label) {
#					tot <- cnt[1,p.label]
#					tot <- tot + 1
#				    cnt[1,p.label] <- tot 
#					print(sprintf("p count is %d", cnt[1,p.label]))
#			    }
#			    else {
#					tot <- cnt[1,q.label]
#					tot <- tot + 1
#				    cnt[1,q.label] <- tot 
#					print(sprintf("q count is %d", cnt[1,q.label]))
#			    }

				if (predict.label == rec.label) {
					if (predict.label == "setosa") {
						setosa.cnt <- setosa.cnt + 1
					}
					else if (predict.label == "versicolor") {
						versicolor.cnt <- versicolor.cnt + 1
                    }
					else {
						virginica.cnt <- virginica.cnt + 1
					}
				}
            }
		}
	}

	
	print(sprintf("%s: %d, %s: %d, %s: %d", labels[1], setosa.cnt, labels[2], versicolor.cnt, labels[3], virginica.cnt))

#	# Get the maximum label count
#	max.cnt <- max(cnt)

	# Now its index
#	max.cnt.idx <- match(max.cnt, cnt)

	max.cnt.idx <- get.greatest(setosa.cnt, versicolor.cnt, virginica.cnt)

	# Now get the label it's attached to
	class <- labels[max.cnt.idx]

	print(sprintf("Label found is: %s", class))

	return(class)
}

get.greatest <- function(setosa, versi, virg)  {
	gr <- setosa 
	idx <- 1

	if (versi > gr) {
		gr <- versi
		idx <- 2
	}

	if (virg > gr) {
		gr <- versi
		idx <- 3
	}

	return(idx)
}

run.builtin <- function() {
    data( iris )
	model <- svm( iris$Species~., iris )
	res <- predict( model, newdata=iris )
	cm      <- table(iris$Species, res)
	print(cm)
	err     <- (cm[1, 2] + cm[2, 1] / length(res) * 100)
	print(err)
}
