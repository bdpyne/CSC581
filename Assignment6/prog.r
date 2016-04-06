###############################################################################
#
###############################################################################
run <- function() {

	data(iris)
	iris.df <- as.data.frame(iris)
	class <- classify(iris.df)
	class
}

###############################################################################
#
###############################################################################
classify <- function(df) {

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

	for (p in 1:(labels.cnt - 1)) {
		for (q in (p + 1):labels.cnt) {
			# Get the label values 
			l1 <- labels[p]
			l2 <- labels[q]

			# Get a new frame that is the union of # the rows matching 
			# l1 and l2
			ds <- df[df[,ncol] == l1 | df[,ncol],]

			###########################################################
			# Train the data using svm() in this section
			###########################################################

			

			###########################################################
			###########################################################


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
