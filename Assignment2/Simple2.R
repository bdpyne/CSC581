#################################################
# The sign() from the base package returns
# -1 for 0.  sgn() returns 1. Otherwise, they
# are the same.
#################################################
sgn <- function(iVal) {
    if (iVal >= 0)
	    1
	else
	    -1
}

learner <- function(training.df) {

    # make sure we are actually handed a data frame
    if (!is.data.frame(training.df))
        stop("not a data frame")

    # initialize some basic parameters
    no.of.attributes <- ncol(training.df)
    target.attribute <- training.df[[no.of.attributes]]
    target.levels <- table(target.attribute)

    # get the majority label
    majority.label.ix <- which.max(target.levels)
    majority.label <- names(target.levels[majority.label.ix])

    #     construct the model - the model accepts a description of an object
    # and then returns a prediction based on the majority label for that object.
    model.function <- "function( "

    # construct the model argument list
    for (i in 1:(no.of.attributes-1)) {
        if (i != 1)
            model.function <- paste(model.function,", ",sep="")
        model.function <- paste(model.function,names(training.df)[i],sep="")
    }

	model.function <- paste(model.function, ", c_vec, d_vec", sep="")
    model.function <- paste(model.function,")")

    # construct the model body
    model.function <- paste(model.function, " {\n", sep="")
	model.function <- paste(model.function, "x_vec <- cbind(", sep="") 
	for (i in 1:(no.of.attributes-1)) {
        if (i != 1)
            model.function <- paste(model.function,", ",sep="")
        model.function <- paste(model.function,names(training.df)[i],sep="")
	}
	model.function <- paste(model.function, ")\n", sep="")
	model.function <- paste(model.function, "sgn((x_vec - c_vec) %*% d_vec)\n", sep="")
    model.function <- paste(model.function, "}", sep="")

    # show the model
    cat("Constructed model: ", model.function, "\n")

    # construct an R function expression and return it
    # as our model function
    eval(parse(text=model.function))
}

    # Separate the data into positive and negative tables.
    pos_tab <- df[df$y == 1, 1:2]
    neg_tab <- df[df$y == -1, 1:2]

    #     Store the columnar means for each table.
    c_pos_vec <- colMeans(pos_tab)
    c_neg_vec <- colMeans(neg_tab)

    # Run a new vector from the postive means
    # to the negative means.
    d_vec <- c_pos_vec - c_neg_vec

    # Now we compute the mean between the postive and negative means.
    c_vec <- (c_pos_vec + c_neg_vec)/2

    #w_vec <- d_vec
    #b <- d_vec %*% c_vec


df <- read.csv("trainingset.csv")
