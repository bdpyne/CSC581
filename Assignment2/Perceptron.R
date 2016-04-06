learner <- function(trainer.df) {
    # make sure we are actually handed a data frame
    if (!is.data.frame(training.df))
        stop("not a data frame")

    # initialize some basic parameters
    no.of.attributes <- ncol(training.df)
    target.attribute <- training.df[[no.of.attributes]]
    target.levels    <- table(target.attribute)

    # construct the model
	# This is the boilerplate part.
	# The key is the algorithm for determining the label.
	# See PAlgo.R for most of the algorithm.
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
