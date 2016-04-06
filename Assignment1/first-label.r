learner <- function(df) {

    # make sure we are actually handed a data frame
    if (!is.data.frame(df))
        stop("not a data frame")
	
    # find the number of columns
    n <- ncol(df)

    # another way of retrieving columns from a frame using the [[ ]] notation
    target.attribute <- df[[n]] 

    # tabulate the levels in the target attribute
    target.levels <- table(target.attribute) 

    # find out which level appears most often
    ix <- which.max(target.levels) 

    # convert the level descriptor into a string
    majority.label <- names(target.levels[ix]) 

    # build our model
    # our model is a **function** that given any object always returns the majority label in the
    # training data
    function(x) majority.label
}
