library(data.tree)


#############################################################################
# Purpose: Entry point for the program.
#############################################################################
run <- function() {
    
    tennis <- read.csv("tennis.csv")

    print("Read tennis data.") 


    # This information stays consistent between the run of training and test.
    attrib.cnt <- ncol(tennis) - 1
    target.idx <- attrib.cnt + 1
    columns    <- colnames(tennis)
    attributes <- columns[1:attrib.cnt]
    target     <- columns[target.idx]


    # For dividing the tennis data set into training and testing.
#    set.seed(123)
    training.size <- floor(nrow(tennis) * 0.75)
    training.ind  <- sample(seq_len(nrow(tennis)), size=training.size)
    

    # Grab a training set from tennis. 
    training <- tennis[training.ind, ]

    # Write the training set out for documentation purposes.
    write.csv(training, "training.csv", row.names=FALSE)

    # Get the decision tree from the training set.
    tree     <- run_ID3(training, target, attributes)

    # Now show the tree.
    print("*********** TRAINING **************")
    print(tree)
    print("*********** TRAINING **************")

    # Now get the testing set from what remains after training.
    testing  <- tennis[-training.ind, ]


    # Get the decision tree from the testing set.
    tree     <- run_ID3(testing, target, attributes)

    # Now show the tree.
    print("*********** TESTING **************")
    print(tree)
    print("*********** TESTING **************")


    # Get the decision tree from the testing set.
    tree     <- run_ID3(tennis, target, attributes)

    # Now show the tree.
    print("*********** WHOLE SET **************")
    print(tree)
    print("*********** WHOLE SET **************")
}


#############################################################################
# Purpose: Using the inputs, it develops a decision tree with the leaf
#          nodes being the target labels.
#############################################################################
run_ID3 <- function(data, target, attributes) {


    # Check to see if unique values exist in the dependent column
    # If unique values do not exist, return the tree with the root node
    # containing the non-unique value

    target.vals   <- data[,target]
    target.unique <- unique(target.vals)


    if (length(target.unique) == 1) {
       Root <- Node$new(target.vals[1])
       return(Root)
    } 


    # Check for attributes other than the target

    if (length(attributes) == 0) {
        Root <- Node$new( labelWitMaxOccurrences(target.vals) )
        return(Root)
    }


    # Find the average entropy for each attribute and print the results
    reducer <- getEntropyReducer(data)

    Root <- Node$new(reducer)

    # Get the values in the "reducer" column
    reducer.vals <- data[,reducer]


    for (i in 1:length(reducer.vals)) {

        val    <- reducer.vals[i]

        branch <- Root$AddChild(val)

        # Get a subset of data where values in the column = val
        subs   <- data[reducer.vals == val,]

        if (nrow(subs) == 0) {
            label <- labelWithMaxOccurrences(target.vals)
            branch$AddChild(label)
        }
        else {
            attributes       <- attributes[attributes != reducer]
            returned.tree    <- run_ID3(subs, target, attributes)
            branch$AddChildNode(returned.tree)
        }
    }


    return(Root)
}


#############################################################################
# Purpose: Returns the attribute with the lowest entropy based on the data
#          input.
#############################################################################
getEntropyReducer <- function (data) {
  
    reducer     <- ""
    tot         <- 0
    avg         <- 0
    avgs        <- data.frame() 

#    if (nrow(data) == 0 | ncol(data) == 0)
#        stop("getEntropyReducer: Invalid input parameter 'data'")

    no.cols <- ncol(data) - 1

    for (i in 1:no.cols) {
        
        # Get unique labels
        labels    <- unique(data[,i])
        no.labels <- length(labels)

        # Now get the subset of data with those labels
        for (j in 1:length(labels)) {
            label.data <- data[data[,i] == labels[j],]
            entropy    <- calculateEntropy(label.data)
            
            # When entropy is 0, a NaN is returned. Need it to be a 0.
            if (is.nan(entropy))
                entropy <- 0

	    
            tot <- tot + entropy
        }

        avg <- tot / no.labels 

        # Add to DF the column name and avg entropy
        avgs[i, 1] <- colnames(data)[i]
        avgs[i, 2] <- avg

        # Important to reset between runs
        tot        <- 0
    } 

    # Column names aren't strictly necessary but they help
    colnames(avgs) <- c("Name","Average")

    # Get the attribute name
    reducer <- avgs[avgs[,2] == min(avgs[,2]),]$Name

    # In the event of a tie, just pick the first.
    if (length(reducer) > 1)
        reducer <- reducer[1] 

    return(reducer) 
}

#############################################################################
# Purpose: Calculates entropy. 
#############################################################################
calculateEntropy <- function(df) {

    tot <- nrow(df)
    n   <- nrow(df[df$PlayTennis == "No",]) / tot
    p   <- nrow(df[df$PlayTennis == "Yes",]) / tot

    entropy <- - (p * log(p)) - (n * log(n))
    
    return(entropy)
}



#############################################################################
# Purpose: Returns the label with the highest occurrences from the target
#          attribute.
#############################################################################
labelWitMaxOccurrences <- function(target) {
    return(names(which.max(table(target))))
}

