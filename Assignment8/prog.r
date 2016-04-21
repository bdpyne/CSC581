library(data.tree)


#############################################################################
# Purpose: Entry point for the program.
#############################################################################
run <- function() {
    
    tennis <- read.csv("tennis.csv")

    print("Read tennis data.") 

    attrib.cnt <- ncol(tennis) - 1
    target.idx <- attrib.cnt + 1
    columns    <- colnames(tennis)
    attributes <- columns[1:attrib.cnt]
    target     <- columns[target.idx]
    tree       <- run_ID3(tennis, target, attributes)

    print(tree)
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


#############################################################################
# Purpose: Shows how my data set can be expressed using the data.tree 
#          library.
#
# THIS IS NOT IMPORTANT FOR THE ID3 ALGORITHM.
#############################################################################
run.tree.example <- function() {

    # Root node
    outlook    <- Node$new("Outlook")

    # Sunny branch
    sunny      <- outlook$AddChild("Sunny")

    sunny.hot  <- sunny$AddChild("Hot")
    sunny.mild <- sunny$AddChild("Mild")
    sunny.cool <- sunny$AddChild("Cool")

    sunny.hot.high       <- sunny.hot$AddChild("High")
    sunny.hot.high.true  <- sunny.hot.high$AddChild("True")
    sunny.hot.high.false <- sunny.hot.high$AddChild("False")
    
    sunny.mild.high         <- sunny.mild$AddChild("High")
    sunny.mild.high.false   <- sunny.mild.high$AddChild("False")
    sunny.mild.normal       <- sunny.mild$AddChild("Normal")
    sunny.mild.normal.true  <- sunny.mild.normal$AddChild("False")

    sunny.cool.normal       <- sunny.cool$AddChild("Normal")
    sunny.cool.normal.false <- sunny.cool.normal$AddChild("False")
    
    # Overcast branch
    overcast      <- outlook$AddChild("Overcast")

    overcast.hot             <- overcast$AddChild("Hot")
    overcast.hot.high        <- overcast.hot$AddChild("High")
    overcast.hot.high.false  <- overcast.hot.high$AddChild("False")
    overcast.hot.normal      <- overcast.hot$AddChild("Normal")
    overcast.hot.normal.true <- overcast.hot.normal$AddChild("True")

    overcast.mild           <- overcast$AddChild("Mild")
    overcast.mild.high      <- overcast.mild$AddChild("High")
    overcast.mild.high.true <- overcast.mild.high$AddChild("True")

    overcast.cool             <- overcast$AddChild("Cool")
    overcast.cool.normal      <- overcast.cool$AddChild("Normal")
    overcast.cool.normal.true <- overcast.cool.normal$AddChild("True")
 

    # Rainy branch
    rainy      <- outlook$AddChild("Rainy")  

    rainy.mild            <- rainy$AddChild("Mild")
    rainy.mild.high       <- rainy.mild$AddChild("High")
    rainy.mild.high.false <- rainy.mild.high$AddChild("False")
    rainy.mild.high.truee <- rainy.mild.high$AddChild("True")
    
    rainy.mild.normal       <- rainy.mild$AddChild("Normal")
    rainy.mild.normal.false <- rainy.mild.normal$AddChild("False")
    
    rainy.cool              <- rainy$AddChild("Cool")
    rainy.cool.normal       <- rainy.cool$AddChild("Normal")
    rainy.cool.normal.false <- rainy.cool.normal$AddChild("False")
    rainy.cool.normal.true  <- rainy.cool.normal$AddChild("True")

    print(outlook)
}
