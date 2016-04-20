library(data.tree)


#############################################################################
# Purpose
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
# Purpose
#############################################################################
run_ID3 <- function(data, target, attributes) {


    # Check to see if unique values exist in the dependent column
    # If unique values do not exist, return the tree with the root node
    # containing the non-unique value

    target.vals   <- data[,target]
    target.unique <- unique(target.vals)


    if (length(target.unique) == 1) {
       T <- Node$new(target.vals[1])
#print("unique = 1")
       return(T)
    } 


    # Check for attributes other than the target

    if (length(attributes) == 0) {
        T <- Node$new( labelWitMaxOccurrences(target.vals) )
print("attribs = 0")
        return(T)
    }


    # Find the average entropy for each attribute and print the results
    reducer <- getEntropyReducer(data)

    print(sprintf("Reducer is %s", reducer))

    T <- Node$new(reducer)

    # Get the unique values in the "reducer" column
    reducer.unique.vals <- unique(data[,reducer]) 

    for (i in 1:length(reducer.unique.vals)) {

        val    <- reducer.unique.vals[i]

        branch <- T$AddChild(val)

        # Get a subset of data where values in the column = val
        subs   <- data[data[,reducer] == val,]

        if (nrow(subs) == 0) {
            label <- labelWithMaxOccurrences(target.vals)
            branch$AddChild(label)
        }
        else {
            col        <- which(colnames(data) == reducer)
            attributes <- attributes[attributes != reducer]
            temp       <- run_ID3(subs, target, attributes)
            branch$AddChild( temp$levelName ) 

            # This bit of magic is here because a root
            # Node cannot be added to a child Node.
            # The lack of children will be taken as an
            # indicator that it is a root.
#            if (is.null(temp$children) == TRUE) {
#                T$AddChild(temp$levelName)
#            }
#            else {
#                T$AddChild( temp$levelName ) 
#            }
        }
    }


    return(T)
}


#############################################################################
# Purpose
#############################################################################
getEntropyReducer <- function (data) {
  
    reducer     <- ""
    tot         <- 0
    avg         <- 0
    avgs        <- data.frame() 

    if (nrow(data) == 0 | ncol(data) == 0)
        stop("getEntropyReducer: Invalid input parameter 'data'")

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

#            print(sprintf("Label %s entropy: %f", labels[j], entropy))

	    
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
# Purpose
#############################################################################
calculateEntropy <- function(df) {

    tot <- nrow(df)
    n   <- nrow(df[df$PlayTennis == "No",]) / tot
    p   <- nrow(df[df$PlayTennis == "Yes",]) / tot

    entropy <- - (p * log(p)) - (n * log(n))
    
    return(entropy)
}



#############################################################################
# Purpose
#############################################################################
labelWitMaxOccurrences <- function(target) {
    return(names(which.max(table(target))))
}


#############################################################################
# Purpose
#     Shows how my data set can be expressed using the data.tree library.
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
