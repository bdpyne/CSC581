library(data.tree)


#############################################################################
# Purpose
#############################################################################
run <- function() {
    
    tennis <- read.csv("tennis.csv")

    print("Read tennis data.") 

    tree <- run_ID3(tennis, tennis$PlayTennis, tennis[,1:ncol(tennis)-1])
    print(tree)
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
getEntropyReducer <- function (data) {
  
    reducer    <- ""  
    lowest.ent <- 1

    if (nrow(data) == 0 | ncol(data) == 0)
        stop("getEntropyReducer: Invalid input parameter 'data'")

    print(sprintf("rows: %d, cols: %d", nrow(data), ncol(data)))

    no.cols <- ncol(data) - 1

    print(sprintf("no.cols: %d", no.cols))

    for (i in 1:no.cols) {
        
        print(sprintf("Pass %d", i))

        # Get unique labels
        labels <- unique(data[,i])

        print(labels)

        # Now get the subset of data with those labels
        for (j in 1:length(labels)) {
            print(sprintf("Label: %s", labels[j]))
            label.data <- data[data[,i] == labels[j],]
            print(label.data)
            entropy <- calculateEntropy(label.data)
            print(sprintf("Label %s entropy: %f", labels[j], entropy))
        }
    } 

    return(reducer) 
}


#############################################################################
# Purpose
#############################################################################
run_ID3 <- function(data, target, attribs) {

    if (nrow(data) == 0)
        stop("Empty data set passed to the ID3 algorithm.")


    # Let T be a new tree
    T <- Node$new("Outlook")


    # Check to see if unique values exist in the dependent column
    # If unique values do not exist, return the tree with the root node
    # containing the non-unique value
    target.unique <- unique(target)

    if (length(target.unique) == 1) {
       T <- Node$new(data$PlayTennis[1])
       return(T)
    } 


    # Check for attributes other than the target
    if (ncol(attribs) == 0) {
        T <- Node$new( names(which.max(table(data$PlayTennis))) )
        return(T)
    }

    return(T)
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
