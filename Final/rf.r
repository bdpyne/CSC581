################################################################################
# Start the program with this call.
################################################################################

run <- function() {

    library(randomForest)

    voting.df <- read.csv("dataset.csv")

#    run.CrudeAnalysis(voting.df)
#    run.PredictPredictors(voting.df)
#    run.Explore.RandomForest(voting.df)
    run.CV.RandomForest(voting.df)
}


################################################################################
# Cross-validate the RandomForest algorithm. 
################################################################################
run.CV.RandomForest <- function(df) {

    print("Running cross-validation on Random Forest")

    df.random <- df[sample(nrow(df), nrow(df), replace=TRUE), ,]
    print(sprintf("Random df has %d rows", nrow(df.random)))

    ###########################################################################
    # Develop training set, run rf, and report the ce
    ###########################################################################
    train.size  <- ceiling(nrow(df.random) * 0.5)

    df.train    <- df.random[1:train.size, ,]
    print(sprintf("Training df has %d rows", nrow(df.train)))

    no.cols <- ncol(df.train)
    print(sprintf("Data set has %d columns", no.cols))

    last.attrib <- no.cols - 1

    rf <- randomForest(df.train[, 2:last.attrib], df.train[, no.cols], prox=TRUE)
    ce <- calc.ClassError.RandomForest(rf)
    print(sprintf("The classification error for the training set is %f", ce))

    ###########################################################################
    # Develop test sets, run rf, and report the ce
    ###########################################################################
    test.idx.start <- train.size + 1
    df.test <- df.random[test.idx.start:nrow(df.random), ,]
    print(sprintf("Test df has %d rows", nrow(df.test)))

    # Split the remaining list into 2 lists with a random selection
    df.test.list <- split(df.test, sample(1:2, nrow(df.test), replace=T))

    for (i in 1:length(df.test.list)) {
        local.df <- df.test.list[[i]]
        rf <- randomForest(local.df[, 2:last.attrib], local.df[, no.cols], prox=TRUE)
        ce <- calc.ClassError.RandomForest(rf)
        print(sprintf("The classification error for the test set %d is %f", i, ce))
    }
}


################################################################################
# Run the RandomForest algorithm on the dataset and print the results.
################################################################################
run.Explore.RandomForest  <- function(df) {

    df.sample <- df[sample(nrow(df), 0.1 * nrow(df)), ,]
    print(sprintf("Sample has %d rows", nrow(df.sample)))

    no.cols <- ncol(df.sample)
    print(sprintf("Data set has %d columns", no.cols))

    last.attrib <- no.cols - 1

    rf <- randomForest(df.sample[, 2:last.attrib], df.sample[, no.cols], prox=TRUE)

    ce <- calc.ClassError.RandomForest(rf)

    print(sprintf("The classification error is %f", ce))
}



################################################################################
# Cycle through the attributes, get voting totals, and list the totals in order
# to predict which attributes are the most important.
################################################################################
run.PredictPredictors <- function(df) {

    tot.dem   <- nrow(df[df$party == "democrat", ])
    tot.rep   <- nrow(df[df$party == "republican", ])
    names     <- colnames(df)

    for (i in 2:ncol(df) - 1) {
        sum.dem.y <- nrow(df[df[,i] == 'y' & df$party == 'democrat',]) / tot.dem * 100
        sum.dem.n <- nrow(df[df[,i] == 'n' & df$party == 'democrat',]) / tot.dem * 100

        sum.rep.y <- nrow(df[df[,i] == 'y' & df$party == 'republican',]) / tot.rep * 100
        sum.rep.n <- nrow(df[df[,i] == 'n' & df$party == 'republican',]) / tot.rep * 100

        new.df <- data.frame(party=c("Democrat","Republican"), pro=c(sum.dem.y,sum.rep.y),anti=c(sum.dem.n,sum.rep.n))

        print(sprintf("Attribute: %s", names[i]))
        print(new.df)
    }
}


################################################################################
# Show basic summary stats and a histogram of the party breakdown within the
# House.
################################################################################
run.CrudeAnalysis  <- function(df) {

    # Summary stats
    print(summary(df))

    target     <- df[, ncol(df)]
    target.int <- ifelse(target == "democrat", 1, 0)

    hist(target.int, border="blue", col="green", xlab="Parties", ylab="Members per Party", main=paste("Histogram of ", xname), labels=c("Republican","Democrat"), breaks=2)
}


################################################################################
# Get the classification error as a decimal from a confusion matrix from a
# Random Forest.
################################################################################
calc.ClassError.RandomForest <- function(rf) {

    cm      <- rf["confusion"]

    print("Confusion Matrix")
    print(cm$confusion)

    tot     <- 0
    tot.err <- 0


    for (i in 1:2) {
        for (j in 1:2) {
            tot <- tot + cm$confusion[i,j]

            if (i != j) 
                tot.err <- tot.err + cm$confusion[i,j]
        }
    }

#    print(sprintf("Total error: %d", tot.err))
#    print(sprintf("Total      : %d", tot))

    if (tot < 1)
        stop("Cannot have a zero total for instances.") 

    tot.err / tot
}
