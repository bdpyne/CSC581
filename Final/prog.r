################################################################################
# Start the program with this call.
################################################################################

run <- function() {

    library(randomForest)

    voting.df <- read.csv("dataset.csv")

#    run.CrudeAnalysis(voting.df)
#    run.PredictPredictors(voting.df)

    run.Explore.RandomForest(voting.df)
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
    print(rf)
}


################################################################################
# Get the classification error as a decimal from a confusion matrix from a
# Random Forest.
################################################################################
calc.ClassError.RandomTree <- function(rf) {

    cm      <- rf["confusion"]

    tot     <- 0
    tot.err <- 0


    for (i in 1:2) {
        for (j in 1:2) {
            tot <- tot + cm$confusion[i,j]

            if (i != j) 
                tot.err <- cm$confusion[i,j]
        }
    }


    if (tot < 1)
        stop("Cannot have a zero total for instances.") 


    tot.err / tot
}


################################################################################
# Cross-validate the RandomForest algorithm. 
################################################################################
run.CV.RandomForest <- function(df) {

    df.train <- df[sample(nrow(df), 0.50 * nrow(df)), ,]
    print(sprintf("Training has %d rows", nrow(df.train)))

    no.cols <- ncol(df.train)
    print(sprintf("Data set has %d columns", no.cols))

    last.attrib <- no.cols - 1

    rf <- randomForest(df.train[, 2:last.attrib], df.train[, no.cols], prox=TRUE)
    print(rf)

    rf
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


