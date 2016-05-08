run <- function() {

    library(randomForest)

    voting.df <- read.csv("dataset.csv")

    run.SetAnalysis(voting.df)

#    run.RandomForest(voting.df)
}

run.SetAnalysis  <- function(df) {

    target     <- df[, ncol(df)]
    target.int <- ifelse(target == "democrat", 1, 0)

    hist(target.int, border="blue", col="green", xlab="Parties", ylab="Members per Party", main=paste("Histogram of ", xname), labels=c("Republican","Democrat"), breaks=2)
}

run.RandomForest  <- function(df) {

    no.cols <- ncol(df)
    print(sprintf("Data set has %d columns", no.cols))

    last.attrib <- no.cols - 1

    rf <- randomForest(df[, 2:last.attrib], df[, no.cols], prox=TRUE)
    print(rf)
}

