run <- function() {

    library(e1071)
    
    voting.df <- read.csv("dataset.csv")

    run.Kernels(voting.df)
}


################################################################################
#
################################################################################
run.Kernels <- function(df) {

    print.Section("RUNNING KERNELS - linear and radial")

    # Take a 10% sample to do a basic analysis in order to compare kernel
    # performance.
    df.sample <- df[sample(nrow(df), 0.1 * nrow(df)), ,]
    print(sprintf("Sample has %d rows", nrow(df.sample)))

    # Randomize the data
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

    ###########################################################################
    # Develop test sets, run rf, and report the ce
    ###########################################################################
    test.idx.start <- train.size + 1
    test.idx.end   <- nrow(df.random)

    print(sprintf("test idx start: %d", test.idx.start))
    print(sprintf("test idx   end: %d", test.idx.end))

    df.test <- df.random[test.idx.start:test.idx.end, ,]

    # Get the records from df.random that are not in df.train
    df.test.size <- nrow(df.test)

    print(sprintf("Test df has %d rows", df.test.size))

    # Split the remaining list into 2 lists with a random selection
    df.test.list <- split(df.test, sample(1:2, df.test.size, replace=TRUE))

    print.Section("LINEAR")
    linear.class.err <- run.Linear(df.sample)

    print.Section("RADIAL")
    radial.class.err <- run.Radial(df.sample)

    if (linear.class.err < radial.class.err) {
        print.Section(sprintf("Linear wins with %f", linear.class.err))

        ########################################################################
        # Now do the cross-validation
        ########################################################################

        print.Section("TRAINING SET")
        run.Linear(df.train)

        for (i in 1:length(df.test.list)) {
            print.Section(sprintf("TEST SET %d", i))
            local.df  <- df.test.list[[i]]
            class.err <- run.Linear(local.df)
            print(sprintf("The classification error for the test set %d is %f", i, class.err))
        }
    } 
    else {
        print.Section(sprintf("Radial wins with %f", radial.class.err))

        ########################################################################
        # Now do the cross-validation
        ########################################################################

        print.Section("TRAINING SET")
        run.Radial(df.train)

        for (i in 1:length(df.test.list)) {
            print.Section(sprintf("TEST SET %d", i))
            local.df  <- df.test.list[[i]]
            class.err <- run.Radial(local.df)
            print(sprintf("The classification error for the test set %d is %f", i, class.err))
        }
    }
}

################################################################################
#
################################################################################
run.Radial <- function(df) {

    print(sprintf("Instances: %d", nrow(df))) 

    cost <- 100
    print(sprintf("Cost: %d", cost))

    gam  <- 0.3
    print(sprintf("Gamma: %f", gam))

    model   <- svm(party ~ ., data=df[, -1], type="C-classification", cost=cost, kernel="radial", gamma=gam)
    print(model)

    class.err <- calc.ClassError(df$party, model)
    print(sprintf("Class Error: %f", class.err))
    class.err
}


################################################################################
#
################################################################################
run.Linear <- function(df) {

    print(sprintf("Instances: %d", nrow(df))) 

    cost <- 100
    print(sprintf("Cost: %d", cost))

    model     <- svm(party ~ ., data=df[,-1], type="C-classification", cost=cost, kernel="linear")
    print(model)

    class.err <- calc.ClassError(df$party, model)
    print(sprintf("Class Error: %f", class.err))
    class.err
}


################################################################################
#
################################################################################
calc.ClassError <- function(dep, model) {

    
    predict <- fitted(model)
    cm      <- table(dep, predict)
    print(cm)
    tot     <- 0
    tot.err <- 0

    for (i in 1:2) {
        for (j in 1:2) {
            tot <- tot + cm[i,j]

            if (i != j)
                tot.err <- tot.err + cm[i,j]
        }
    }

    if (tot < 1)
        stop("Cannot have a zero total for instances.")

    tot.err / tot    
}


################################################################################
#
################################################################################
print.Section <- function(msg) {
    
    print("")
    print("*********************************************")
    print("")
    print(msg)
    print("")
    print("*********************************************")
    print("")
}
