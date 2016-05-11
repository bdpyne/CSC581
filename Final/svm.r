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
    linear.tmp.err    <- run.Linear(df.sample)
    linear.class.err  <- linear.tmp.err[1]

    print.Section("RADIAL")
    radial.tmp.err    <- run.Radial(df.sample)
    radial.class.err  <- radial.tmp.err[1]

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
            linear.err <- run.Linear(local.df)
            train.err  <- radial.err[2]

            print(sprintf("The training error for the test set %d is %f", i, train.err))

            if (train.err < best.err) {
                best.idx <- i
                best.err <- train.err
            }
        }
    } 
    else {
        print.Section(sprintf("Radial wins with %f", radial.class.err))

        ########################################################################
        # Now do the cross-validation
        ########################################################################

        best.idx <- 0
        best.err <- 1000.0

        print.Section("TRAINING SET")
        run.Radial(df.train)

        for (i in 1:length(df.test.list)) {
            print.Section(sprintf("TEST SET %d", i))
            local.df   <- df.test.list[[i]]
            radial.err <- run.Radial(local.df)
            train.err  <- radial.err[2]

            print(sprintf("The training error for the test set %d is %f", i, train.err))

            if (train.err < best.err) {
                best.idx <- i
                best.err <- train.err
            }
        }

        print.Section("BOOTSTRAP")
        err <- bootstrap(df.test.list[[best.idx]])

        print(sprintf("The upper bound is %d and the lower bound is %d", err[195], err[5]))
    }
}

################################################################################
#
################################################################################
run.Radial <- function(df) {

    err <- vector(,2)

    print(sprintf("Instances: %d", nrow(df))) 

    cost <- 100
    print(sprintf("Cost: %d", cost))

    gam  <- 0.3
    print(sprintf("Gamma: %f", gam))

    model   <- svm(party ~ ., data=df[, -1], type="C-classification", cost=cost, kernel="radial", gamma=gam)
#    print(model)

    class.err <- calc.ClassError(df$party, model)
    print(sprintf("Class Error: %f", class.err))

    train.err <- calc.TrainError(df$party, model)
    print(sprintf("Train Error: %f", train.err))

    err[1] <- class.err
    err[2] <- train.err

    return(err)
}


################################################################################
#
################################################################################
run.Linear <- function(df) {

    err <- vector(,2)

    print(sprintf("Instances: %d", nrow(df))) 

    cost <- 100
    print(sprintf("Cost: %d", cost))

    model     <- svm(party ~ ., data=df[,-1], type="C-classification", cost=cost, kernel="linear")
    print(model)

    class.err <- calc.ClassError(df$party, model)
    print(sprintf("Class Error: %f", class.err))

    train.err <- calc.TrainError(df$party, model)
    print(sprintf("Train Error: %f", train.err))

    err[1] <- class.err
    err[2] <- train.err

    return(err)
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

    return(tot.err / tot) 
}


################################################################################
#
################################################################################
calc.TrainError <- function(dep, model) {

    
    predict <- fitted(model)
    cm      <- table(dep, predict)
    print(cm)
    tot.err <- 0

    for (i in 1:2) {
        for (j in 1:2) {
            if (i != j)
                tot.err <- tot.err + cm[i,j]
        }
    }

    if (tot < 1)
        stop("Cannot have a zero total for instances.")

    return(tot.err / length(predict) * 100)
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



################################################################################
#
################################################################################
bootstrap  <- function(df) {

    # Collect the err's in here
    err    <- vector(,200)

    for (i in 1:200) {
        # Take a 10% sample of the input dataset
        df.sample <- df[sample(nrow(df), 0.1 * nrow(df)), ,]

        # Get back the errors - classification and training
        temp <- run.Radial(df.sample)

        # Add the training error to the data frame of them
        err[i] <- temp[2]
    }

    order(err)

    return(err)
}
