run <- function() {

    # Read the original file from the download.
    df <- read.csv("original.csv")

    # Computer the median of total riders per day.
    med <- median(df$cnt)

    # Use the median to create a binary, dependent variable.
    df <- df[, -1]
    df <- within(df, ridersup <- ifelse(cnt > med, "UP", "NOTUP"))

    # Write the modified data set to a file.
    write.csv(df, "universe.csv")
}
