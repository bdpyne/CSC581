training.set <- read.csv("trainingset.csv");

#################################################
# The sign() from the base package returns
# -1 for 0.  sgn() returns 1. Otherwise, they
# are the same.
#################################################
sgn <- function(iVal) {
    if (iVal >= 0)
	    1
	else
	    -1
}


# n seems to be a random real between 0 and 1 non-inclusive
# randomly choosing 0.3
n     <- 0.3

w_vec <- cbind(0,0)
b     <- 0

# vector with the maximum length
r     <- max(training.set)

# rows in the training set
l     <- nrow(training.set)

# index for outer loop
j     <- 0

repeat
{
    for (i in 1:l)
	{
	    x_i_vec <- cbind(training.set[i]$x1,training.set[i]$x2)
		y_i     <- training.set[i]$y
	    if (sgn(w_vec %*% x_i_vec - b) != y_i) {
            w_vec <- w_vec + n * y_i * x_i_vec
			b     <- b - n * y_i * r * r
		}

	}

    j       <- j + 1
	x_j_vec <- cbind(training.set[j]$x1,training.set[j]$x2)
	y_j     <- training.set[j]$y

	if ((sgn(w_vec %*% x_j_vec - b) == y_j) ! (j > l) {
        break
	}
}
