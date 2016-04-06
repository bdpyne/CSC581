# this program implements the primal perceptron training algorithm

#### main driver routine ###
# the learning rate should be a value in the interval [0,1]
run <- function(learningrate) {
	ds <- read.csv("ds-problem-4-1.csv")
	plot.setup(ds)
	train(ds,learningrate)
}

# df - training data dataframe, n - learning rate
train <- function(df, n) {	

	l <- length(df$x1)	# records in dataframe
	w <- c(5.0,5.0)
	b <- 0
	r <- max(sqrt(df$x1^2 + df$x2^2))
	
	repeat {
		mistake <- FALSE
		for (i in 1:l) {
			xi <- c(df$x1[i],df$x2[i])
			yi <- df$y[i]
			if (sign(sum(w*xi) - b) != yi) {
				mistake <- TRUE
				w <- w + n*yi*xi
				b <- b - n*yi*r^2
			}
		}
		if (!mistake) 
			break
	}

	surface(list("w"=w,"b"=b)) 	
	slope = -(w[1]/w[2])
	offset =  (b/w[2])
	list("slope"=slope,"offset"=offset)
}

# plot the decision surface
surface <- function(m) {
	w <- m$w
	b <- m$b
	
	slope = -(w[1]/w[2])
	offset =  (b/w[2])
	
	# assumes that the plot already has been set up
	abline(offset,slope,lty="solid",lwd=2,col="green")
}

# set up the plot and plot the data points in the training set
plot.setup <- function(df) {
	# uncomment the line below if you are running on a Mac
	quartz(width=8,height=8)

	# setup the plot
	plot(0:10,0:10,type="n",main="Perceptron Learning",xlab="x1",ylab="x2")
	
	# plot the classes: red = + and blue = -
	for (i in 1:length(df$x1))
		if (df$y[i] > 0 )
			points(df$x1[i],df$x2[i],col="red")
		else
			points(df$x1[i],df$x2[i],col="blue")
}

