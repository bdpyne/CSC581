# this program implements the simple learning algorithm

#### main driver routine ###
run <- function() {
	ds <- read.csv("ds-problem-4-1.csv")
	plot.setup(ds)
	learn(ds)
}

# df - training data dataframe
learn <- function(df) {	

	# extract training sets for each of the classes
	class1 <- subset(df,df$y > 0)
	class2 <- subset(df,df$y < 0)

	# drop the instance labels
	class1 <- subset(class1,select=-y)
	class2 <- subset(class2,select=-y)
	
	# compute the class means
	mean1 <- c(mean(class1$x1),mean(class1$x2))
	mean2 <- c(mean(class2$x1),mean(class2$x2))

	# plot the class means
	points(mean1[1],mean1[2],col="black")
	points(mean2[1],mean2[2],col="black")
	
	# decision surface w*x=b, from the book
	w <- mean1 - mean2
	b <- .5 * ((mean1+mean2) %*% w)

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
	plot(0:10,0:10,type="n",main="Simple Learning",xlab="x1",ylab="x2")
	
	# plot the classes: red = + and blue = -
	for (i in 1:length(df$x1))
		if (df$y[i] > 0 )
			points(df$x1[i],df$x2[i],col="red")
		else
			points(df$x1[i],df$x2[i],col="blue")
}

