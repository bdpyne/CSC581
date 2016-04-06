library(quadprog)

run <- function() {

	df  <- read.csv("ds-problem-4-1.csv")
	plot.setup(df)
	train(df)
}

train <- function(df) {

	num.rows  <- nrow(df)
	num.cols  <- ncol(df)
	r         <- max(sqrt(df$x1^2 + df$x2^2))
	q         <- 20
	b         <- (-1) * q
	step      <- .5

	# Construct X according to (6.32) using D.
    X  <- get.constraints(df)

	# The n x n identity matrix.
	I  <- diag(ncol(df))

	# Set a temp variable. The expression below seems to
	# not apply the - 1 before the loop.
	id_vec <- 0 * (1:ncol(df))



	##########################################
	while (b <= q) {

		# Construct c_vec according to (6.33) using b.
		c_vec <- sapply(df$y, function(x) { 1 + x * b })

		print(c_vec)

		# 	Enter the quadratic part
		w_vec <- my.solve.QP(I, id_vec, X, c_vec)

		##############################################################################
		# For some reason, having a single if with an OR'ed condition did not work.
		##############################################################################
		if (exists("w_vec") & !exists("w_ast_vec")) {
			w_ast_vec <- w_vec
			b_ast     <- b
    	}
		else if (exists("w_vec") & (sum(w_vec$solution^2) < sum(w_ast_vec$solution^2))) {
			w_ast_vec <- w_vec
			b_ast     <- b
		}

		b <- b + step
	}
	##########################################


	if (!exists("w_ast_vec"))
		stop("constraints not satisfiable")

	if (sum(w_ast_vec$solution^2) > q/r)
		stop("bounding assumption of |w_vec| violated")


	plot.decision.surface(w_ast_vec$solution, b_ast)
}




# my own version of the QP wrapper...needed to get rid of the 'stop' statements
# Dmat -- matrix appearing in the quadratic function to be minimized (Q in the book)
# dvec -- vector appearing in the quadratic function to be minimized (q in the book)
# Amat -- matrix defining the constraints under which we want to minimize the quadratic function (X in the book)
# bvec -- vector holding the values of constraint bounds (c in the book)

my.solve.QP <- function (Dmat, dvec, Amat, bvec, meq = 0, factorized = FALSE) 
{
    n <- nrow(Dmat)
    q <- ncol(Amat)

    if (missing(bvec)) 
        bvec <- rep(0, q)

    if (n != ncol(Dmat)) 
        stop("Dmat is not symmetric!")

    if (n != length(dvec))
        stop("Dmat and dvec are incompatible!")

    if (n != nrow(Amat))
        stop("Amat and dvec are incompatible!")

    if (q != length(bvec)) 
        stop("Amat and bvec are incompatible!")

    if ((meq > q) || (meq < 0)) 
        stop("Value of meq is invalid!")

    iact  <- rep(0, q)
    nact  <- 0
    r     <- min(n, q)
    sol   <- rep(0, n)
	lagr  <- rep(0, q)
    crval <- 0
    work  <- rep(0, 2 * n + r * (r + 5)/2 + 2 * q + 1)
    iter  <- rep(0, 2)
    res1  <- .Fortran("qpgen2"
	                 , as.double(Dmat)
					 , dvec = as.double(dvec)
					 , as.integer(n)
					 , as.integer(n)
					 , sol = as.double(sol)
					 , lagr = as.double(lagr)
					 , crval = as.double(crval)
					 , as.double(Amat)
					 , as.double(bvec)
					 , as.integer(n)
					 , as.integer(q)
					 , as.integer(meq)
					 , iact = as.integer(iact)
					 , nact = as.integer(nact)
					 , iter = as.integer(iter)
					 , work = as.double(work)
					 , ierr = as.integer(factorized)
					 , PACKAGE = "quadprog")

	#lhh modified the error handling so we can put the solver in a loop

    if (res1$ierr == 1) 
        list(status= -1, value = "constraints are inconsistent, no solution!")
    else if (res1$ierr == 2) 
        list(status=-2, value = "matrix D in quadratic function is not positive definite!")
	else
		list(status=0, solution = res1$sol, value = res1$crval, unconstrainted.solution = res1$dvec, 
 			iterations = res1$iter, iact = res1$iact[1:res1$nact])
}

# set up the plot and plot the data points in the training set
plot.setup <- function(df) {
	# uncomment the line below if you are running on a Mac
	quartz(width=8,height=8)

	# setup the plot
	plot(0:10,0:10,type="n",main="Maximum Margin Learning",xlab="x1",ylab="x2")
	
	# plot the classes: red = + and blue = -
	for (i in 1:length(df$x1))
		if (df$y[i] > 0 )
			points(df$x1[i],df$x2[i],col="red")
		else
			points(df$x1[i],df$x2[i],col="blue")
}

plot.decision.surface <- function(w,b) {
	slope = -(w[1]/w[2])
	offset =  (b)/w[2]
	offset1 =  (b+1)/w[2]
	offset2 =  (b-1)/w[2]

	cat("slope = ", slope, "offset = ", offset,"\n")
				
	# plot the decision surface with supporting hyperplanes
	abline(offset,slope,lty="solid",lwd=2,col="green")
	abline(offset1,slope,lty="dashed")
	abline(offset2,slope,lty="dashed")
}

get.constraints <- function(df) {

	transpose <- t(df)

	rows <- nrow(transpose)
	cols <- ncol(transpose)

	for (i in 1:cols) {
		
		y <- transpose[rows, i]

		for (j in 1:rows) {
			transpose[j, i] <- y * (transpose[j, i] ^ j)
		}
	}

	transpose
}
