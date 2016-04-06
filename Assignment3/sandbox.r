#run <- function() {
#	df  <- read.csv("trainingset.csv")
#	train(df)
#}

train <- function(df) {
	num.rows  <- nrow(df)
	num.cols  <- ncol(df)
	r         <- max(sqrt(df$x1^2 + df$x2^2))
	q         <- 20
	b         <- (-1) * q
	step      <- .1

	# Construct X according to (6.32) using D.
    X  <- get.constraints(df)

	# The n x n identity matrix.
	I  <- diag(nrow(X))

	# Set a temp variable. The expression below seems to
	# not apply the - 1 before the loop.
#	num.cols2    <- num.cols - 1
	identity_vec <- 0 * (1:nrow(X))



	##########################################
	##########################################



#    c(w_ast_vec, b_ast)
	X
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

get.constraint.matrix <- function(df) {

	num.rows  <- nrow(df)
	num.cols  <- ncol(df)

   	mat  <- matrix(data=NA, nrow=num.rows, ncol=num.cols-1)

	for (i in 1:num.rows) {
		mat[i,] <- c((df[i,]$x1 ^ i) * df[i,]$y, (df[i,]$x2 ^ i) * df[i,]$y)
	}

	mat
}

library(quadprog)

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

    if (q != length(bvec)) {
		print("Amat columns")
		print(q)
		print("length bvec")
		print(length(bvec))
        stop("Amat and bvec are incompatible!")
	}

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
