# As the calculation of inversion of a matrix is costly, the following two 
# functions are developed to cache the inverse of a matrix.

# makeCacheMatrix creates a list of functions to
# set the value of matrix x
# get the value of matrix x
# set the value of inverse of matrix x
# get the value of inverse of matrix x
makeCacheMatrix <- function(x = matrix()){
	inver <- NULL

	set <- function(y) {
		x <<- y
	 	inver <<- NULL
	}

	get <- function() x
	setInver <- function(solve) inver <<- solve
	getInver <- function() inver

	list(set = set, get = get, setInver = setInver, getInver = getInver)
}


# The cacheSolve function returns the inverse of the matrix. If
# the inverse has already been computed, it returns its value. 
# Otherwise, it computes the inverse. 

# cacheSolve assumes that x is invertible
cacheSolve <- function(x, ...) {
        inver <- x$getInver()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        data <- x$get()
        inver <- solve(data, ...)
        x$setInver(inver)
        inver
}


# Running the code 
# A <- matrix(1:4,2,2)
# A
#     [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# AA <- makeCacheMatrix(A)
# AA$get()
#     [,1] [,2]
# [1,]    1    3
# [2,]    2    4
# cacheSolve(AA)
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
# cacheSolve(AA)
#getting cached data
#     [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5


