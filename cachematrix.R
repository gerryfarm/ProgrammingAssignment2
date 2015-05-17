#These functions work together to cache and calculate inverses of square matrices.


# The following function, makeCacheMatrix, takes a matrix x as input, and returns a number of functions, stored in a list.
# Calls will be in the form of b<-MakeCacheMatrix() followed by b$set(mymat) or b<-MakeCacheMatrix(mymat), where mymat is a matrix. The matrix is
# saved (cached) in the environment of makeCacheMatrix. The value of that matrix can be recalled by then using b$get().
# b$setsolve is used to set the value of another cached variable, also saved in the environment of makeCacheMatrix. (This is used by the sisteer
# function, cacheSolve, to store the inverse of the original cached matrix (eg mymat)).
# It should be noted that each time a new matrix is set, the matinv matrix is reset to null, thus ensuring that if the matinv matrix exists, it equals the 
# inverse of the stored matrix.


makeCacheMatrix <- function(x = matrix()) {
		matinv <- NULL
		set <- function(y){
			 x <<- y
			 matinv <<- NULL}
		get <- function() x
		setsolve <- function(solve) matinv <<- solve
		getsolve <- function() matinv	
		list (set=set, get= get, 
			setsolve = setsolve,
			getsolve = getsolve)}


# The following function takes on as input the list object created by makeCacheMatrix. If the inverse of the matrix cached in the makeCacheMatrix has
# not yet been set, then it inverts it using the "solve" function and then stores it. If it already exists then it inverts it.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		matinv <- x$getsolve()
		if (!is.null(matinv)) {
			message("getting cached data")
			return(matinv)}
		data <- x$get()
		matinv <- solve(data,...)
		x$setsolve(matinv)
		matinv}

