# makeCacheMatrix() is an object that contains a matrix and can store the inverse of
# this matrix.
# cacheSolve() calculates the inverse of a makeCacheMatrix() matrix if it doesn't already
# contain the inverse. Otherwise cacheSolve returns the stored inverse.

## makeCacheMatrix contains 2 variables and a get/set function for each
## <x> is a matrix, which is set when the function is called.
## <inverse> contains the inverse of this matrix once it has been calculated.
## get() and getInverse() return <x> or <inverse> respectively.
## set(y) changes the value of <x> to <y>, it also sets <inverse> to NULL since 
##  it will no longer be correct.
## setInverse(newInverse) changes the value of <inverse> to <newInverse>
#  Both set functions use the <<- assignment to change the variables in makeCacheFunction()
# instead of creating a new variable in the set function.

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(y) {
		x <<- y
		inverse <<- NULL

	}
	
	get <- function() {
		x
	}
	
	getInverse <- function() {
		inverse
	}
	
	setInverse <- function(newInverse) {
		inverse <<- newInverse

	}
	#return
	list( set = set, get = get,
		getInverse = getInverse,
		setInverse = setInverse )
}


# cacheSolve returns the inverse of the matrix in the makeCacheMatrix
# argument. If the makeCacheMatrix already contains an inverse (i.e. the
# <inverse> is not NULL) it will return this inverse, otherwise the inverse
# is calculated using solve().

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	inverse <- x$getInverse()
	if( !(is.null(inverse) )) {
		message("Retrieving inverse from cache.")
	}
	else {
		inverse <- solve(x$get())
		x$setInverse(inverse)
	}
	#return
	inverse
}