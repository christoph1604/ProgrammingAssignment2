## Programming Assignment 2
## Two functions for caching the inverse of a matrix (for more efficient calculation)

##Function makeCacheMatrix creates a vector containing 4 internal functions to set/get the matrix data and to set/get 
## cached inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){
		x <<- y
		inv <<- NULL
	}
	get<-function() x
	setinv<-function(i) inv <<- i
	getinv<-function() inv
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Function cacheSolve checks whether the internal of the matrix (in the vector) is already stored in the cache of the vector. 
## If yes, it returns it. If not, it calculates the inverse of the matrix, saves it in the cache and gives it back.

cacheSolve <- function(x, ...) {
	inv <- x$getinv()
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	data<-x$get()
	inv<-solve(data,...)
	x$setinv(inv)
	inv
}
