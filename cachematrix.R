## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i<-NULL
	set<-function(y) {
		x<<-y
		i<<-NULL	##initialize the inverse as NULL
	}
	
	get<-function() x	## returns the matrix
	setinverse <- function(solve) i <<- solve
	getinverse <- function() i
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
##This function accesses the cached data if it is not cached
cacheSolve <- function(x, ...) {
        
		##Check if an inverse is cached
		i<-x$getinverse()
		if (!is.null(i)) {
			message("Getting cached inverse matrix")
			return(i)	## return cached data and exit function
		}
		
		message("No cached data found. Calculating Inverse")
		data<-x$get()	##get matrix
		i<-solve(data,...)	##compute inverse
		x$setinverse(i)
		i	## Return a matrix that is the inverse of 'x'
}
