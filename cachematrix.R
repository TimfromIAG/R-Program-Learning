

# makeCacheMatrix creates a list containing a function to
# 1. set matrix value 
# 2. get matrix value 
# 3. set matrix Inverse
# 4. get matrix Inverse

makeCacheMatrix <- function(x = matrix()) {   #makeCacheMatrix takes input of matrix x
    inv_matrix <- NULL   #Set Inv_matrix to null 
    set <- function(y) {    # Set function takes arguement y and sets x in an outer environment to y and nullifies inv_matrix
        x <<- y
        inv_matrix <<- NULL
    }
    get <- function() { x }    
    setinverse <- function(inverse) {  inv_matrix <<- inverse }
    getinverse <- function() { inv_matrix }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

cacheSolve <- function(x, ...) {
    inv_matrix <- x$getinverse()
    if(!is.null(inv_matrix)) {
        message("Retrived not calculated")
        return(inv_matrix)  			# stop and return if already calculated
    }
    data <- x$get()  				# Gets the required matrix
    inv_matrix <- solve(data)  		# Solves for the inverse
    x$setinverse(inv_matrix)			# Sets the variable inv_matrix to the inverse
    inv_matrix					# returns inv_matrix
}
