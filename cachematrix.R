## The below functions check if the inverse of the matrix already exists returns its value
## If the inverse doesno exists it calculates it's value

## Returns a list which contains the set, get, setinverse, getinverse function
## Also contains the cached inverse of the matrix if has been previously calculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
        set <- function(y) {
                mat <<- y
                inv <<- NULL
        }
        get <- function() mat
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function calculates the inverse of the matrix if it exists
## The arguments to this function is the list returned from the makeCacheMatrix function

cacheSolve <- function(x, ...) {
	    m <- x$getinverse()
        if ( !is.null (m) ) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()
        
	if( det (data) != 0 ) {
        m <- solve(data)
        x$setinverse(m)
        m  ## Return a matrix that is the inverse of 'x'
	}
	else 
	message("Inverse cannot be calculated as determinant is 0") ## Return an error messgae if the matrix is not invertible
       
}
