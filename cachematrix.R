## The two function below work together to calculate the inverse of a matrix
## In addition, the functions will cache the solution for the inverse of a matirx so it can be retrived later 
## without recalculation.

## make CacheMatrix creates a list that contains 4 functions that perform the following roles:
        # 1. set the value of the matrix
        # 2. retrieve the value of the matrix
        # 3. set the value of the inverse matrix
        # 4. retrieve the value inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        
        # set m to NULL, this is used in cacheSolve. If m is NULL then R needs to calculate the inverse, otherwise
        # r can return the cached version of the inverse matrix
        m <- NULL
        
        # creates function reads in an object and assigns object to "x" in a separate environment
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        
        # creates function that returns the value stored as "x" in the separate environment
        get <- function() x
        # creates function that assignes the inverse matrix to m in the cached environment
        set.inverse <- function(inverse) m <<- inverse
        # function that returns m
        get.inverse <- function() m
        
        #creates a list of the four functions above to be used in cacheSolve
        list(set = set, 
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


## cacheSolve solves for the inverse of a matrix if the inverse has not already been calculated and cached.
## if its already been cached then it will simply return the cached inverse matrix

cacheSolve <- function(x, ...) {
        #assigned "m" from cache environment to "m" 
        m <- x$get.inverse()
        #if "m" has been cached then return the cached value for m
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        # if "m" hasn't been cached yet solve for the inverse and cache as "m"
        data <- x$get()
        m <- solve(data, ...)
        x$set.inverse(m)
        m
}
