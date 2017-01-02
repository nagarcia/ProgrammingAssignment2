#Overall, the purpose of this set of functions is to cache and calculate the inverse of 
#matrices. In the case that a matrix whose inverse has already been cached is passed through
#the functions, they will (quickly) return the inverse of that matrix without recomputing it.

#This function returns a list of named functions that can be used in the subsequent function 
#to set and get the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


#This function returns the inverse of the matrix. If the inverse is not already cached,
#the function will calculate the inverse. If it is already cached, the function will return
#"getting cached data" along with the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

