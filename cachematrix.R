## This function creates a special "matrix" object that can 
## cache its inverse
## This function returns a "matrix" object
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- solve(x)
       if(!is.null(m)) {
                message("getting Inverse of a Matrix")
                return(m)
        }
        data <- solve(x)
        m <- mean(data, ...)
}
