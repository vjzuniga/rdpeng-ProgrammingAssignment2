
makeCacheMatrix <- function(x = matrix() ) {
        MInversa  <- NULL
        set <- function(y) {
                x <<- y
                MInversa  <<- NULL
        }
        get <- function() x
        setMInversa <- function(solve) MInversa <<- solve
        getMInversa <- function() MInversa
        list(set = set, get = get,
             setMInversa = setMInversa,
             getMInversa = getMInversa)
}


cacheSolve <- function(x, ...) {
        MInversa <- x$getMInversa()
        if(!is.null(MInversa)) {
                message("getting cached data")
                return(MInversa)
        }
        data <- x$get()
       MInversa<- solve(data, ...)
        x$setMInversa(MInversa)
        MInversa
}