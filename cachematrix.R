## Put comments here that give an overall description of what your
## functions do

## Function to set / get the inverse of a matrix in cache

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set <- function(y) {
        x<<-y
        m<<-NULL
    }
    get <- function() x
    setmtrxinv <- function(minv) m <<- minv
    getmtrxinv <- function() m
    list(set=set, get=get, setmtrxinv=setmtrxinv, getmtrxinv=getmtrxinv)
}


## Function to calculate the inverse of a matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmtrxinv()
    if(!is.null(m)) {
        message("getting cache inverse matrix")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setmtrxinv(m)
    m
}
