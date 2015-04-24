## The two functions together give the inverse of a matrix processed in the cache

## Gives a special object that points to the matrix in memory which will be used to find the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y){
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setmatrix<-function(solve) m<<- solve
    getmatrix<-function() m
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}


## Uses the R function "solve" for inverting the above matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
            m<-x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix<-x$get()
    m<-solve(matrix, ...)
    x$setmatrix(m)
    m
}
