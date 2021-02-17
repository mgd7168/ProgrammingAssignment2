## functions to allow keeping a copy of an matrix inverse for reuse
## and making the inverse as needed

## stores a function in a list to allow caching a copy of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
    m<-NULL
    set<-function(y) {
        x<<-y
        m<<-NULL
    }
    get<-function() x
    setinvert<-function(solve) m <<-solve
    getinvert<-function() m
    list(set=set, get=get, 
         setinvert=setinvert,
         getinvert=getinvert)
}


## function to check if inverse of matrix exists, if not creates and saves it


cacheSolve <- function(x, ...) {
    m<-x$getinvert()
    if(!is.null(m)) {
        message("retrieving cached matrix inverse")
        return(m)
    }
        ## Return a matrix that is the inverse of 'x'
    data<-x$get()
    m<-solve(data,...)
    x$setinvert(m)
    m
}
