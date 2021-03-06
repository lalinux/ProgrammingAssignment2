## This functions are useful if you want to cache the inverse of a matrix.
## First, you need to pass your matrix to the makeCacheMatrix function, 
## it will create a cacheable matrix, then, every time you need to get the 
## inverse, call the cacheSolve function with your previously created 
## cacheable matrix

## initialize a cacheable matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x<-y
                inv<-NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv<<-inverse
        getinverse <- function() inv
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## takes a cacheable matrix and calculate the inverse of it if is the first time
## otherwise, return the previously calculated inverse matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)){
                print("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
