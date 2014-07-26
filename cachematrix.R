## creates a special matrix object, caches its inverse
makeCacheMatrix <- function(x = matrix()){
        inverse<-NULL
        set<-function(y){ ## set the value of the x matrix
             x<<-y ## assign value to x, if different from the current environment
             inverse <<-NULL ## assign value to inverse, if different from the current environment
        }
        get<-function()x ## get the value of the x matrix
        setInverse<-function(solve) inverse<<-solve ## set inverse of the matrix x
        getInverse<- function () inverse ## get the inverse of the matrix x
        list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## Computes the inverse of the special matrix object created by makeCacheMatrix function.
## If the inverse has been calculated and the matrix has not changed,
## cacheSolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...){
        inverse <-x$getInverse()
        if (!is.null(inverse)) {
                message("gettig cached data")
                return (inverse)
        }
        data<-x$get()
        inverse<-solve(data,...)
        x$setInverse(inverse)
        inverse ## returns the inverse of the special matrix
}
