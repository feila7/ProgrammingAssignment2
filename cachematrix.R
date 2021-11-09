## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse
## it's a list containing a function to set the value of the matrix,
## get the value of matrix, set the value of inverse and get the value of inverse
library(MASS) 
#use function in MASS package for non-square inverse

makeCacheMatrix <- function(x = matrix()) {
        invs <-NULL
        #set the value of matrix
        set <- function(y){
                x<<-y
                invs<<-NULL
        }

        get <- function() {x}
        setinverse <- function(inverse) {invs <<-inverse}
        getinverse <- function(){
                inver <- ginv(x)
                inver%*%x
                }
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## This function computes the inverse of the special "matrix" 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache and skips the computation,
## otherwise it calculates the inverse of the matrix and set inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs<-x$getinverse()
        if(!is.null(invs)){
                message("getting cached data")
                return (invs)
                
        }
        data<- x$get()
        invs<- solve(data, ...)
        x$setinverse(invs)
        invs
        }
