# The following two functions can be used to cache the inverse of a matrix

## The first function is "makeCacheMatrix". 
### This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y){
                                x<<-y
                                m<<-NULL
                }
                get <- function() x
                setmatrix <- function(solve) m <<- solve
                getmatrix <- function() m
                list(set=set, get=get,
                     setmatrix=setmatrix,
                     getmatrix=getmatrix)
}


## The second function computes the inverse of the special 
###"matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
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

### The following is a test for the two functions:

test <- makeCacheMatrix() # assign the first function to a variable called "test"

test$set(matrix(1:4, 2, 2)) # assign (or set) the matrix to be sovled

cacheSolve(test) # return the inverse of the test matrix
