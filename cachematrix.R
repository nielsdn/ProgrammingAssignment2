## The function cacheSolve takes an invertible square matrix and returns the 
## inverse of that matrix. At the same time a cached version of the inverse 
## matrix is stored. 
## When cacheSolve is later called, it returns the cached version of the 
## inversed matrix if the parsed matrix is identical to the matrix from 
## the previous call.

## The function makeCacheMatrix takes a matrix and stores (and returns) a 
## 3D matrix (a list of 2 matrices) in the variable 'cachelist'. 
## 'cachelist$org' contains the matrix parsed in the function call.
## 'cachelist$cache' contains a matrix of the same dimensions as the parsed 
## matrix but containing only NA's.

makeCacheMatrix <- function(x = matrix()) {
        cache <- matrix(nrow = dim(x)[1], ncol = dim(x)[2])
        cachelist <<- list(org = x, cache = cache)
}


## cacheSolve takes an invertible square matrix. If 'cachelist' (see above) 
## does not exist it is created by calling 'makeCacheMatrix'.
## If the first value of the cached matrix ('cachelist$cache') is NA (ie. no 
## cached inverse matrix is available) or the parsed matrix is not identical to 
## the previosly inverted matrix ('cachelist$org') the parsed matrix is inverted 
## and saved in the cache ('cachelist$cache') along with a copy of itself 
## ('cachelist$org').
## Finally the inverted matrix is returned.

cacheSolve <- function(x, ...) {
        if (!is.matrix(x)) {
                stop("Please parse a matrix.")
        } else if (dim(x)[1] != dim(x)[2]) {
                stop("Please parse a square matrix (number of rows = number of columns).")
        }
        if (!exists("cachelist")) {
                makeCacheMatrix(x)  
        }
        if (is.na(cachelist$cache[1,1]) | !identical(x, cachelist$org)) {  
                cachelist$cache <<- solve(x)
                cachelist$org <<- x
        }
        cachelist$cache
 }
