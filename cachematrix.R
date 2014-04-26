## These two functions will take a matrix and return the inverse of it either from a previously cached value or 
## after computing the inverse and adding it to a cache for subsequent calls

## Takes in a mtrix and return a list with 4 functions :
## getter and setter for the matrix, and getter and setter for inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL             #initialize inverse to NULL
        set <- function(y)   #define setter function for input matrix
        {       x <<- y
                i <<- NULL
        }
        get <- function() x   #define getter method for matrix
        setinverse <- function(inverse) i <<- inverse   #define setter for inverse
        getinverse <- function() i       #define getter for inverse
        list(set = set, get = get,       #add all 4 functions to a list and return
             setinverse = setinverse ,
             getinverse = getinverse )
}


## Takes in a special vector : the list of functions returned by the makeCacheMatrix function and 
## returns the inverse of the matrix either from the cache(if present) or after computation


cacheSolve <- function(x, ...) {
        m <- x$getinverse()    #query the x vector's cache
        if(!is.null(m))        #if there is a cache
        {      
                message("getting cached data")
                return(m)      #just return the cache, no computation needed
        }
        data <- x$get()        #if there's no cache
        m <- solve(as.matrix(data.frame(data)))  #we actually compute them here
        x$setinverse(m)        #save the result back to x's cache
        m                      #return the result
}



