## These functions store / retrieve the inverse of a matrix. If the inverse has
#not already been computed, the function cacheSolve computes it.


## This function creates an object with properties a and i + four "methods"
        #sett    if called, assign new value b to property a
        #gett    if called, return a
        #setinvrs    if called, assign argument to i
        #getinvrs    if called, return i

makeCacheMatrix <- function(a = matrix()){
        i <- NULL
        sett <- function(b) {   #if called, assign new value b to property a
                a <<- b
                i <<- NULL
        }
        gett <- function () a     #if called, return a
        setinvrs <- function(solve) i <<- solve  #in called, set argument as i
        getinvrs <- function() i                  #if called, return i
        list (sett = sett,            #output = list of 4 fxns
              gett = gett, 
              setinvrs = setinvrs, 
              getinvrs = getinvrs)
}


## This function treats an object (created above). It first calls the getinvrs
#method and checks if the value has been computed already or not. If not, it 
#computes and returns the value.

cacheSolve <- function(a, ...) {
        i <- a$getinvrs()        #will be null if never done before
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        dataa <- a$gett()
        i <- solve(dataa, ...)
        i
}
