## Put comments here that give an overall description of what your
## functions do:
##
## These functions compute, store and return the inverse of a supplied matrix.
## The functions attempt to reduce the processing cost of inversion by re-using solutions
## that have already been computed.
##
## Write a short comment describing this function:
##
## This function generates a cache and family of four sub-functions which can be
## operated either directly by the user or the cacheSolve function automatically
## --------------------------------------------------------
## Inputs: It requires a square (N x N), non-singular matrix as an input.
## Syntax and Usage: makeCacheMatrix(M1) where M1 is a square, non-singular matrix.
## Outputs: A List of Matrix Operations: $set_input, $get_input, $set_inv, $get_inv,
##              cached inverse matrix(after further computation)

makeCacheMatrix <- function(x = matrix()) {
        
        matrix_inv <- NULL ## Reset Matrix Inverse
        
        ## SET INPUT: This sub-fn takes an input and assigns it to the makeCacheMatrix environment

        set_input <- function(y){
                x <<- y               ## Pass Input up to Parent environment
                matrix_inv <<- NULL   ## Wipe any exiting inversions
        }
        
        ## GET INPUT: This sub-fn returns the current stored matrix
        get_input <- function() x       
        
        ## SET INVERSE: This sub-fn caches the computed inverse from cacheSolve.
        
        set_inv <- function(inv) matrix_inv <<- inv
        
        ## GET INVERSE: This sub-fn returns the current stored inverse
        get_inv <- function() matrix_inv
        
        list(set_input = set_input, get_input = get_input,
             set_inv = set_inv,
             get_inv = get_inv)
}


## Write a short comment describing this function:
## This Function will either compute and store the inverse of a given matrix
## or retrieve an existing computed inverse from a cache.
## ------------------------------------------------------
## Inputs: It requires the output from the makeCacheMatrix Function
## Syntax and Usage: cacheSolve(makeCacheMatrix(M1)) where M1 is a square, non-singular matrix
## Outputs: A single matrix of identical dimensions to M1 (the inverse)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## Check and Return if an Inverse already exists
        
        matrix_inv <- x$get_inv()
        if(!is.null(matrix_inv)){
                message("Using cached inverse data")
                return(matrix_inv)
        }
        
        ## If no Inverse exists, Retrieve the Input Data and Solve(x)
        
        data <- x$get_input()
        matrix_inv <- solve(data, ...)
        
        ## Pass Inversion Solution back to Cache. Return Solution to Console.
        x$set_inv(matrix_inv)
        message("Computed inverse data")
        matrix_inv
}
