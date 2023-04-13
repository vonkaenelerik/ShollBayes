setClass("shollmcmcObj",
         slots = list(model_alias = "character",
                      model = "character",
                      jags_data = "list",
                      jags_model = "jags",
                      init_fun = "function",
                      samples = "mcmc.list")) #),
         # validity = check_stackObj)


### Setters & Getters

#sample

#' sample()
#'
#' Retrieve the sample slot from an object of class shollmcmcObj
#'
#' @param x An object of class shollmcmcObj
#'
#' @return
#'
#' The stack slot of shollmcmcObj
#'
#' @export
#'
#' @examples
#'
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
#'
setGeneric("sample", function(x) standardGeneric("sample"))


#' sample<-
#'
#' Set the sample slot in an object of class shollmcmcObj
#'
#' @param x An object of class shollmcmcObj
#' @param value Value to assign to slot.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#'
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
#'
setGeneric("sample<-", function(x, value) standardGeneric("sample<-"))

#' stack()
#'
#' Retrieve the sample slot from an object of class shollmcmcObj
#'
#' @param x An object of class shollmcmcObj
#'
#' @return
#'
#' The sample slot of shollmcmcObj
#'
#' @export
#'
#' @examples
#'
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
#'
setMethod("sample", "shollmcmcObj", function(x) x@sample)

#' sample<-
#'
#' Set the sample slot in an object of class shollmcmcObj
#'
#' @param x An object of class shollmcmcObj
#' @param value Value to assign to slot.
#'
#' @return NULL
#'
#' @export
#'
#' @examples
#'
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
#'
setMethod("sample<-", "shollmcmcObj", function(x, value) {
    x@sample <- value
    x
})


