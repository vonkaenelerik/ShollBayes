#' Load a pre-specified hierachical model for Sholl inference
#'
#' Loads a pre-specified JAGS model
#'
#' @param model A string naming the desied model. Currenly supported models are "ug3_model", "mdnd_model", "crko_model", which correspond to the 3 examples in https://www.biorxiv.org/content/10.1101/2023.01.23.525256v1, respectively.
#'
#' @return A shollmcmcObj
#' @export
#'
#' @examples
#'
#' m = load_model("ug3_model")
#'
load_model = function(model){
    #initialize s4 object
    m = new("shollmcmcObj")

    #load external model data
    if(model == "ug3_model"){
        m@model_alias = model
        m@model = ShollBayes::ug3_model
        m@init_fun = ShollBayes::ug3_inits
        # m = list(model_alias = model, model = ShollBayes::ug3_model, init_fun = ShollBayes::ug3_inits)
    } else if(model == "mdnd_model"){
        m@model_alias = model
        m@model = ShollBayes::mdnd_model
        m@init_fun = ShollBayes::mdnd_inits
        # m = list(model_alias = model, model = ShollBayes::mdnd_model, init_fun = ShollBayes::mdnd_inits)
    } else if(model == "crko_model"){
        m@model_alias = model
        m@model = ShollBayes::crko_model
        m@init_fun = ShollBayes::crko_inits
        # m = list(model_alias = model, model = ShollBayes::crko_model, init_fun = ShollBayes::crko_inits)
    } else {
        stop("Model not recognized. Please see ?load_model for a list of pre-specified models.")
    }
    return(m)
}







