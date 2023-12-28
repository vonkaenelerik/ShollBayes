#' Initialize a jags model prior to MCMC sampling.
#'
#' @param m A shollmcmcObj created by initialize_model()
#' @param n.chains The number of chains used for MCMC sampling. For details see ?rjags::jags.model.
#' @param n.adapt The number of adaptation iterations. For details see ?rjags::jags.model.
#' @param quiet For details see ?rjags::jags.model.
#'
#' @return A shollmcmcObj
#' @export
#'
#' @examples
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
initialize_model = function(m, n.chains = 1, n.adapt = 1000, quiet = FALSE){
    if(n.chains > 8){
        stop("The number of chains must be <=8.")
    }
    if(m@model_alias == "ug3_model"){
        jgs_m = initialize_model_ug3(m = m,
                                     n.chains = n.chains,
                                     n.adapt = n.adapt,
                                     quiet = quiet)
    } else if(m@model_alias == "mdnd_model"){
        jgs_m = initialize_model_mdnd(m = m,
                                     n.chains = n.chains,
                                     n.adapt = n.adapt,
                                     quiet = quiet)
    } else if(m@model_alias == "crko_model"){
        jgs_m = initialize_model_crko(m = m,
                                     n.chains = n.chains,
                                     n.adapt = n.adapt,
                                     quiet = quiet)
    } else{
        stop("Model not recognized. Please see ?initialize_model for a list of pre-specified models.")
    }
    m@jags_model = jgs_m
    return(m)
}

#' Fits the UG3 model
#' @param m A shollmcmcObj created by initialize_model()
#' @param n.chains The number of chains used for MCMC sampling. For details see ?rjags::jags.model.
#' @param n.adapt The number of adaptation iterations. For details see ?rjags::jags.model.
#' @param quiet For details see ?rjags::jags.model.
#' @keywords internal
initialize_model_ug3 = function(m, n.chains, n.adapt, quiet){
    jgs_m = rjags::jags.model(file = textConnection(m@model),
                              data = m@jags_data,
                              inits = function(chain) m@init_fun(chain = n.chains,
                                                                 N.Cell = m@jags_data$N.Cell,
                                                                 N.Image = m@jags_data$N.Image,
                                                                 N.Animal = m@jags_data$N.Animal),
                              n.chains = n.chains,
                              n.adapt = n.adapt,
                              quiet = quiet)
    return(jgs_m)
}

#' Fits the MDND model
#' @param m A shollmcmcObj created by initialize_model()
#' @param n.chains The number of chains used for MCMC sampling. For details see ?rjags::jags.model.
#' @param n.adapt The number of adaptation iterations. For details see ?rjags::jags.model.
#' @param quiet For details see ?rjags::jags.model.
#' @keywords internal
initialize_model_mdnd = function(m, n.chains, n.adapt, quiet){
    jgs_m = rjags::jags.model(file = textConnection(m@model),
                              data = m@jags_data,
                              inits = function(chain) m@init_fun(chain = n.chains,
                                                                 N.Animal = m@jags_data$N.Animal,
                                                                 N.Cond_Side = length(unique((m@jags_data$Cond_Side_ID)))),
                              n.chains = n.chains,
                              n.adapt = n.adapt,
                              quiet = quiet)
    return(jgs_m)
}

#' Fits the CRKO model
#' @param m A shollmcmcObj created by initialize_model()
#' @param n.chains The number of chains used for MCMC sampling. For details see ?rjags::jags.model.
#' @param n.adapt The number of adaptation iterations. For details see ?rjags::jags.model.
#' @param quiet For details see ?rjags::jags.model.
#' @keywords internal
initialize_model_crko = function(m, n.chains, n.adapt, quiet){
    jgs_m = rjags::jags.model(file = textConnection(m@model),
                              data = m@jags_data,
                              inits = function(chain) m@init_fun(chain = n.chains,
                                                                 N.Cell = m@jags_data$N.Cell,
                                                                 N.Genotype = length(unique(m@jags_data$Genotype_ID)),
                                                                 N.Animal = m@jags_data$N.Animal,
                                                                 N.Groups = m@jags_data$N.Group),
                              n.chains = n.chains,
                              n.adapt = n.adapt,
                              quiet = quiet)
    return(jgs_m)
}



#' A function to burn-in a specified number of iterations.
#'
#' @param m A shollmcmcObj created by initialize_model()
#' @param n.iter Number of burn-in iterations
#' @param ... Additional arguments. See rjags::update.
#'
#' @return A shollmcmcObj
#' @export
#'
#' @examples
#'
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
#'
#'
update_model = function(m, n.iter, ...){
    extra_args = list(...)

    update(object = m@jags_model,
           n.iter = n.iter,
           ... = extra_args)
}


#' Obtain MCMC samples from a jags model
#'
#' @param m A shollmcmcObj created by initialize_model()
#' @param variable.name A string vector specifying which parameters should be monitored during sampling. Default is "All", which monitors all parameters. We recommend using this argument unless computationally limited
#' @param n.iter Number of MCMC samples to obtain.
#' @param thin Thinning interval
#' @param na.rm Logical flag that indicates whether variables containing missing values should be omitted. See ?coda::coda.samples for details.
#' @param new_sample Logical. If TRUE any existing samples in the samples slot will be overwritten, if FALSE they will be appended.
#' @param ... Additional arguments. See rjags::coda.samples.
#'
#' @return A shollmcmcObj
#' @export
#'
#' @examples
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
sample_model = function(m, variable.name = 'All', n.iter, thin = 1, na.rm = FALSE, new_sample = TRUE, ...){
    extra_args = list(...)
    if(variable.name == 'All'){
        if(m@model_alias == "ug3_model"){
            variable.name = c(names(m@init_fun(1, m@jags_data$N.Cell, m@jags_data$N.Image, m@jags_data$N.Animal)))
        } else if(m$model_alias == "mdnd_model"){
            variable.name = c(names(m@init_fun(1, N.Animal = m@jags_data$N.Animal, N.Cond_Side = length(unique(m@jags_data$Cond_Side_ID)))))
        } else if(m$model_alias == "crko_model"){
            variable.name = c(names(m@init_fun(1, N.Cell = m@jags_data$N.Cell, N.Genotype = length(unique(m@jags_data$Genotype_ID)), N.Animal = m@jags_data$N.Animal, N.Groups = m@jags_data$N.Group)))
        } else{
            stop("Model not recognized. Please see ?initialize_model for a list of pre-specified models.")
        }
    }


    samp = rjags::coda.samples(model = m@jags_model,
                        variable.names = variable.name,
                        n.iter = n.iter,
                        thin = thin,
                        na.rm = na.rm,
                        ... = extra_args)


    if(new_sample){
        m@samples = samp
    } else {
        if(is.null(m@samples)) {
            m@samples = samp
        } else {
            m@samples = c(m@samples, samp)
        }
    }

    return(m)
}







