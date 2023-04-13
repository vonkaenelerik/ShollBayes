#' Restructures a data.frame into a format readable by rjags
#'
#' @param m A shollmcmcObj created by initialize_model()
#' @param ... Additional arguments, see Details.
#'
#' @details The required arguments for the currently supported models are included below:
#' test
#' another test
#' yet another test
#'
#' @return A shollmcmcObj
#' @export
#'
#' @examples
#' #See vignette("ungrouped_mouse_example", package = "ShollBayes") for examples.
restructure_data = function(m, ...){
    extra_args = list(...)
    if(m@model_alias == "ug3_model"){
        if(!all(c("y", "R", "Cell_ID", "Animal_ID", "Image_ID") %in% names(extra_args))) stop(paste0("You must also specify the columns:\n\t", paste(c("y", "R", "Cell_ID", "Animal_ID", "Image_ID")[!(c("y", "R", "Cell_ID", "Animal_ID", "Image_ID") %in% names(extra_args))], collapse = '\n\t'), "\nSee ?restructure_data for details."))
        d = restructure_data_ug3(extra_args$y,
                                 extra_args$R,
                                 extra_args$Cell_ID,
                                 extra_args$Image_ID,
                                 extra_args$Animal_ID)
    } else if(m@model_alias == "mdnd_model"){
        if(!all(c("y", "R", "Animal_ID", "Group_ID") %in% names(extra_args))) stop(paste0("You must also specify the columns:\n\t", paste(c("y", "R", "Animal_ID", "Group_ID")[!(c("y", "R", "Animal_ID", "Group_ID") %in% names(extra_args))], collapse = '\n\t'), "\nSee ?restructure_data for details."))
        d = restructure_data_mdnd(extra_args$y,
                                  extra_args$R,
                                  extra_args$Animal_ID,
                                  extra_args$Group_ID)
    } else if(m@model_alias == "crko_model"){
        if(!all(c("y", "R", "Cell_ID", "Group_ID", "Animal_ID", "Genotype_ID") %in% names(extra_args))) stop(paste0("You must also specify the columns:\n\t", paste(c("y", "R", "Cell_ID", "Group_ID", "Animal_ID", "Genotype_ID")[!(c("y", "R", "Cell_ID", "Group_ID", "Animal_ID", "Genotype_ID") %in% names(extra_args))], collapse = '\n\t'), "\nSee ?restructure_data for details."))
        d = restructure_data_crko(extra_args$y,
                                  extra_args$R,
                                  extra_args$Cell_ID,
                                  extra_args$Group_ID,
                                  extra_args$Animal_ID,
                                  extra_args$Genotype_ID)
    } else{
        stop("Model not recognized. Please see ?restructure_data for a list of pre-specified models.")
    }
    m@jags_data = d
    return(m)
}

restructure_data_ug3 = function(y, R, Cell_ID, Image_ID, Animal_ID){
    #coerce data to data.frame
    data = data.frame(y, R, Cell_ID, Image_ID, Animal_ID)

    #might need to arrange data here
    data = data %>% dplyr::arrange(Animal_ID, Image_ID, Cell_ID, R)

    N = nrow(data)

    data_cell_level = data %>% distinct(Cell_ID, .keep_all = TRUE) %>% select(Cell_ID, Image_ID)
    N.Cell = nrow(data_cell_level)

    data_image_level = data %>% distinct(Image_ID, .keep_all = TRUE) %>% select(Image_ID, Animal_ID)
    N.Image = nrow(data_image_level)

    data_animal_level = data %>% distinct(Animal_ID)
    N.Animal = nrow(data_animal_level)

    if (!all(data$y %% 1 == 0)) {
        warning("Provided sholl crossings are not composed entirely of integers. Rounding to nearest integer.")
        data$y = round(data$y)
    }

    d = list(y = data$y,
             x = data$R,
             Cell_ID = as.numeric(as.factor(data$Cell_ID)),
             Image_ID = as.numeric(as.factor(data_cell_level$Image_ID)),
             Animal_ID = as.numeric(as.factor(data_image_level$Animal_ID)),
             N = N,
             N.Cell = N.Cell,
             N.Image = N.Image,
             N.Animal = N.Animal)

    return(d)
}


restructure_data_mdnd = function(y, R, Animal_ID, Group_ID){
    #coerce data to data.frame
    data = data.frame(y, R, Animal_ID, Group_ID)

    #might need to arrange data here
    data = data %>% arrange(Group_ID, Animal_ID, R)

    N = nrow(data)

    data_animal_level = data %>% distinct(Animal_ID, Group_ID, .keep_all = TRUE)
    N.Animal = nrow(data_animal_level)

    # data_group_level = data %>% distinct(Group_ID, Animal_ID, .keep_all = TRUE)
    # N.Group = nrow(data_group_level)

    if (!all(data$y %% 1 == 0)) {
        warning("Provided sholl crossings are not composed entirely of integers. Rounding to nearest integer.")
        data$y = round(data$y)
    }

    d = list(y = data$y,
             x = data$R,
             Animal_ID = as.numeric(as.factor(data$Animal_ID)),
             Cond_Side_ID = as.numeric(as.factor(data_animal_level$Group_ID)),
             N = N,
             N.Animal = N.Animal)

    return(d)
}


restructure_data_crko = function(y, R, Cell_ID, Group_ID, Animal_ID, Genotype_ID){
    #coerce data to data.frame
    data = data.frame(y, R, Cell_ID, Group_ID, Animal_ID, Genotype_ID)

    #might need to arrange data here
    data = data %>% arrange(Genotype_ID, Animal_ID, Group_ID, Cell_ID, R)

    N = nrow(data)

    data_cell_level = data %>% distinct(Cell_ID, .keep_all = TRUE)
    N.Cell = nrow(data_cell_level)

    data_group_level = data %>% distinct(Group_ID, .keep_all = TRUE)
    N.Image = nrow(data_group_level)

    data_animal_level = data %>% distinct(Animal_ID, .keep_all = TRUE)
    N.Animal = nrow(data_animal_level)

    data_genotype_level = data %>% distinct(Genotype_ID, .keep_all = TRUE)
    N.Genotype = nrow(data_genotype_level)

    Cell_ID = as.numeric(as.factor(data$Cell_ID))
    Animal_ID = as.numeric(as.factor(data_cell_level$Animal_ID))
    Group_ID = as.numeric(as.factor(data_cell_level$Group_ID)) - 1 # minus 1 to make is 0-1 coding
    Genotype_ID_4Int = 1 - (as.numeric(as.factor(data_cell_level$Genotype_ID)) - 1)  # minus 1 to make is 0-1 coding
    Genotype_ID = 3 - as.numeric(as.factor(data_animal_level$Genotype_ID))

    if (!all(data$y %% 1 == 0)) {
        warning("Provided sholl crossings are not composed entirely of integers. Rounding to nearest integer.")
        data$y = round(data$y)
    }

    d = list(y = data$y,
             x = data$R,
             Cell_ID = Cell_ID,
             Animal_ID = Animal_ID,
             Genotype_ID = Genotype_ID,
             Genotype_ID_4Int = Genotype_ID_4Int,
             Group_ID = Group_ID,
             N = N,
             N.Cell = N.Cell,
             N.Animal = N.Animal)

    return(d)
}
