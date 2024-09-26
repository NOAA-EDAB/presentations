# make these into a package please
# VAST model output utilities
#
# getmodinfo to pull all settings AIC and convergence test 
#   from a directory with model subdirectories
# 
# getmodindex to grab all index.csvs from a directory with model subdirectories
#
# modtable makes a table using getmodinfo given a directory of VAST outpyt directories
#
# 

modtable <- function(moddirs){
  
  # apply getmodinfo function to inout directories
  modcompare <- purrr::map_dfr(moddirs, getmodinfo)
  
  modselect <- modcompare |>
    dplyr::mutate(season = dplyr::case_when(stringr::str_detect(modname, "_fall_") ~ "Fall",
                                            stringr::str_detect(modname, "spring") ~ "Spring",
                                            stringr::str_detect(modname, "_all_") ~ "Annual",
                                            TRUE ~ as.character(NA))) |>
    dplyr::mutate(converged2 = dplyr::case_when(stringr::str_detect(converged, "no evidence") ~ "likely",
                                                stringr::str_detect(converged, "is likely not") ~ "unlikely",
                                                TRUE ~ as.character(NA))) |>
    dplyr::mutate(copegroup = stringr::str_extract(modname, "[^_]+")) |>
    #dplyr::mutate(modname = str_extract(modname, '(?<=allagg_).*')) |>
    dplyr::group_by(copegroup, season) |>
    dplyr::mutate(deltaAIC = AIC-min(AIC)) |>
    dplyr::select(copegroup, modname, season, deltaAIC, fixedcoeff,
                  randomcoeff, use_anisotropy, 
                  omega1, omega2, epsilon1, epsilon2, 
                  beta1, beta2, AIC, converged2) |>
    dplyr::arrange(copegroup, season, AIC)
  
  return(modselect)
}



# function to apply extracting info
getmodinfo <- function(d.name){
  # read settings
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modname <- modpath[length(modpath)]
  
  settings <- read.table(file.path(d.name, "settings.txt"), comment.char = "",
                         fill = TRUE, header = FALSE)
  
  n_x <- as.numeric(as.character(settings[(which(settings[,1]=="$n_x")+1),2]))
  grid_size_km <- as.numeric(as.character(settings[(which(settings[,1]=="$grid_size_km")+1),2]))
  max_cells <- as.numeric(as.character(settings[(which(settings[,1]=="$max_cells")+1),2]))
  use_anisotropy <- as.character(settings[(which(settings[,1]=="$use_anisotropy")+1),2])
  fine_scale <- as.character(settings[(which(settings[,1]=="$fine_scale")+1),2])
  bias.correct <- as.character(settings[(which(settings[,1]=="$bias.correct")+1),2])
  
  #FieldConfig
  if(settings[(which(settings[,1]=="$FieldConfig")+1),1]=="Component_1"){
    omega1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+2),2])
    omega2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),1])
    epsilon1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+4),2])
    epsilon2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+5),1])
    beta1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+6),2])
    beta2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+7),1])
  }
  
  if(settings[(which(settings[,1]=="$FieldConfig")+1),1]=="Omega1"){
    omega1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),1])
    omega2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+4),1])
    epsilon1 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+3),2])
    epsilon2 <- as.character(settings[(which(settings[,1]=="$FieldConfig")+4),2])
    beta1 <- "IID"
    beta2 <- "IID"
  }
  
  
  #RhoConfig
  rho_beta1 <- as.numeric(as.character(settings[(which(settings[,1]=="$RhoConfig")+3),1]))
  rho_beta2 <- as.numeric(as.character(settings[(which(settings[,1]=="$RhoConfig")+3),2]))
  rho_epsilon1 <- as.numeric(as.character(settings[(which(settings[,1]=="$RhoConfig")+4),1]))
  rho_epsilon2 <- as.numeric(as.character(settings[(which(settings[,1]=="$RhoConfig")+4),2]))
  
  # read parameter estimates, object is called parameter_Estimates
  if(file.exists(file.path(d.name, "parameter_estimates.RData"))) {
    load(file.path(d.name, "parameter_estimates.RData"))
    
    AIC <- parameter_estimates$AIC[1]  
    converged <- parameter_estimates$Convergence_check[1]
    fixedcoeff <- unname(parameter_estimates$number_of_coefficients[2])
    randomcoeff <- unname(parameter_estimates$number_of_coefficients[3])
    
  }else if(file.exists(file.path(d.name, "parameter_estimates.txt"))){
    
    reptext <- readLines(file.path(d.name, "parameter_estimates.txt"))
    
    AIC <- as.double(reptext[grep(reptext, pattern = "AIC")+1])
    converged <- reptext[grep(reptext, pattern = "Convergence_check")+1]
    fixedcoeff <- as.integer(stringr::str_split(reptext[grep(reptext, pattern = "number_of_coefficients")+2], 
                                                boundary("word"))[[1]][2])
    randomcoeff <- as.integer(stringr::str_split(reptext[grep(reptext, pattern = "number_of_coefficients")+2], 
                                                 boundary("word"))[[1]][3])
    
  }else{
    
    AIC <- NA_real_
    converged <- NA_character_
    fixedcoeff <- NA_integer_
    randomcoeff <- NA_integer_
  }
  
  
  #index <- read.csv(file.path(d.name, "Index.csv"))
  
  
  # return model attributes as a dataframe
  out <- data.frame(modname = modname,
                    n_x = n_x,
                    grid_size_km = grid_size_km,
                    max_cells = max_cells,
                    use_anisotropy = use_anisotropy,
                    fine_scale =  fine_scale,
                    bias.correct = bias.correct,
                    omega1 = omega1,
                    omega2 = omega2,
                    epsilon1 = epsilon1,
                    epsilon2 = epsilon2,
                    beta1 = beta1,
                    beta2 = beta2,
                    rho_epsilon1 = rho_epsilon1,
                    rho_epsilon2 = rho_epsilon2,
                    rho_beta1 = rho_beta1,
                    rho_beta2 = rho_beta2,
                    AIC = AIC,
                    converged = converged,
                    fixedcoeff = fixedcoeff,
                    randomcoeff = randomcoeff#,
                    #index = index
  )
  
  return(out)
  
}


# function to apply extracting info
getmodindex <- function(d.name){
  # read settings
  modpath <- stringr::str_split(d.name, "/", simplify = TRUE)
  modname <- modpath[length(modpath)]
  
  if(file.exists(file.path(d.name,"Index.csv"))){
    index <- read.csv(file.path(d.name, "Index.csv"))
  }else{
    stopifnot()
  }
  # return model indices as a dataframe
  out <- data.frame(modname = modname,
                    index
  )
  
  return(out)
}
