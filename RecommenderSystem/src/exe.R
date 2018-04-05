
## THIS WILL EXECUTE THE RECOMMENDER SYSTEM ##


wd <- rprojroot::find_rstudio_root_file() # find the root file of the project - will be used as default directory
setwd(wd)


readinteger <- function(){
	
	n <- readline(prompt="Are you a new customer? Yes or No:")
	new <- c("TRUE","yes","YES","Yes","y","Y","T")

	if (n %in% new) {
	
		source("./src/4_predict_by_popularity.R")
	}

#	source("./src/5_predict_by_probability.R")
	
}
print(readinteger())
