# Install dependencies in Linux
# sudo apt-get install libcurl4-openssl-dev 
options(download.file.method = "libcurl")

include <- function(packages){
	for(pkg in packages){
		# Si ya está instalado, no lo instala.
		if(!require(pkg, character.only = T)){
			install.packages(pkg, repos = "https://cran.rstudio.com", dependencies = T)
			if(!require(pkg, character.only = T)) 
				stop(paste("load failure:", pkg))
		}
		library(pkg, character.only = T)
	}
}