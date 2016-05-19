#	1. Requerimientos de plataforma
# -----------------------------------------------------
source("src/utilities.R")
utilities(c("arules", "arulesViz"))

#	2. Carga de datos
# -----------------------------------------------------
dataset <- read.csv2("dat/periodico.csv", header = T,
					 sep = ",",
					 colClasses = "character", 
					 nrows = 131300,
					 comment.char = "",
					 stringsAsFactors = F)
#	Temas de los artículos del periódico
subjects <- c("deportes", "politica", 
		      "variedades", "internacional", 
		      "nacionales", "sucesos", 
		      "comunidad", "negocios", 
		      "opinion")
colnames(dataset)

#	3. Preprocesamiento y preparación de los datos
# -----------------------------------------------------
# 	- Transformando columna X de character a integer:
dataset$X <- as.integer(dataset$X)

# 	- Transformando columnas entry y exit:
dataset$entry <- strptime(dataset$entry, "%Y-%m-%d %H:%M:%S")
dataset$exit <- strptime(dataset$exit, "%Y-%m-%d %H:%M:%S")

dataset$time <- difftime(dataset$exit, dataset$entry, unit = "secs")

# 	Hay 2 columnas que pueden ser el id de las transacciones: ID y X,
#	cada transacción (a pesar de tener el mismo valor) son entradas distintas
#	del dataset, por lo que de acuerdo al contexto del problema (Web log) no 
#	deben haber entradas repetidas (con mismo id)
rows <- nrow(dataset)
nrow(unique(dataset[,c("entry","exit","articles")]))

length(unique(dataset$ID)) == rows
length(unique(dataset$X)) == rows
#	Por lo que podemos concluir que X es la columna de id de transacciones
colnames(dataset)[1] <- "tid"

#	- Removiendo columnas que no se usaran:
dataset$ID <- NULL
dataset$entry <- NULL
dataset$exit <- NULL

#	- Transformando columna articles:
#	Transformando de item<N> a <subject>/articulo<n>:
items_id <- seq(1, 81)
items <- sprintf("%s/articulo%d", subjects[(items_id-1)%/%9+1], (items_id-1)%%9+1)
for(i in items_id)	dataset$articles <- gsub(sprintf("item%d(?=[,}])", i), 
											 items[i], 
											 dataset$articles, 
											 perl = T)
#	Eliminando {}
dataset$articles <- gsub("\\{|\\}", "", dataset$articles)
#	Cada transaccion como vector
transactions <- strsplit(dataset$articles, ",")

#	4. Requerimientos de negocio
# -----------------------------------------------------
#	- Caso transacciones bot:
tol <- 20 # Cantidad de segundos mínima
bots <- dataset$time <= (lengths(transactions)*tol)

# 	Cantidad de transacciones de bots:
nrow(dataset[bots,])
length(unique(dataset[bots, "articles"]))

#	Eliminando transacciones de bots:
dataset <- dataset[bots == F,]
transactions <- transactions[bots == F]

#	- Cargando transacciones:
names(transactions) <- dataset$tid
transactions <- as(transactions, "transactions")

#	- Buscando tipos de usuarios:

#	10 visitas con mayor y menor tiempo 
by_time <- order(dataset$time, decreasing = T) 
head(dataset[by_time,], n = 10)
tail(dataset[by_time,], n = 10)

#	10 transacciones con mayor cantidad de apariciones
sort(dataset$articles, decreasing = T)[1:10]