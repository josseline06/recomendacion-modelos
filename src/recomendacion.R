#	1. Requerimientos de plataforma
# -----------------------------------------------------
source("src/utilities.R")
include(c("arules", "arulesViz"))

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

# - Tipos de Usuario
u_transactions <- unique(transactions)
small_sample <- sample(u_transactions, 10000)

# Calculando similaridad entre itemsets
d_jaccard <- dissimilarity(small_sample)
d_jaccard[is.na(d_jaccard)] <- 1 # Eliminando NA

# Calculando clusters
hclusters <- hclust(d_jaccard, method = "ward")
# Dendrograma
pdf(file = "similarity.pdf", width = 150)
plot(hclusters, cex = 0.5)
dev.off()

# Haciendo corte
cutting <- cutree(hclusters, k = 8)

# Descartando transacciones pertenecientes a small_sample
big_sample <- u_transactions[!(u_transactions %in% small_sample)]
# Prediciendo
pred_labels <- predict(small_sample, big_sample, cutting) 

# Resultados
print(table(cutting))
print(table(pred_labels))

#	- Generando reglas:
rules <- apriori(transactions, 
                 parameter = list(sup = 0.00003, 
                                  conf = 0.65, 
                                  target="rules"))
# Ordenando reglas por confianza
rules <- sort(rules, by = "confidence", decreasing = T)

# Viendo reglas
plot(rules, method ="grouped", control = list(k = 50))

# - Recomendador
recommend <- function(itemset, rules){
  suitableRules <- subset(rules,lhs %ain% itemset & !rhs %in% itemset)
  # Si no se encuentra transacción, entonces buscar un subconjunto
  if(length(suitableRules) == 0)
    suitableRules <- subset(rules, lhs %in% itemset & !rhs %in% itemset)
  
  return(inspect(suitableRules@rhs[1]))
}

# Probando
example <- c("deportes/articulo1",     
             "deportes/articulo2",     
             "deportes/articulo3")

recommend_out <- recommend(example,rules)


#	10 visitas con mayor y menor tiempo 
by_time <- order(dataset$time, decreasing = T) 
head(dataset[by_time,], n = 10)
tail(dataset[by_time,], n = 10)

#	10 transacciones más frecuentes
freq_itemsets <- apriori(transactions, parameter = list(sup = 0.00003, target = "frequent"))
inspect(head(sort(freq_itemsets, decreasing = T), n = 10))