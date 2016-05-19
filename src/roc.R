generate_ROC <- function (scores, real, target){
	data <- data.frame(y = real, score = scores)
	# Ordenando por score
	data <- data[with(data, order(score, decreasing = T)),]
	# Matriz de confusión, columna de valores positivos 
	p_confusion <- setNames(c(0,0), c("TP", "FP"))
	# Cantidad total de valores de la clase P y N
	target_entries <- data$y == target
	real_v <- setNames(c(length(data$y[target_entries]),
	                     length(data$y[!target_entries])),
	                   c("P", "N"))
	prev <- Inf
	roc_curve <- data.frame(x = double(), y = numeric(), score = numeric())
	indices <- seq(1, nrow(data))
	
	for(i in indices){
		if(data$score[i] != prev){
			roc_curve[nrow(roc_curve)+1,] <- c(p_confusion["FP"]/real_v["N"], p_confusion["TP"]/real_v["P"],prev)
			prev <- data$score[i]
		}
		if(data$y[i] == target)
			p_confusion["TP"] <- p_confusion["TP"] + 1
		else
			p_confusion["FP"] <- p_confusion["FP"] + 1
	}
	# Insertando último punto (1,1)
	roc_curve[nrow(roc_curve)+1,] <- c(p_confusion["FP"]/real_v["N"], p_confusion["TP"]/real_v["P"],prev)
	
	return(roc_curve)
}

y = c(2, 2, 1, 2, 2, 2, 2, 1, 2, 1, 2, 1, 2, 1, 1, 1, 2, 1, 1, 1)
scores = c(0.9, 0.8, 0.7, 0.6, 0.55, 0.54, 0.53, 0.52, 0.5, 0.5, 0.5, 0.5, 0.38, 0.37, 0.36, 0.35, 0.34, 0.33, 0.30, 0.1)
target = 2

curve <- generate_ROC(scores, y, target)
# Graficando la curva retornada
plot(curve$x, curve$y, 
     type ="l", 
     lty = 2,
     xlab = "FP-Rate",
     ylab = "TP-Rate",
     main = "ROC Curve")
abline(0, 1, lty = 2, col = "darkgray")
points(curve$x, curve$y, col = "red", pch = 19)
text(curve$x, curve$y, curve$score, cex = 0.85, pos = 4)