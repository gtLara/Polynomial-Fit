#Implementação de um aproximador linear (polinomial) de uma função 
#recebe uma sequência de entradas e saídas, retorna uma lista de pesos que 
#representam os coeficientes de um polinômio de grau P, que aproxima
#o mapeamento da entrada na saída
library('corpcor')

H_init <- function(x, p){
    h <- cbind(x^1, 1)
    if(p < 2){
        h
    }else{
        for(i in 2:p){
            h <- cbind(x^i, h)
        }
        h
    }
}

polinomial_aproximation <- function(xin, yin, verb = FALSE, plot = FALSE, degree = 0, dlim = 100, tol = 1e-10){
    
    if(degree == 0){
        
        e_min <- Inf
        
        for(p in 1:dlim){
            
            H <- H_init(xin, p)
            
            w <- pseudoinverse(H) %*% yin
            
            y_hat <- H %*% w 
  
            e <- t(yin - y_hat) %*% (yin - y_hat)
            
            if(e < e_min){
                e_min <- e
                p_min <- p
            }
            if(e <= tol){
                break
            }
        }
        print(p)
        
        degree <- p_min
    }
    
    H <- H_init(xin, degree)
    
    w <- pseudoinverse(H) %*% yin
    
    y_hat <- H %*% w 
    
    r <- max(abs(yin))
    
    error <- (t(yin - y_hat)/r) %*% ((yin - y_hat)/r)
    
    if(error > 0.01){
        print("Error too large, generator function may not be polynomial. Visualization through 'plot = TRUE' is recommended.")
    }
    
    if(verb == TRUE){
        print(paste('Error: ', error))
        print(paste('Degree:', degree))
    }
    
    if(plot == TRUE){
        plot(x = xin, y = yin,  xlab = 'Input', ylab = 'Output', pch = 6)
        lines(x = xin, y = y_hat, col = 'red', lty = 3, lwd = 2.3)
        legend('topright', c('Aproximation', 'Samples'), lty = c(3, -1), pch = c(-1, 6), col = c('red', 'black'), lwd = c(1.5, 1))
    }

    list(coefficients = w, aproximation = y_hat)
}


tseq <- 1:100
test <- (tseq)^2

polinomial_aproximation(tseq, test, verb = TRUE, plot = TRUE)