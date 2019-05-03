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

polinomial_aproximation <- function(p, xin, yin, verb = FALSE, plot = FALSE, lambda = 0.1){
    #xin: entrada
    #yin: saída que se deseja mapear

    H <- H_init(xin, p)
    p <- p + 1
    A <-  t(H) %*% H + diag(p) * lambda
    
    w <- solve(A) %*% t(H) %*% yin
    y_hat <- H %*% w #função aproximadora
    
    if(verb == TRUE){
        print(paste('Error: ', t(yin - y_hat) %*% (yin - y_hat)))
        print(paste('Degree:', p - 1))
    }
    
    if(plot == TRUE){
        plot(x = xin, y = yin,  xlab = 'Input', ylab = 'Output', pch = 6)
        lines(x = xin, y = y_hat, col = 'red', lty = 3, lwd = 2.3)
        legend('topright', c('Aproximation', 'Samples'), lty = c(3, -1), pch = c(-1, 6), col = c('red', 'black'), lwd = c(1.5, 1))
    }

    list(pesos = w, aprox = y_hat)
}


tseq <- 1:103
test <- log10(tseq)

plot(y=test, x=tseq)

polinomial_aproximation(8, tseq, test, verb = TRUE, plot = TRUE)