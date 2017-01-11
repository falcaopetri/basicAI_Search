source("Estado.R")

## Classe e métodos para o problema do Mundo do Aspirador de Pó
Aspirador <- function(desc = NULL, pai = NULL){

  e <- environment()
  
  assign("desc", desc, envir = e)
  assign("pai", pai, envir = e)
  assign("g", 0, envir = e)
  assign("h", Inf, envir = e)
  assign("f", Inf, envir = e)
  
  class(e) <- c("Aspirador", "Estado")

  return(e)
}

## Sobrecarregando o operador "==" para comparação entre estados
Ops.Aspirador = function(obj1,obj2){
  if(.Generic == "=="){
    if (is.null(obj2[["P"]])) {
      return(all(obj1$desc$G == obj2$desc$G))
    }
    else {
      return(all(obj1$desc == obj2$desc))
    }
  }
}

## Sobrecarga da função genérica "print" do R
print.Aspirador <- function(obj) {
  cat("(P G): (", obj$desc$P, ", ", obj$desc$G, ")\n")
  cat("G(n): ", obj$g, "\n")
  cat("H(n): ", obj$h, "\n")
  cat("F(n): ", obj$f, "\n")
}

## Sobrecarga da função genérica "heuristica", definida por Estado.R
heuristica.Aspirador <- function(atual){
  
  if(is.null(atual$desc$G))
    return(Inf)
  ## h(obj) = soma das posições sujas no grid G
  return(sum(atual$desc$G))
}

geraFilhos.Aspirador <- function(obj) {
  
  filhos <- list()
  filhosDesc <- list()

  desc <- obj$desc
  desc$C <- 0
  
  ## gera filhos usando todos os operadores  
  
  cleared_pos_g <- desc$G
  cleared_pos_g[desc$P[1], desc$P[2]] <- 0
  # 1) aspirar sujeira do quadrado em que se encontra - custo: 2
  # 2) mover para o quadrado da direita               - custo: 1
  # 3) mover para o quadrado de baixo                 - custo: 3
  # 4) mover para o quadrado da esquerda              - custo: 1
  # 5) mover para o quadrado de cima.                 - custo: 3
  operadores <- list( 
                      list(P = c( 0, 0), G = cleared_pos_g, C = 2), # limpa
                      list(P = c( 0, 1), G = desc$G, C = 1), # dir
                      list(P = c( 1, 0), G = desc$G, C = 3), # baixo
                      list(P = c( 0,-1), G = desc$G, C = 1), # esq
                      list(P = c(-1, 0), G = desc$G, C = 3)  # cima
                    )
  
  filhosDesc <- lapply(operadores, function(op) list(P = desc$P + op$P, 
                                                     G = op$G,
                                                     C = op$C)
  )
  
  ## verifica estados filhos incompatíveis com o problema  
  incompativeis <- sapply(1:length(filhosDesc),
                    function(i) {
                      fDesc <- filhosDesc[[i]]
                      if((any(fDesc$P < 1)) ||
                         (fDesc$P[1] > dim(desc$G)[1]) ||
                         (fDesc$P[2] > dim(desc$G)[2]))
                        i ## é incompatível: retorna índice
                      else
                        0 ## senão é compatível
                    })
  
  ## mantém no vetor apenas os que são incompatíveis
  incompativeis <- incompativeis[incompativeis != 0]
  
  ## remove estados filhos incompatíveis
  filhosDesc <- filhosDesc[-incompativeis]
  
  #print("filhos:")
  #print(filhosDesc)
  
  ## gera os objetos Aspirador para os filhos
  for(filhoDesc in filhosDesc){
    filho <- Aspirador(desc = list(P = filhoDesc$P, G = filhoDesc$G), pai = obj)
    filho$h <- heuristica(filho)
    filho$g <- obj$g + filhoDesc$C
    #filho$f <- filho$h + filho$g
    filhos <- c(filhos, list(filho))
  }
  
  return(filhos)
}