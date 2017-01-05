debugSource("aspirador/Aspirador.R")
debugSource("buscaDesinformada.R")
debugSource("buscaInformada.R")

grid_inicial <- matrix(c(1, 1, 0, 1), nrow=2, ncol=2, byrow = TRUE) 
inicial <- Aspirador(desc = list(P = c(1,1), G = grid_inicial))

grid_final <- matrix(0L, nrow = dim(grid_inicial)[1], ncol = dim(grid_inicial)[2])
objetivo <- Aspirador(desc = list(G = grid_final))

cat("====\tBusca em Largura\t====\n")
print(unlist(buscaEmLargura(inicial, objetivo)))

cat("====\tBusca em Profundidade\t=====\n")
print(buscaEmProfundidade(inicial, objetivo))

cat("====\tBusca de Custo Uniforme\t=====\n")
print(buscaCustoUniforme(inicial, objetivo))

cat("====\tBusca Best-First (Gulosa)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "Gulosa"))

cat("====\tBusca Best-First (A*)\t=====\n")
print(buscaBestFirst(inicial, objetivo, "AEstrela"))