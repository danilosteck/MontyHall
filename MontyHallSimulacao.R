# ---
#   title: "MontyHall"
#   author: "Danilo Steckelberg"
#   date: "8/31/2020"
#---


#### Função que simula um jogo de Monty Hall. ####
MontyHall <- function(simulacoes = 100, nPortas = 3, modeloTroca = "aleatorio"){
  for(i in 1:simulacoes){
    Portas <- c(1:nPortas)
    Premio <- sample(Portas,1)
    Escolha <- sample(Portas,1)
    
    # Quais portas estão disponíveis para serem abertas: excluir a porta com prêmio e a porta escolhida.
    PortasDisponiveis <- Portas[-c(Premio,Escolha)]
    
    # Se apenas uma porta for disponível, escolher esta porta. Se duas portas estiverem disponíveis, 
    # escolher aleatoriamente entre as duas.
    if(length(PortasDisponiveis) == 1){
      PortasReveladas <- PortasDisponiveis
    }else{
      PortasReveladas <- sample(PortasDisponiveis,(nPortas-2))
    }
    
    # Portas finais que ficam disponíveis para o participante escolher
    PortasFinais <- Portas[-c(PortasReveladas)]
    
    # Modelos de troca para o participante: 
    # 1) "troca" ele sempre trocará; 
    # 2) "nao_troca" ele nunca trocará; 
    # 3) "aleatorio" ele troca de acordo com uma probabilidade aleatoria.
    
    if(modeloTroca == "aleatorio"){
      Troca <- ifelse(runif(1)>=0.5,1,0)
    }
    if(modeloTroca == "troca"){
      Troca <- 1
    }
    if(modeloTroca == "nao_troca"){
      Troca <- 0
    }
    
    
    # Rotina para troca ou não troca das portas.
    if(Troca == 1){
      PortaEscolhida <- PortasFinais[PortasFinais != Escolha]
    }else{
      PortaEscolhida <- PortasFinais[PortasFinais == Escolha]
    }
    
    if(PortaEscolhida == Premio){Vitoria = 1}else{Vitoria = 0}
    
    if(i == 1){
      Resultados <- data.frame(EscolhaInicial = Escolha, EscolhaFinal = PortaEscolhida, Premio = Premio, Troca = Troca, Vitoria = Vitoria)
    }else{
      Resultados <- rbind(Resultados,data.frame(EscolhaInicial = Escolha, EscolhaFinal = PortaEscolhida, Premio = Premio, Troca = Troca, Vitoria = Vitoria))
    }
  }
  return(Resultados)
}

#### Criação de tabela para comparar vitórias com as trocas de porta. ####
nsim <- 1000
nportas <- 3
troca <- "aleatorio"

if(troca %in% c("troca", "aleatorio","nao_troca")){
  Resultados <- MontyHall(simulacoes = nsim, nPortas = nportas, modeloTroca = troca)
  
  tabResultados <- table(Resultados$Vitoria,Resultados$Troca,dnn = c("Vitoria","Troca"))
  
  totalTrocas <- tabResultados[3]+tabResultados[4]
  totalNaoTrocas <- tabResultados[1]+tabResultados[2]
  
  par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
  barplot(tabResultados, 
          col = c(1,2),
          xlab = "Troca", ylab = "Simulações",
          main = paste("Distribuição de resultados de Monty Hall para\n",nportas,"portas e",nsim,"simulações."))
  text(x = 0.15, y = 0.85*(totalNaoTrocas), pos = 4, col = "white",cex = 0.8,
       labels = paste(tabResultados[2], " casos\n(",round(tabResultados[2]/totalNaoTrocas,3)*100,"%)",sep =""))
  
  text(x = 1.3, y = 0.85*(totalTrocas),pos = 4, col = "white",cex = 0.8, 
       labels = paste(tabResultados[4], " casos\n(",round(tabResultados[4]/totalTrocas,3)*100,"%)",sep =""))
  
  text(x = 0.15, y = 0.1*(totalNaoTrocas), pos = 4, col = "white",cex = 0.8,
       labels = paste(tabResultados[1], " casos\n(",round(tabResultados[1]/totalNaoTrocas,3)*100,"%)",sep =""))
  
  text(x = 1.3, y = 0.1*(totalNaoTrocas), pos = 4, col = "white",cex = 0.8,
       labels = paste(tabResultados[3], " casos\n(",round(tabResultados[3]/totalTrocas,3)*100,"%)",sep =""))
  
  legend("topright",col = c(1,2),legend = c("Perda", "Vitoria"),inset = c(-0.5,0), pch = 15, cex = 0.8)
  
}else{
  warning("Por favor escolha um dos seguintes modelos: aleatorio, troca ou nao_troca. Se nenhum modelo for escolhido, o modelo aleatório será utilizado")
}
# FIM!
