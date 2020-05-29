
buffer_conc <- function(pH,pka,buffer_conc){
  eq1 <- c(10^(pH-pka),-1)
  eq2 <- c(1,1)
  matrix1 <- rbind(eq1,eq2)
  matrix2 <- matrix(c(0,buffer_conc),2,1)
  concs <- solve(matrix1,matrix2)
  concentrations <- as.vector(concs)
  names(concentrations) <- c("acid concentration" , "base concentration")
} + return(concentrations)



SA_vol <- function(pH,pka,buffer_conc,SA_conc,vol_sol){
  eq1 <- c(10^(pH-pka),-1)
  eq2 <- c(1,1)
  matrix1 <- rbind(eq1,eq2)
  matrix2 <- matrix(c(0,buffer_conc),2,1)
  concs <- solve(matrix1,matrix2)
  SA_vol_L <-  (concs[1,1] * vol_sol) / SA_conc
  SA_vol_mL <- SA_vol_L * 1000
  names(SA_vol_mL) <- c("strong acid mL")
} + return(SA_vol_mL)