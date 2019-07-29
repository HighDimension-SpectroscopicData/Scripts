
lr <- function(data_train_rp, data_control_rp, data_recovered_rp, variables, m.all.analysed){
  population <- data_train_rp
  data.analysed_control <- data_control_rp
  data.analysed_rec <- data_recovered_rp
  p <- length(variables)
  
  #n <- length(unique(data.analysed$rep))
  
  output.matrix.Nor = matrix(0,ncol = m.all.analysed, nrow = m.all.analysed)
  rownames(output.matrix.Nor) = as.character(unique(data.analysed_control$item))
  colnames(output.matrix.Nor) = as.character(unique(data.analysed_control$item))
  
#  output.matrix.KDE = matrix(0,ncol = m.all.analysed, nrow = m.all.analysed)
#  rownames(output.matrix.KDE) = as.character(unique(data.analysed_control$item))
#  colnames(output.matrix.KDE) = as.character(unique(data.analysed_control$item))
  
  vcov_data_rp <- two.level.components(population, data.columns = variables, item.column = 1)
  
  for(i in 1:m.all.analysed){
    for(j in i:m.all.analysed){
      if(i == j){
        control <- two.level.comparison.items(data.analysed_control[data.analysed_control$item==i,], variables)
        recovered <- two.level.comparison.items(data.analysed_rec[data.analysed_rec$item==j,], variables)
      }
      else{
        control <- two.level.comparison.items(data.analysed_control[data.analysed_control$item==i,], variables)
        recovered <- two.level.comparison.items(data.analysed_rec[data.analysed_rec$item==j,], variables)
      }
      
      LR.Nor <- two.level.normal.LR(control, recovered, vcov_data_rp)
#      LR.KDE <- two.level.density.LR(control, recovered, vcov_data_rp)
      
      output.matrix.Nor[j,i] = LR.Nor
#      output.matrix.KDE[j,i] = LR.KDE
      
    }
  }
  
#  output <- list("output.matrix.Nor" = output.matrix.Nor, "output.matrix.KDE" = output.matrix.KDE)

  output <- output.matrix.Nor
  
  return(output)
}