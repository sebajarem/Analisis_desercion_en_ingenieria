
describe_variable_tipo = function(df){
  
  df.describe = data_frame(
    variable = colnames(df),
    tipo = apply(df, MARGIN = 2, FUN = typeof)
  )
  
  return(df.describe)
}
