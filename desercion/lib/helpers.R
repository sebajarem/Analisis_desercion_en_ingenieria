#configura subCarpetas segun versionado
if (str_trim(config$versionData) != ""){
  subFolderName <- paste0(config$versionData , ifelse(str_trim(config$versionDataNro) == '', '', paste0('_', config$versionDataNro)))
  subFolderIncluidos = str_trim(unlist(strsplit(config$subFoldersIncludes, split=",")))
  for (carpeta in subFolderIncluidos){
    if (dir.exists(carpeta)){
      directorio <- paste0(carpeta, "/", subFolderName)
      config[carpeta] <- paste0(directorio, "/")
      if (!dir.exists(directorio)){
        dir.create(directorio)
      }
    }
  }
  rm('subFolderName')
  rm('subFolderIncluidos')
  rm('directorio')
  rm('carpeta')
}


helper.function <- function()
{
  return(1)
}
