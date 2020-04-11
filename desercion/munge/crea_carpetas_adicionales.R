# crea carpeta tablas en docs

if(str_trim(config$versionData) != ""){
  subFolderName <- paste0(config$versionData , ifelse(str_trim(config$versionDataNro) == '', '', paste0('_', config$versionDataNro)))
  # si existe la carpeta, correr todos los scripts que hay ahi
  path_docs_version = paste0("./docs/",subFolderName)
  path_docs_version_tablas = paste0(path_docs_version, "/", "tablas")
  if (!dir.exists(path_docs_version_tablas)){
    dir.create(path_docs_version_tablas)
  }
}

