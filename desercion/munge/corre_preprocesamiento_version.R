# corre los archivos que esten en la carpeta munge que corresponda segun
# los parametros de en './lib/globals.R' y './config/global.dcf'

if(str_trim(config$versionData) != ""){
  subFolderName <- paste0(config$versionData , ifelse(str_trim(config$versionDataNro) == '', '', paste0('_', config$versionDataNro)))
  # si existe la carpeta, correr todos los scripts que hay ahi
  path_munge_version = paste0("./munge/",subFolderName)
  if(dir.exists(path_munge_version)){
    l = list.files(path_munge_version)
    # si la lista esta vacia, responder que no hay scripts
    # si hay .R correrlos con source, si hay .Rmd correrlos con rendermarkdown (o algo asi)
    l = l[str_ends(l, pattern = ".R") | str_ends(l, pattern = ".Rmd")]
    if(!(length(l) ==0)){
      for(i in 1:length(l)){
        # pregunto si es script *.R o *.Rmd
        if(str_ends(l[i], pattern = ".R")){
          source(paste0(path_munge_version,"/",l[i]))
        }else{
          if(str_detect(l[i], pattern = ".Rmd")){
            rmarkdown::render( input = paste0(path_munge_version,"/",l[i]),
                               output_dir = paste0("./reports/",subFolderName),
                               output_format = c("html_notebook", "html_document"))# se tiene que respetar el orden. primero el html_notebook y dsps el html_document
          }
        }
      }
    }else{
      print(paste("no hay archivos en la carpeta", glue("{path_munge_version}")))
    }
    
  }else{
    print("no exite la carpeta munge de esta version, por favor ver configuracion o hacer load.project para crear las subcarpetas primero")
  }
  
  
}else{
  print("por favor especifique la version que desea correr")
}
