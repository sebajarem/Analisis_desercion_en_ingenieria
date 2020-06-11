# Add any project specific configuration here.
add.config(
  apply.override = FALSE
)

# Add project specific configuration that can be overridden from load.project()
add.config(
  apply.override = TRUE,
  data = "data/",
  outputs = "outputs/",
  graphs = "graphs/",
  reports = "reports/",
  diagnostics = "diagnostics/",
  tests = "tests/",
  cache = "cache/",
  
  versionData="0",
  versionDataNro="1",
  subFoldersIncludes = "data, outputs, graphs, reports, diagnostics, docs, tests, munge, src",
  logFileName="project.log"
)

add.config(
  apply.override = T,
  
  path_docs_version_tablas = paste0("./docs/",
                                    paste0(config$versionData , ifelse(str_trim(config$versionDataNro) == '', '', paste0('_', config$versionDataNro))),
                                    "/",
                                    "tablas")
)
