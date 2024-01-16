# NOSH

## Development
## Deployment
Currently, the app is only on enterprise github. In order to have rconnect able to install the package, it must first be released to rspm.  The rspm repo is then listed as the primary repo, the app is installed, and the manifest can be written

Shiny apps structured as a package do not return a shiny object when deployed directly, resulting in an error on rsconnect/posit connect.  Our solution is to include a directory (`deploy`) with a simple app.R script sourcing this package and running the app function.  writeManifest is done on that directory.  


```
options(repos = c("msk"="http://rspm.mskcc.org/MSKREPO/latest", "CRAN"="https://cran.rstudio.com"));  install.packages("dietapp")
rsconnect::writeManifest(appDir = "deploy")

```