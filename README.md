# NOSH

## Deployment
We recommend deploying this app via Posit Connect:

1. make a git repo with the name of your NOSH instance.  this allows you to use git-backed deployment
2. create a simple app script in `app/app.R` containing:
```
library(NOSH)
NOSH()
```
3. create the manifest.json file needed for Posit connect.  We like to do that as follows:
  
  a.  initiate renv with `renv::init()`
  b. install the desired version of NOSH with remotes/devtools: `remotes::install_github("mskcc-microbiome/NOSH", ref = "33f4d3d7a1ea95ef4c5ad4de9c933a91c4b5170b")`
  c. create the manifest with `rsconnect::writeManifest("app")`
  d. commit the renv.lock, app.R, and the manifest; push the changes



## Development

You need to have the following environemnt variables defined.  We use `.env` files to help with this
```
UNITTABLE_REDCAP_URI=<redcap base url, such as https://redcap.acme.org/api/ >
UNITTABLE_REDCAP_TOKEN=<your redcap unittable api token>
DIETDATA_REDCAP_URI=<redcap base url, such as https://redcap.acme.org/api/ >
DIETDATA_REDCAP_TOKEN==<your redcap unittable api token>
NOSH_USER_TYPE=<BASIC,ADMIN,DEV>
```
`NOSH_USER_TYPE` determines which tabs are visible and the level of logging available
