# 
# ## Load comp vision model
# 
# library(reticulate)
# if (Sys.info()['user']=='mateograciano'){
#   virtualenv_create(envname = "python_environment") 
# }else{
#   virtualenv_create(envname = "python_environment", python= "python3") 
# }
# 
# virtualenv_install("python_environment", packages = c('keras', 'pandas','numpy','scipy','scikit-learn', 'tensorflow-cpu','pillow'))
# reticulate::use_virtualenv("python_environment", required = TRUE)
# 
# 
# 
# 
# 
# source_python("tf/pokepred.py")


Sys.setenv("GCS_DEFAULT_BUCKET" = "poke-images",
           "GCS_AUTH_FILE" = "auth.json",
           "GCS_PROJ" = "gentle-analyst-307101")

library(googleCloudStorageR)