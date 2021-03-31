
## Load comp vision model

library(reticulate)

use_condaenv(condaenv = "tfpose", required = TRUE)
source_python("tf/pokepred.py")