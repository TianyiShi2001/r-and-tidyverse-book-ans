{
loadedOnly_base <- names(sessionInfo()$loadedOnly)
if (length(loadedOnly_base) > 15) stop("请完全关闭R并重新启动。")
if (require("tidyverse") == FALSE) stop("你还没有安装tidyverse.")

library(tidyverse)

loadedOnly_all <- names(sessionInfo()$loadedOnly)

new <- !(loadedOnly_all %in% loadedOnly_base)

loadedOnly_tidyverse <- loadedOnly_all[new]

attached_tidyverse <- names(sessionInfo()$otherPkgs)

tidyverse_packages <- c(attached_tidyverse, loadedOnly_tidyverse)
}

tidyverse_packages
