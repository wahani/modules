#' @import aoos
#' @import methods
NULL

globalVariables(c("processModule", "into"))

# This may be needed if users want to use modfun type as argument to S4 methods.
setOldClass(c("modfun", "function"))

# The central class is built on S3 to have the public interface minimalistic and
# to not enforce S4.
setOldClass(c("module", "list"))

setOldClass("ModuleConst", "list")
