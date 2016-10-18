#' @import aoos
#' @import methods
#' @importFrom utils installed.packages str
NULL

globalVariables(c("processModule", "into"))

# The central class is built on S3 to have the public interface minimalistic and
# to not enforce S4.
setOldClass(c("module", "list"))

setOldClass("ModuleConst", "list")
