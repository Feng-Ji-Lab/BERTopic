#' SMS Spam Collection (UCI) - subset for examples
#'
#' A cleaned subset of the UCI SMS Spam Collection, suitable for quick examples
#' and tests in this package. Each row is an SMS message labeled as "ham" or "spam".
#'
#' @format A data frame with two columns:
#' \describe{
#'   \item{label}{Character, either "ham" or "spam".}
#'   \item{text}{Character, the SMS message content (UTF-8).}
#' }
#'
#' @source UCI Machine Learning Repository: SMS Spam Collection.
#'   Dataset page: \url{https://archive.ics.uci.edu/dataset/228/sms+spam+collection}
#'   Original citation: Almeida, T.A., Hidalgo, J.M.G., & Yamakami, A. (2011).
#'   Contributions to the Study of SMS Spam Filtering: New Collection and Results.
#'
#' @note This dataset is included for educational/demo purposes. If you use it in
#'   publications, please cite the original authors and the UCI repository page.
#'
#' @examples
#' data(sms_spam)
#' head(sms_spam)
"sms_spam"
