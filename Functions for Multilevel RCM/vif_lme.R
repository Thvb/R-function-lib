############################################
#Variance inflation factor function for LME#
############################################

vif.lme <- function (fit) {
  ## adapted from rms::vif
  v <- vcov(fit)
  if(class(fit)=="lme"){
    nam <- names(fixef(fit))
  }else{
    nam <- names(coef(fit))
  }
  ## exclude intercepts
  ns <- sum(1 * (nam == "Intercept" | nam == "(Intercept)"))
  if (ns > 0) {
    v <- v[-(1:ns), -(1:ns), drop = FALSE]
    nam <- nam[-(1:ns)] }
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v }