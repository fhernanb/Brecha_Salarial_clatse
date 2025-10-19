est_param_2 <- function (mod, fun = "mean", m = 100, 
                         data, newdata = NULL, 
          ...) 
{
  stopifnot(class(mod) == "gamlss2")
  if (is.null(newdata)) {
    fit <- predict(mod)
    #fit
  }
  else {
    fit <- NULL
    for (i in mod$family$names) {
      temp <- predict(mod, model = i, data = data, newdata = newdata, 
                      type = "parameter")
      fit <- cbind(fit, temp)
    }
  }
  generator <- function(x) {
    aux <- paste0("r", mod$family$family, "(n=m, ", paste(x, 
                                                          sep = "", collapse = ", "), ")")
    eval(parse(text = aux))
  }
  x <- t(apply(fit, 1, generator))
  return(apply(x, 1, fun, ...))
}
