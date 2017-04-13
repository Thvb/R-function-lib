train_models2 <- function (container, algorithms, ...) 
{
  result = list()
  for (algorithm in algorithms) {
    model = train_model2(container, algorithm, ...)
    result[[algorithm]] = model
  }
  return(result)
}
