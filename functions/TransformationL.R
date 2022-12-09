TransL <- function(dt) {
  if(deparse(substitute(dt)) != "pop"){
  pivot_longer(
    dt, 
    cols = years_to_evaluate,
    names_to= "Year",
    values_to= deparse(substitute(dt))
  )
  }else{
    pivot_longer(
      dt, 
      cols = years_to_evaluate,
      names_to= "Year",
      values_to= "population"
    )
  }
}




