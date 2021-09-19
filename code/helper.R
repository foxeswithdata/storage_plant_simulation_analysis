ci_lower <- function(dat){
  ci = MeanCI(dat, conf.level=0.95)
  return(ci["lwr.ci"])
}

ci_upper <- function(dat){
  ci = MeanCI(dat, conf.level=0.95)
  return(ci["upr.ci"])
}
