st_rbind_all <- function(a,b) {

  cols.to.add.to.b <- setdiff(names(a),names(b))
  cols.to.remove.from.b <- setdiff(names(b),names(a))
  # add them
  b[,cols.to.add.to.b] <- NA
  # remove them
  b <- b %>%
    select(-one_of(cols.to.remove.from.b))
  # make sure we're only using the same columns from d.foc.yr
  a <- a[,names(b)]
  # put the columns in the same order
  b <- b[,names(a)]
  # combine them
  combined <- rbind(a,b)

  return(combined)
}

