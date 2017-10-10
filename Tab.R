# Tab, like in STATA 
# Works best if set main dataset to tab.data by running:
#  tab.data <- [name of maing dataset]
tab <- function(x, y=NULL, data=tab.data, m=FALSE){
  if (m==FALSE){
    if (is.null(y)){
      d <- data[!is.na(data[,c(deparse(substitute(x)))]),]
      t1 <- table(d[,c(deparse(substitute(x)))])
      m <- round(prop.table(t(t1), 1)*100,1)
      t <- rbind(cbind(t1,t(m)), c(sum(t1), 100))
      colnames(t) <- c('Freq', 'Perc.')
      rownames(t) <- c(names(t1), 'Total')
      return(t)
    }else{
      d <- data[!is.na(data[,c(deparse(substitute(x)))]) & 
                  !is.na(data[,c(deparse(substitute(y)))]),]
      t1 <- table(d[,c(deparse(substitute(x)))])
      m <- prop.table(t(t1), 1)*100
      t2 <- table(d[,c(deparse(substitute(y)))])
      n <- prop.table(t(t2), 1)*100
      t <- table(d[,c(deparse(substitute(x)))], d[,c(deparse(substitute(y)))])
      t <- round(rbind(cbind(t, t1, t(m)), c(t2, sum(t2), NA), c(n, NA, 100)), 0)
      colnames(t) <- c(names(t2), 'Row', 'Col%')
      rownames(t) <- c(names(t1), 'Col', 'Row%')
      return(t)
    }
  }else{
    if (is.null(y)){
      d <- data
      t1 <- table(d[,c(deparse(substitute(x)))], exclude=NULL)
      m <- round(prop.table(t(t1), 1)*100,1)
      t <- rbind(cbind(t1,t(m)), c(sum(t1), 100))
      colnames(t) <- c('Freq', 'Perc.')
      rownames(t) <- c(names(t1), 'Total')
      return(t)
    }else{
      d <- data
      t1 <- table(d[,c(deparse(substitute(x)))], exclude=NULL)
      m <- prop.table(t(t1), 1)*100
      t2 <- table(d[,c(deparse(substitute(y)))], exclude=NULL)
      n <- prop.table(t(t2), 1)*100
      t <- table(d[,c(deparse(substitute(x)))], d[,c(deparse(substitute(y)))], exclude=NULL)
      t <- round(rbind(cbind(t, t1, t(m)), c(t2, sum(t2), NA), c(n, NA, 100)), 0)
      colnames(t) <- c(names(t2), 'Row', 'Col%')
      rownames(t) <- c(names(t1), 'Col', 'Row%')
      return(t)
    }
  }