R Programming
# vgacutan

options(expressions=500000)

#2. Log Gamma (Loop)
log_gamma_loop = function(n){
  x=0
  for (i in (n-1):1)
  {
    x = x + log(i)
  }
  return(x)
}

#3 Log Gamma (Recursive)
log_gamma_recursive = function(n){
  #x=0
  if (n <= 1) {
    return(0)
  } else {
    return(log(n-1) + log_gamma_recursive(n-1))
  }
}


#4.a Sum of Log Gamma
sum_log_gamma_loop = function(z){
    x=0
    for (i in 2:z){
    x = x + log_gamma_loop(i)
    }
    return(x)
}

#4.b Sum of Log Gamma recursive
sum_log_gamma_recursive = function(n){
  x=0
  if (n<1)  {
    return(0)
  } else {
    
    #gval = c(gval, log_gamma_recursive(n))
    #return(sum_log_gamma_recursive2(log_gamma_recursive(n)) + sum_log_gamma_recursive2(log_gamma_recursive(n-1)))
    
    #return(log_gamma_recursive(n+1) + sum_log_gamma_recursive2(log_gamma_recursive(n-1)))
    x = x + log_gamma_recursive(n)
    sum_log_gamma_recursive2(n-1)
    
  }
  return(sum(x))
}


#5. code to call 3 functions
df = function(q){
  for (i in seq(10, q, length = 20)) #seq(1, q, length = 20)
  {
    #print(i)
    x = system.time(sum_lgamma(i))
    y = system.time(sum_log_gamma_loop(i))
    z = system.time(sum_log_gamma_recursive2(i))
    x1 = x['user.self']
    y1 = y['user.self']
    z1 = z['user.self']
    #print(z)
    #print(cat(as.integer(i),y1))
    #print(cat(as.integer(i), y1))
    #writeLines(strwrap(paste(as.integer(i),as.numeric(y1) )))# as.numeric(y1), as.numeric(z1))))
    #writeLines(strwrap(paste(as.integer(i), ",", as.numeric(x1), ",",as.numeric(y1))))#, as.numeric(z1))))
    #writeLines(strwrap(paste(as.integer(i), ",",as.numeric(z1))))
    writeLines(strwrap(paste(as.integer(i), as.numeric(x1),as.numeric(y1),as.numeric(z1))))
    #reslt = data.frame(i,x1,y1,z1)
    #print(reslt)
  }
}

#5 Compare results to Built-in Functions
sum_lgamma = function(n){
  x=0
  for (i in 1:n)
  {
    x = x + lgamma(i)
  }
  return(x)
}

