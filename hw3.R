#Logistic Regression
#vgacutan3

library(ggplot2)
setwd("Users/HW3")
#0. Data Preprocessing
mnist_train= read.csv('mnist_train.csv', header = FALSE)
t_mnist_train = t(mnist_train) # transposed values
xt = subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="0" | t_mnist_train[,ncol(t_mnist_train)]=="1") # "values
train_0_1 = data.matrix(xt[ ,1:784])
true_label_train_0_1 = data.matrix(as.numeric(xt[ ,785]))
cat("train_0_1 has", dim(train_0_1)[1], "rows and", dim(train_0_1)[2], "columns", end="\n", file="")


xt2 = subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="3" | t_mnist_train[,ncol(t_mnist_train)]=="5") # "values"
train_3_5 = data.matrix(xt2[ ,1:784])
true_label_train_3_5 = data.matrix(as.numeric(xt2[ ,785]))
cat("train_3_5 has", dim(train_3_5)[1], "rows and", dim(train_3_5)[2], "columns", end="\n", file="")

mnist_test= read.csv('mnist_test.csv', header = FALSE)
t_mnist_test = t(mnist_test) # transposed values
xtest = subset(t_mnist_test, t_mnist_test[,ncol(t_mnist_test)]=="0" | t_mnist_test[,ncol(t_mnist_test)]=="1") # "values"
test_0_1 = data.matrix(xtest[ ,1:784])
true_label_test_0_1 = data.matrix(as.numeric(xtest[ ,785]))
cat("test_0_1 has", dim(test_0_1)[1], "rows and", dim(test_0_1)[2], "columns", end="\n", file="")

xtest2 = subset(t_mnist_test, t_mnist_test[,ncol(t_mnist_test)]=="3" | t_mnist_test[,ncol(t_mnist_test)]=="5") # "values"
test_3_5 = data.matrix(xtest2[ ,1:784])
true_label_test_3_5 = data.matrix(as.numeric(xtest2[ ,785]))
cat("test_3_5 has", dim(test_3_5)[1], "rows and", dim(test_3_5)[2], "columns", end="\n", file="")

im_0 = t(subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="0"))
im0 = matrix(im_0[1:784, ], nrow=28, ncol=28)
im0_num = apply(im0, 2, as.numeric)
image(1:28, 1:28, im0_num, col=gray((0:255)/255))
cat("train_0 has", dim(im_0)[1], "rows and", dim(im_0)[2], "columns", end="\n", file="")

im_1=t(subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="1"))
im1<-matrix(im_1[1:784, ], nrow=28, ncol=28)
im1_num <- apply(im1, 2, as.numeric)
image(1:28, 1:28, im1_num, col=gray((0:255)/255))
cat("train_1 has", dim(im_1)[1], "rows and", dim(im_1)[2], "columns", end="\n", file="")

im_3=t(subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="3"))
im3<-matrix(im_3[1:784, ], nrow=28, ncol=28)
im3_num <- apply(im3, 2, as.numeric)
image(1:28, 1:28, im3_num, col=gray((0:255)/255))
cat("train_3 has", dim(im_3)[1], "rows and", dim(im_3)[2], "columns", end="\n", file="")


im_5=t(subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="5"))
im5<-matrix(im_5[1:784, ], nrow=28, ncol=28)
im5_num <- apply(im5, 2, as.numeric)
image(1:28, 1:28, im5_num, col=gray((0:255)/255))
cat("train_5 has", dim(im_5)[1], "rows and", dim(im_5)[2], "columns", end="\n", file="")

#2. Implementation
#steps:
#- initialize values of theta ex. theta_1 to theta_n = 0
#- use gradient descent to minimize the objection function J-theta using choosen number of iteration until convergence
#- use the learned/converge values of theta to calculate y  prediction using the hypothesisOfx function.

hypothesisOfx<-function(xval, theta) #function to calculate hyppthesis.
{
  h = 1/(1 + exp(-1*(xval%*%theta)))  # xval is the training data
  return(h)
}

gradientDes<-function(xval, yval,theta,alpha, max_iter)  #gradient descent function
{
  m <- length(xval)  #dimension of the dataset

  for(i in 1:max_iter) # iteration used as convergence criteria.
  {
    theta = theta - alpha*(1/m)*(t(xval)%*%(hypothesisOfx(xval, theta) - yval))
  }

  return(list(theta))
}

logistReg <-function(xval,yval,alpha,max_iter)
{
  xval<-cbind(rep(1, nrow(xval)), xval) #xval is a matrix containing the training data
  theta<-rep(0,ncol(xval))  # 0-initial value of theta

  results <- gradientDes(xval, yval, theta, alpha, max_iter)
  theta <- results[[1]]

  return(theta) #learned values of theta
}

#3. Training
#3a
xt_01 = cbind(rep(1, nrow(train_0_1)), train_0_1)
xtst = cbind(rep(1, nrow(test_0_1)), test_0_1)
theta_01 = logistReg(se, true_label_train_0_1, alpha= 50 ,max_iter=50)
y_predict_xt = hypothesisOfx(xt_01, theta_01)
y_predict_xtst = hypothesisOfx(xtst, theta_01)

df_Ypredict = data.frame(train_0_1_ylabel = true_label_train_0_1, y_predict_xt =  y_predict_xt)
df_Ypredict$y_predict_xt[df_Ypredict$y_predict_xt + 0.5 >= 1.0] <-1
df_Ypredict$y_predict_xt[df_Ypredict$y_predict_xt + 0.5 < 1.0] <-0
df_Ypredict$error = abs(df_Ypredict$y_predict_xt - df_Ypredict$train_0_1_ylabel)

df_test_Ypredict = data.frame(test_0_1_ylabel = true_label_test_0_1, y_predict_xtst =  y_predict_xtst)
df_test_Ypredict$y_predict_xtst[df_test_Ypredict$y_predict_xtst + 0.5 >= 1.0] <-1
df_test_Ypredict$y_predict_xtst[df_test_Ypredict$y_predict_xtst + 0.5 < 1.0] <-0
df_test_Ypredict$error = abs(df_test_Ypredict$y_predict_xtst - df_test_Ypredict$test_0_1_ylabel)

Accuracy_training01 = (nrow(train_0_1)-sum(df_Ypredict$error))/nrow(train_0_1)
Accuracy_test01 = (nrow(test_0_1) - sum(df_test_Ypredict$error))/nrow(test_0_1)

xt_35 = cbind(rep(1, nrow(train_3_5)), train_3_5)
xtst_35 = cbind(rep(1, nrow(test_3_5)), test_3_5)
true_label01_train_3_5 = true_label_train_3_5
true_label01_train_3_5[,ncol(true_label01_train_3_5)][true_label01_train_3_5[,ncol(true_label01_train_3_5)] == 3] <- 0
true_label01_train_3_5[,ncol(true_label01_train_3_5)][true_label01_train_3_5[,ncol(true_label01_train_3_5)] == 5] <- 1
theta_35 = logistReg(train_3_5,true_label01_train_3_5, alpha= 50 ,max_iter=50)
y_predict_xt_35 = hypothesisOfx(xt_35, theta_35)
y_predict_xtst_35 = hypothesisOfx(xtst_35, theta_35)

df_Ypredict_35 = data.frame(train_3_5_ylabel = true_label01_train_3_5, y_predict_xt_35 =  y_predict_xt_35)
df_Ypredict_35$y_predict_xt_35[df_Ypredict_35$y_predict_xt_35 + 0.5 >= 1.0] <-1
df_Ypredict_35$y_predict_xt_35[df_Ypredict_35$y_predict_xt_35 + 0.5 < 1.0] <-0
df_Ypredict_35$error = abs(df_Ypredict_35$y_predict_xt_35 - df_Ypredict_35$train_3_5_ylabel)

true_label01_test_3_5 = true_label_test_3_5
true_label01_test_3_5[,ncol(true_label01_test_3_5)][true_label01_test_3_5[,ncol(true_label01_test_3_5)] == 3] <- 0
true_label01_test_3_5[,ncol(true_label01_test_3_5)][true_label01_test_3_5[,ncol(true_label01_test_3_5)] == 5] <- 1

df_test_Ypredict_35 = data.frame(test_3_5_ylabel = true_label01_test_3_5, y_predict_xtst_35 =  y_predict_xtst_35)
df_test_Ypredict_35$y_predict_xtst_35[df_test_Ypredict_35$y_predict_xtst_35 + 0.5 >= 1.0] <-1
df_test_Ypredict_35$y_predict_xtst_35[df_test_Ypredict_35$y_predict_xtst_35 + 0.5 < 1.0] <-0
df_test_Ypredict_35$error = abs(df_test_Ypredict_35$y_predict_xtst_35 - df_test_Ypredict_35$test_3_5_ylabel)

Accuracy_training_35 = (nrow(train_3_5)-sum(df_Ypredict_35$error))/nrow(train_3_5)
Accuracy_test_35 = (nrow(test_3_5) - sum(df_test_Ypredict_35$error))/nrow(test_3_5)


cat("Accuracy of training set train_0_1:", Accuracy_training01, "," ,"Accuracy of test set test_0_1:", Accuracy_test01)
cat("Accuracy of training set train_3_5:", Accuracy_training_35, "," ,"Accuracy of test set test_3_5:", Accuracy_test_35)

#3b
logisRegAccuracy01<-function(mnist_train, mnist_test, alpha= 50, max_iter=100)
{
  t_mnist_train = t(mnist_train) # transposed values
  xt = subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="0" | t_mnist_train[,ncol(t_mnist_train)]=="1") # "values"
  trn_sample = floor(0.8*nrow(xt))
  randtr01 = sample(seq_len(nrow(xt)), size = trn_sample)
  tr01 = xt[randtr01, ]
  train_0_1 = data.matrix(tr01[ ,1:784])
  true_label_train_0_1 = data.matrix(as.numeric(tr01[ ,785]))

  t_mnist_test = t(mnist_test) # transposed values
  xtest = subset(t_mnist_test, t_mnist_test[,ncol(t_mnist_test)]=="0" | t_mnist_test[,ncol(t_mnist_test)]=="1") # "values"
  tst_sample = floor(0.8*nrow(xtest))
  randtst01 = sample(seq_len(nrow(xtest)), size = tst_sample)
  tst01 = xtest[randtst01, ]
  test_0_1 = data.matrix(tst01[ ,1:784])
  true_label_test_0_1 = data.matrix(as.numeric(tst01[ ,785]))

  xt01 = cbind(rep(1, nrow(train_0_1)), train_0_1)
  xtst = cbind(rep(1, nrow(test_0_1)), test_0_1)

  theta_01 = logistReg(train_0_1, true_label_train_0_1, alpha, max_iter)
  y_predict_xt = hypothesisOfx(xt01, theta_01)
  y_predict_xtst = hypothesisOfx(xtst, theta_01)

  df_Ypredict01 = data.frame(train_0_1_ylabel = true_label_train_0_1, y_predict_xt =  y_predict_xt)
  df_Ypredict01$y_predict_xt[df_Ypredict01$y_predict_xt + 0.5 >= 1.0] = 1
  df_Ypredict01$y_predict_xt[df_Ypredict01$y_predict_xt + 0.5 < 1.0] = 0
  df_Ypredict01$error = abs(df_Ypredict01$y_predict_xt - df_Ypredict01$train_0_1_ylabel)

  df_test01_Ypredict = data.frame(test_0_1_ylabel = true_label_test_0_1, y_predict_xtst =  y_predict_xtst)
  df_test01_Ypredict$y_predict_xtst[df_test01_Ypredict$y_predict_xtst + 0.5 >= 1.0] = 1
  df_test01_Ypredict$y_predict_xtst[df_test01_Ypredict$y_predict_xtst + 0.5 < 1.0] = 0
  df_test01_Ypredict$error = abs(df_test01_Ypredict$y_predict_xtst - df_test01_Ypredict$test_0_1_ylabel)

  Accuracy_training01 = (nrow(train_0_1)-sum(df_Ypredict01$error))/nrow(train_0_1)
  Accuracy_test01 = (nrow(test_0_1) - sum(df_test01_Ypredict$error))/nrow(test_0_1)
  acc01 = data.frame(Accuracy_training01 = Accuracy_training01, Accuracy_test01 = Accuracy_test01)
  #print(acc01)
  return(list(Accuracy_training01,Accuracy_test01))
}


logisRegAccuracy35<-function(mnist_train, mnist_test, alpha= 50, max_iter=100)
{
  t_mnist_train = t(mnist_train) # transposed values
  xt35 = subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="3" | t_mnist_train[,ncol(t_mnist_train)]=="5") # "values"
  trn35_sample = floor(0.8*nrow(xt35))
  randtr35 = sample(seq_len(nrow(xt35)), size = trn35_sample)
  tr35 = xt35[randtr35, ]
  train_3_5 = data.matrix(tr35[ ,1:784])
  true_label_train_3_5 = data.matrix(as.numeric(tr35[ ,785]))
  true_label01_train_35 = true_label_train_3_5
  true_label01_train_35[,ncol(true_label01_train_35)][true_label01_train_35[,ncol(true_label01_train_35)] == 3] <- 0
  true_label01_train_35[,ncol(true_label01_train_35)][true_label01_train_35[,ncol(true_label01_train_35)] == 5] <- 1

  t_mnist_test = t(mnist_test) # transposed values
  xt35test = subset(t_mnist_test, t_mnist_test[,ncol(t_mnist_test)]=="3" | t_mnist_test[,ncol(t_mnist_test)]=="5") # "values"
  tst35_sample = floor(0.8*nrow(xt35test))
  randtr35test = sample(seq_len(nrow(xt35test)), size = tst35_sample)
  tr35test = xt35test[randtr35test, ]
  test_3_5 = data.matrix(tr35test[ ,1:784])
  true_label_test_3_5 = data.matrix(as.numeric(tr35test[ ,785]))
  true_label01_test_35 = true_label_test_3_5
  true_label01_test_35[,ncol(true_label01_test_35)][true_label01_test_35[,ncol(true_label01_test_35)] == 3] <- 0
  true_label01_test_35[,ncol(true_label01_test_35)][true_label01_test_35[,ncol(true_label01_test_35)] == 5] <- 1

  xtr_35 = cbind(rep(1, nrow(train_3_5)), train_3_5)
  xtest_35 = cbind(rep(1, nrow(test_3_5)), test_3_5)

  theta_tr_35 = logistReg(train_3_5, true_label01_train_35, alpha=50, max_iter=100)
  y_predict_xtr_35 = hypothesisOfx(xtr_35, theta_tr_35)
  y_predict_xtest_35 = hypothesisOfx(xtest_35, theta_tr_35)

  df_Ypredict_35 = data.frame(train_3_5_ylabel = true_label01_train_35, y_predict_xtr_35 =  y_predict_xtr_35)
  df_Ypredict_35$y_predict_xtr_35[df_Ypredict_35$y_predict_xtr_35 + 0.5 >= 1.0] <-1
  df_Ypredict_35$y_predict_xtr_35[df_Ypredict_35$y_predict_xtr_35 + 0.5 < 1.0] <-0
  df_Ypredict_35$error = abs(df_Ypredict_35$y_predict_xtr_35 - df_Ypredict_35$train_3_5_ylabel)

  df_test_Ypredict_35 = data.frame(test_3_5_ylabel = true_label01_test_35, y_predict_xtest_35 =  y_predict_xtest_35)
  df_test_Ypredict_35$y_predict_xtest_35[df_test_Ypredict_35$y_predict_xtest_35 + 0.5 >= 1.0] <-1
  df_test_Ypredict_35$y_predict_xtest_35[df_test_Ypredict_35$y_predict_xtest_35 + 0.5 < 1.0] <-0
  df_test_Ypredict_35$error = abs(df_test_Ypredict_35$y_predict_xtest_35 - df_test_Ypredict_35$test_3_5_ylabel)

  Accuracy_training_35 = (nrow(train_3_5)-sum(df_Ypredict_35$error))/nrow(train_3_5)
  Accuracy_test_35 = (nrow(test_3_5) - sum(df_test_Ypredict_35$error))/nrow(test_3_5)

  return(list(Accuracy_training_35,Accuracy_test_35))
}

#3b_training_0_1
Accuracy_trn01 = rep(0L, times=10)
Accuracy_tst01 = rep(0L, times=10)
iter = rep(0L, times=10)
for(i in 1:10)
{
  acc_01 = logisRegAccuracy01(mnist_train, mnist_test, alpha= 50, max_iter=100)
  iter[i] = i
  Accuracy_trn01[i] = acc_01[[1]]
  Accuracy_tst01[i] = acc_01[[2]]
}
df_3b = data.frame(iter = iter, Accuracy_trn = Accuracy_trn01, Accuracy_tst = Accuracy_tst01)
print(df_3b)
cat("Average training set train_0_1 accuracy:", mean(df_3b$Accuracy_trn), "," ,"Average testing set test_0_1 set average accuracy:", mean(df_3b$Accuracy_tst))

#3b_training_3_5
Accuracy_trn35 = rep(0L, times=10)
Accuracy_tst35 = rep(0L, times=10)
iter_35 = rep(0L, times=10)
for (i in 1:10)
{
  acc_35 = logisRegAccuracy35(mnist_train, mnist_test, alpha= 50, max_iter=50)
  iter_35[i] = i
  Accuracy_trn35[i] = acc_35[[1]]
  Accuracy_tst35[i] = acc_35[[2]]
}
df_3b_35 <- data.frame(iter = iter_35, Accuracy_trn = Accuracy_trn35, Accuracy_tst = Accuracy_tst35)
print(df_3b_35)
cat("Average training set train_3_5 set accuracy:", mean(df_3b_35$Accuracy_trn), ",", "Average training set test_3_5 accuracy:", mean(df_3b_35$Accuracy_tst))

#4a
acc_trn = rep(0L, times=10)
acc_tst = rep(0L, times=10)
iterations = rep(0L, times=10)
acc_trn_1 = rep(0L, times=10)
acc_tst_1 = rep(0L, times=10)
for(i in 1:10)
  {
  i_cnt = i * 50
  for(l in 1:10)
    {
    acc = logisRegAccuracy35(mnist_train, mnist_test, alpha= 50, max_iter= i_cnt)
    acc_trn[l] = acc[[1]]
    acc_tst[l] = acc[[2]]
  }
  iterations[i] = i_cnt
  acc_trn_1[i] = mean(acc_trn)
  acc_tst_1[i] = mean(acc_tst)
}
df_4a = data.frame(iter = iterations, acc_trn = acc_trn_1, acc_tst = acc_tst_1)
df_4a_plt = ggplot(df_4a, aes(iter)) + geom_line(aes(y = acc_trn, colour = "acc_trn")) + geom_line(aes(y = acc_tst, colour = "acc_tst")) + labs(title="Training and Test set accuracy Vs Number of iteration", subtitle = "train_3_5")
print(df_4a)
print(df_4a_plt)

#4b
acc_trn_35 = rep(0L, times=10)
acc_tst_35 = rep(0L, times=10)
iter_alp = rep(0L, times=10)
acc_trn35 = rep(0L, times=10)
acc_tst35 = rep(0L, times=10)
for(i in 1:10)
{
  i_cnt_alp = i * 10
  for(l in 1:10)
  {
    accuracy_35 = logisRegAccuracy35(mnist_train, mnist_test, alpha= i_cnt_alp, max_iter= 50)
    acc_trn_35[l] = accuracy_35[[1]]
    acc_tst_35[l] = accuracy_35[[2]]
  }
  iter_alp[i] = i_cnt_alp
  acc_trn35[i] = mean(acc_trn_35)
  acc_tst35[i] = mean(acc_tst_35)

}
df_4b = data.frame(alpha_iter = iter_alp, accuracy_trn_35 = acc_trn35, accuracy_tst_35 = acc_tst35)
df_4b_plt = ggplot(df_4b, aes(alpha_iter)) + geom_line(aes(y = accuracy_trn_35, colour = "accuracy_trn_35")) + geom_line(aes(y = accuracy_tst_35, colour = "accuracy_tst_35")) + labs(title="Training and Test set accuracy Vs Learning rate Alpha", subtitle = "train_3_5")
print(df_4b)
print(df_4b_plt)

#5a
logisRegAccuracy_setSize01<-function(mnist_train, mnist_test, setSize = 0.8, alpha= 50, max_iter=100)
{
  t_mnist_train = t(mnist_train) # transposed values
  xt = subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="0" | t_mnist_train[,ncol(t_mnist_train)]=="1") # "values"
  trn_sample = floor(setSize*nrow(xt))
  randtr01 = sample(seq_len(nrow(xt)), size = trn_sample)
  tr01 = xt[randtr01, ]
  train_0_1 = data.matrix(tr01[ ,1:784])
  true_label_train_0_1 = data.matrix(as.numeric(tr01[ ,785]))

  t_mnist_test = t(mnist_test) # transposed values
  xtest = subset(t_mnist_test, t_mnist_test[,ncol(t_mnist_test)]=="0" | t_mnist_test[,ncol(t_mnist_test)]=="1") # "values"
  tst_sample = floor(setSize*nrow(xtest))
  randtst01 = sample(seq_len(nrow(xtest)), size = tst_sample)
  tst01 = xtest[randtst01, ]
  test_0_1 = data.matrix(tst01[ ,1:784])
  true_label_test_0_1 = data.matrix(as.numeric(tst01[ ,785]))

  xt01 = cbind(rep(1, nrow(train_0_1)), train_0_1)
  xtst = cbind(rep(1, nrow(test_0_1)), test_0_1)

  theta_01 = logistReg(train_0_1, true_label_train_0_1, alpha, max_iter)
  y_predict_xt = hypothesisOfx(xt01, theta_01)
  y_predict_xtst = hypothesisOfx(xtst, theta_01)

  df_Ypredict01 = data.frame(train_0_1_ylabel = true_label_train_0_1, y_predict_xt =  y_predict_xt)
  df_Ypredict01$y_predict_xt[df_Ypredict01$y_predict_xt + 0.5 >= 1.0] = 1
  df_Ypredict01$y_predict_xt[df_Ypredict01$y_predict_xt + 0.5 < 1.0] = 0
  df_Ypredict01$error = abs(df_Ypredict01$y_predict_xt - df_Ypredict01$train_0_1_ylabel)

  df_test01_Ypredict = data.frame(test_0_1_ylabel = true_label_test_0_1, y_predict_xtst =  y_predict_xtst)
  df_test01_Ypredict$y_predict_xtst[df_test01_Ypredict$y_predict_xtst + 0.5 >= 1.0] = 1
  df_test01_Ypredict$y_predict_xtst[df_test01_Ypredict$y_predict_xtst + 0.5 < 1.0] = 0
  df_test01_Ypredict$error = abs(df_test01_Ypredict$y_predict_xtst - df_test01_Ypredict$test_0_1_ylabel)

  Accuracy_training01 = (nrow(train_0_1)-sum(df_Ypredict01$error))/nrow(train_0_1)
  Accuracy_test01 = (nrow(test_0_1) - sum(df_test01_Ypredict$error))/nrow(test_0_1)
  #acc01 = data.frame(Accuracy_training01 = Accuracy_training01, Accuracy_test01 = Accuracy_test01)
  #print(acc01)
  return(list(Accuracy_training01,Accuracy_test01))
}

logisRegAccuracy_setSize35<-function(mnist_train, mnist_test, setSize = 0.8,alpha= 50, max_iter=100)
{
  t_mnist_train = t(mnist_train) # transposed values
  xt35 = subset(t_mnist_train, t_mnist_train[,ncol(t_mnist_train)]=="3" | t_mnist_train[,ncol(t_mnist_train)]=="5") # "values"
  trn35_sample = floor(setSize*nrow(xt35))
  randtr35 = sample(seq_len(nrow(xt35)), size = trn35_sample)
  tr35 = xt35[randtr35, ]
  train_3_5 = data.matrix(tr35[ ,1:784])
  true_label_train_3_5 = data.matrix(as.numeric(tr35[ ,785]))
  true_label01_train_35 = true_label_train_3_5
  true_label01_train_35[,ncol(true_label01_train_35)][true_label01_train_35[,ncol(true_label01_train_35)] == 3] <- 0
  true_label01_train_35[,ncol(true_label01_train_35)][true_label01_train_35[,ncol(true_label01_train_35)] == 5] <- 1

  t_mnist_test = t(mnist_test) # transposed values
  xt35test = subset(t_mnist_test, t_mnist_test[,ncol(t_mnist_test)]=="3" | t_mnist_test[,ncol(t_mnist_test)]=="5") # "values"
  tst35_sample = floor(setSize*nrow(xt35test))
  randtr35test = sample(seq_len(nrow(xt35test)), size = tst35_sample)
  tr35test = xt35test[randtr35test, ]
  test_3_5 = data.matrix(tr35test[ ,1:784])
  true_label_test_3_5 = data.matrix(as.numeric(tr35test[ ,785]))
  true_label01_test_35 = true_label_test_3_5
  true_label01_test_35[,ncol(true_label01_test_35)][true_label01_test_35[,ncol(true_label01_test_35)] == 3] <- 0
  true_label01_test_35[,ncol(true_label01_test_35)][true_label01_test_35[,ncol(true_label01_test_35)] == 5] <- 1

  xtr_35 = cbind(rep(1, nrow(train_3_5)), train_3_5)
  xtest_35 = cbind(rep(1, nrow(test_3_5)), test_3_5)

  theta_tr_35 = logistReg(train_3_5, true_label01_train_35, alpha=50, max_iter=100)
  y_predict_xtr_35 = hypothesisOfx(xtr_35, theta_tr_35)
  y_predict_xtest_35 = hypothesisOfx(xtest_35, theta_tr_35)

  df_Ypredict_35 = data.frame(train_3_5_ylabel = true_label01_train_35, y_predict_xtr_35 =  y_predict_xtr_35)
  df_Ypredict_35$y_predict_xtr_35[df_Ypredict_35$y_predict_xtr_35 + 0.5 >= 1.0] <-1
  df_Ypredict_35$y_predict_xtr_35[df_Ypredict_35$y_predict_xtr_35 + 0.5 < 1.0] <-0
  df_Ypredict_35$error = abs(df_Ypredict_35$y_predict_xtr_35 - df_Ypredict_35$train_3_5_ylabel)

  df_test_Ypredict_35 = data.frame(test_3_5_ylabel = true_label01_test_35, y_predict_xtest_35 =  y_predict_xtest_35)
  df_test_Ypredict_35$y_predict_xtest_35[df_test_Ypredict_35$y_predict_xtest_35 + 0.5 >= 1.0] <-1
  df_test_Ypredict_35$y_predict_xtest_35[df_test_Ypredict_35$y_predict_xtest_35 + 0.5 < 1.0] <-0
  df_test_Ypredict_35$error = abs(df_test_Ypredict_35$y_predict_xtest_35 - df_test_Ypredict_35$test_3_5_ylabel)

  Accuracy_training_35 = (nrow(train_3_5)-sum(df_Ypredict_35$error))/nrow(train_3_5)
  Accuracy_test_35 = (nrow(test_3_5) - sum(df_test_Ypredict_35$error))/nrow(test_3_5)

  return(list(Accuracy_training_35,Accuracy_test_35))
}
# #5a_training_0_1
acc_01_trn = rep(0L, times=20)
acc_01_tst = rep(0L, times=20)
iter = rep(0L, times=20)
for(i in 1:20)
{
  i_size=(i * 5)/100
  acc_01x = logisRegAccuracy_setSize01(mnist_train, mnist_test, setSize = i_size, alpha= 50, max_iter=100)
  iter[i] = i_size * 100
  acc_01_trn[i] = acc_01x[[1]]
  acc_01_tst[i] = acc_01x[[2]]
}
df_5a = data.frame(iter = iter, Accuracy_trn = acc_01_trn, Accuracy_tst = acc_01_tst)
df_5a_plt = ggplot(df_5a, aes(iter)) + geom_line(aes(y = Accuracy_trn, colour = "acc_trn")) + geom_line(aes(y = Accuracy_tst, colour = "acc_tst")) + labs(title="Training and Test set accuracy Vs training set Sample Sizes", subtitle = "train_0_1")+xlab("Sample Size")+ylab("Taining and Test set accuracy")
print(df_5a_plt)

# #5a_training_3_5
acc_35_trn = rep(0L, times=20)
acc_35_tst = rep(0L, times=20)
iter = rep(0L, times=20)
for(i in 1:20)
{
  i_size35=(i * 5)/100
  acc_35x = logisRegAccuracy_setSize35(mnist_train, mnist_test, setSize = i_size35,alpha= 50, max_iter=100)
  iter[i] = i_size35 * 100
  acc_35_trn[i] = acc_35x[[1]]
  acc_35_tst[i] = acc_35x[[2]]
}
df_5a_35 = data.frame(iter = iter, Accuracy_trn = acc_35_trn, Accuracy_tst = acc_35_tst)
df_5a_35_plt = ggplot(df_5a_35, aes(iter)) + geom_line(aes(y = Accuracy_trn, colour = "acc_trn")) + geom_line(aes(y = Accuracy_tst, colour = "acc_tst")) + labs(title="Training and Test set accuracy Vs training set Sample Sizes", subtitle = "train_3_5")+xlab("Sample Size")+ylab("Taining and Test set accuracy")
print(df_5a_35_plt)

#5b_training_0_1
gradientDes_b5<-function(x, y,theta,alpha, max_iter)
{
  n <- length(x)  #nrow*ncol=dimension d
  theta_j <- rep(0, max_iter)

  for(i in 1:max_iter)
  {
    theta = theta - alpha*(1/n)*(t(x)%*%(hypothesisOfx(x, theta) - y))
    theta_j[i]  = sum(y*log(hypothesisOfx(x, theta)) + (1-y)*log(1-hypothesisOfx(x, theta)))/(-1*n)
  }

  return(list(theta, theta_j))
  #return(theta)
}

logistReg_5b <-function(x,y,alpha,max_iter)
{
  #max_iter = 1500
  #alpha = 0.1
  x<-cbind(rep(1, nrow(x)), x)
  theta<-rep(0,ncol(x))

  results <- gradientDes_b5(x, y, theta, alpha, max_iter)
  theta <- results[[1]]
  theta_j <- results[[2]]

  #plot(1:max_iter, theta_j, type = 'l')
  return(list(1:max_iter,theta_j))
}


q5b_logloss<-function(xt, setSize = 0.8)
{
  alpha=50
  max_iter=100
  trn_sample = floor(setSize*nrow(xt))
  randtr01 = sample(seq_len(nrow(xt)), size = trn_sample)
  tr01 = xt[randtr01, ]
  train_0_1 = data.matrix(tr01[ ,1:784])
  true_label_train_0_1 = data.matrix(as.numeric(tr01[ ,785]))

  ther01=logistReg_5b(train_0_1, true_label_train_0_1, alpha=alpha, max_iter=max_iter)
  setSizesplit=rep(setSize, times=max_iter)
  t=ther01[[2]]
  return(list(setSizesplit,1:max_iter,t))
}

q5b_logloss_train35<-function(xt2, setSize = 0.8)
{
  alpha=50
  max_iter=100
  trn35_sample = floor(setSize*nrow(xt2))
  randtr35 = sample(seq_len(nrow(xt2)), size = trn35_sample)
  tr35 = xt2[randtr35, ]
  train_3_5 = data.matrix(tr35[ ,1:784])
  true_label_train_3_5 = data.matrix(as.numeric(tr35[ ,785]))
  true_label01_train_35 = true_label_train_3_5
  true_label01_train_35[,ncol(true_label01_train_35)][true_label01_train_35[,ncol(true_label01_train_35)] == 3] <- 0
  true_label01_train_35[,ncol(true_label01_train_35)][true_label01_train_35[,ncol(true_label01_train_35)] == 5] <- 1

  theta35=logistReg_5b(train_3_5, true_label01_train_35, alpha=alpha, max_iter=max_iter)
  setSizesplit=rep(setSize, times=max_iter)
  t=theta35[[2]]
  return(list(setSizesplit,1:max_iter,t))
}

loglossTr01<-function()
{
  logl_05 = q5b_logloss(xt, setSize = 0.05)
  ss_05 = logl_05[[1]]
  iter_05 = logl_05[[2]]
  thet_05 = logl_05[[3]]
  df_05 = data.frame(sampleSize = ss_05, iter = iter_05, thetaJ = thet_05)
  df_05$sampleSize[df_05$sampleSize==0.05] = "size_5%"

  logl_10 = q5b_logloss(xt, setSize = 0.10)
  ss_10 = logl_10[[1]]
  iter_10 = logl_10[[2]]
  thet_10 = logl_10[[3]]
  df_10 = data.frame(sampleSize = ss_10, iter = iter_10, thetaJ = thet_10)
  df_10$sampleSize[df_10$sampleSize==0.10] = "size_10%"

  logl_15 = q5b_logloss(xt, setSize = 0.15)
  ss_15 = logl_15[[1]]
  iter_15 = logl_15[[2]]
  thet_15 = logl_15[[3]]
  df_15 = data.frame(sampleSize = ss_15, iter = iter_15, thetaJ = thet_15)
  df_15$sampleSize[df_15$sampleSize==0.15] = "size_15%"

  logl_20 = q5b_logloss(xt, setSize = .20)
  ss_20 = logl_20[[1]]
  iter_20 = logl_20[[2]]
  thet_20 = logl_20[[3]]
  df_20 = data.frame(sampleSize = ss_20, iter = iter_20, thetaJ = thet_20)
  df_20$sampleSize[df_20$sampleSize==0.2] = "size_20%"

  logl_25 = q5b_logloss(xt, setSize = 0.25)
  ss_25 = logl_25[[1]]
  iter_25 = logl_25[[2]]
  thet_25 = logl_25[[3]]
  df_25 = data.frame(sampleSize = ss_25, iter = iter_25, thetaJ = thet_25)
  df_25$sampleSize[df_25$sampleSize==0.25] = "size_25%"

  logl_30 = q5b_logloss(xt, setSize = 0.30)
  ss_30 = logl_30[[1]]
  iter_30 = logl_30[[2]]
  thet_30 = logl_30[[3]]
  df_30 = data.frame(sampleSize = ss_30, iter = iter_30, thetaJ = thet_30)
  df_30$sampleSize[df_30$sampleSize==0.30] = "size_30%"

  logl_35 = q5b_logloss(xt, setSize = 0.35)
  ss_35 = logl_35[[1]]
  iter_35 = logl_35[[2]]
  thet_35 = logl_35[[3]]
  df_35 = data.frame(sampleSize = ss_35, iter = iter_35, thetaJ = thet_35)
  df_35$sampleSize[df_35$sampleSize==0.35] = "size_35%"

  logl_40 = q5b_logloss(xt, setSize = 0.40)
  ss_40 = logl_40[[1]]
  iter_40 = logl_40[[2]]
  thet_40 = logl_40[[3]]
  df_40 = data.frame(sampleSize = ss_40, iter = iter_40, thetaJ = thet_40)
  df_40$sampleSize[df_40$sampleSize==0.40] = "size_40%"

  logl_45 = q5b_logloss(xt, setSize = 0.45)
  ss_45 = logl_45[[1]]
  iter_45 = logl_45[[2]]
  thet_45 = logl_45[[3]]
  df_45 = data.frame(sampleSize = ss_45, iter = iter_45, thetaJ = thet_45)
  df_45$sampleSize[df_45$sampleSize==0.45] = "size_45%"

  logl_50 = q5b_logloss(xt, setSize = 0.50)
  ss_50 = logl_50[[1]]
  iter_50 = logl_50[[2]]
  thet_50 = logl_50[[3]]
  df_50 = data.frame(sampleSize = ss_50, iter = iter_50, thetaJ = thet_50)
  df_50$sampleSize[df_50$sampleSize==0.50] = "size_50%"

  logl_55 = q5b_logloss(xt, setSize = 0.55)
  ss_55 = logl_55[[1]]
  iter_55 = logl_55[[2]]
  thet_55 = logl_55[[3]]
  df_55 = data.frame(sampleSize = ss_55, iter = iter_55, thetaJ = thet_55)
  df_55$sampleSize[df_55$sampleSize==0.55] = "size_55%"

  logl_60 = q5b_logloss(xt, setSize = 0.60)
  ss_60 = logl_60[[1]]
  iter_60 = logl_60[[2]]
  thet_60 = logl_60[[3]]
  df_60 = data.frame(sampleSize = ss_60, iter = iter_60, thetaJ = thet_60)
  df_60$sampleSize[df_60$sampleSize==0.60] = "size_60%"

  logl_65 = q5b_logloss(xt, setSize = 0.65)
  ss_65 = logl_65[[1]]
  iter_65 = logl_65[[2]]
  thet_65 = logl_65[[3]]
  df_65 = data.frame(sampleSize = ss_65, iter = iter_65, thetaJ = thet_65)
  df_65$sampleSize[df_65$sampleSize==0.65] = "size_65%"

  logl_70 = q5b_logloss(xt, setSize = 0.70)
  ss_70 = logl_70[[1]]
  iter_70 = logl_70[[2]]
  thet_70 = logl_70[[3]]
  df_70 = data.frame(sampleSize = ss_70, iter = iter_70, thetaJ = thet_70)
  df_70$sampleSize[df_70$sampleSize==0.70] = "size_70%"

  logl_75 = q5b_logloss(xt, setSize = 0.75)
  ss_75 = logl_75[[1]]
  iter_75 = logl_75[[2]]
  thet_75 = logl_75[[3]]
  df_75 = data.frame(sampleSize = ss_75, iter = iter_75, thetaJ = thet_75)
  df_75$sampleSize[df_75$sampleSize==0.75] = "size_75%"

  logl_80 = q5b_logloss(xt, setSize = 0.80)
  ss_80 = logl_80[[1]]
  iter_80 = logl_80[[2]]
  thet_80 = logl_80[[3]]
  df_80 = data.frame(sampleSize = ss_80, iter = iter_80, thetaJ = thet_80)
  df_80$sampleSize[df_80$sampleSize==0.80] = "size_80%"

  logl_85 = q5b_logloss(xt, setSize = 0.85)
  ss_85 = logl_85[[1]]
  iter_85 = logl_85[[2]]
  thet_85 = logl_85[[3]]
  df_85 = data.frame(sampleSize = ss_85, iter = iter_85, thetaJ = thet_85)
  df_85$sampleSize[df_85$sampleSize==0.85] = "size_85%"

  logl_90 = q5b_logloss(xt, setSize = 0.90)
  ss_90 = logl_90[[1]]
  iter_90 = logl_90[[2]]
  thet_90 = logl_90[[3]]
  df_90 = data.frame(sampleSize = ss_90, iter = iter_90, thetaJ = thet_90)
  df_90$sampleSize[df_90$sampleSize==0.90] = "size_90%"

  logl_95 = q5b_logloss(xt, setSize = 0.95)
  ss_95 = logl_95[[1]]
  iter_95 = logl_95[[2]]
  thet_95 = logl_95[[3]]
  df_95 = data.frame(sampleSize = ss_95, iter = iter_95, thetaJ = thet_95)
  df_95$sampleSize[df_95$sampleSize==0.95] = "size_95%"

  logl_1 = q5b_logloss(xt, setSize = 1.0)
  ss_1 = logl_1[[1]]
  iter_1 = logl_1[[2]]
  thet_1 = logl_1[[3]]
  df_1 = data.frame(sampleSize = ss_1, iter = iter_1, thetaJ = thet_1)
  df_1$sampleSize[df_1$sampleSize==1.0] = "size_100%"

  v=rbind(df_05,df_10,df_15,df_20,df_25,df_30,df_35,df_40,df_45,df_50,df_55,df_60,df_65,df_70,df_75,df_80,df_85,df_90,df_95,df_1)
  dfv=data.frame(v)
  dfv_plot=ggplot() + geom_line(aes(y = thetaJ, x = iter, colour = sampleSize), data = dfv, stat="identity") + labs(title="Logistic loss theta per Sample Size Vs Number of iteration", subtitle = "train_0_1")+xlab("max_iter")+ylab("logloss_theta")
  print(dfv_plot)
}

logloss_Training35<-function()
{
  logl_05 = q5b_logloss_train35(xt, setSize = 0.05)
  ss_05 = logl_05[[1]]
  iter_05 = logl_05[[2]]
  thet_05 = logl_05[[3]]
  df_05 = data.frame(sampleSize = ss_05, iter = iter_05, thetaJ = thet_05)
  df_05$sampleSize[df_05$sampleSize==0.05] = "size_5%"

  logl_10 = q5b_logloss_train35(xt, setSize = 0.10)
  ss_10 = logl_10[[1]]
  iter_10 = logl_10[[2]]
  thet_10 = logl_10[[3]]
  df_10 = data.frame(sampleSize = ss_10, iter = iter_10, thetaJ = thet_10)
  df_10$sampleSize[df_10$sampleSize==0.10] = "size_10%"

  logl_15 = q5b_logloss_train35(xt, setSize = 0.15)
  ss_15 = logl_15[[1]]
  iter_15 = logl_15[[2]]
  thet_15 = logl_15[[3]]
  df_15 = data.frame(sampleSize = ss_15, iter = iter_15, thetaJ = thet_15)
  df_15$sampleSize[df_15$sampleSize==0.15] = "size_15%"

  logl_20 = q5b_logloss_train35(xt, setSize = .20)
  ss_20 = logl_20[[1]]
  iter_20 = logl_20[[2]]
  thet_20 = logl_20[[3]]
  df_20 = data.frame(sampleSize = ss_20, iter = iter_20, thetaJ = thet_20)
  df_20$sampleSize[df_20$sampleSize==0.2] = "size_20%"

  logl_25 = q5b_logloss_train35(xt, setSize = 0.25)
  ss_25 = logl_25[[1]]
  iter_25 = logl_25[[2]]
  thet_25 = logl_25[[3]]
  df_25 = data.frame(sampleSize = ss_25, iter = iter_25, thetaJ = thet_25)
  df_25$sampleSize[df_25$sampleSize==0.25] = "size_25%"

  logl_30 = q5b_logloss_train35(xt, setSize = 0.30)
  ss_30 = logl_30[[1]]
  iter_30 = logl_30[[2]]
  thet_30 = logl_30[[3]]
  df_30 = data.frame(sampleSize = ss_30, iter = iter_30, thetaJ = thet_30)
  df_30$sampleSize[df_30$sampleSize==0.30] = "size_30%"

  logl_35 = q5b_logloss_train35(xt, setSize = 0.35)
  ss_35 = logl_35[[1]]
  iter_35 = logl_35[[2]]
  thet_35 = logl_35[[3]]
  df_35 = data.frame(sampleSize = ss_35, iter = iter_35, thetaJ = thet_35)
  df_35$sampleSize[df_35$sampleSize==0.35] = "size_35%"

  logl_40 = q5b_logloss_train35(xt, setSize = 0.40)
  ss_40 = logl_40[[1]]
  iter_40 = logl_40[[2]]
  thet_40 = logl_40[[3]]
  df_40 = data.frame(sampleSize = ss_40, iter = iter_40, thetaJ = thet_40)
  df_40$sampleSize[df_40$sampleSize==0.40] = "size_40%"

  logl_45 = q5b_logloss_train35(xt, setSize = 0.45)
  ss_45 = logl_45[[1]]
  iter_45 = logl_45[[2]]
  thet_45 = logl_45[[3]]
  df_45 = data.frame(sampleSize = ss_45, iter = iter_45, thetaJ = thet_45)
  df_45$sampleSize[df_45$sampleSize==0.45] = "size_45%"

  logl_50 = q5b_logloss_train35(xt, setSize = 0.50)
  ss_50 = logl_50[[1]]
  iter_50 = logl_50[[2]]
  thet_50 = logl_50[[3]]
  df_50 = data.frame(sampleSize = ss_50, iter = iter_50, thetaJ = thet_50)
  df_50$sampleSize[df_50$sampleSize==0.50] = "size_50%"

  logl_55 = q5b_logloss_train35(xt, setSize = 0.55)
  ss_55 = logl_55[[1]]
  iter_55 = logl_55[[2]]
  thet_55 = logl_55[[3]]
  df_55 = data.frame(sampleSize = ss_55, iter = iter_55, thetaJ = thet_55)
  df_55$sampleSize[df_55$sampleSize==0.55] = "size_55%"

  logl_60 = q5b_logloss_train35(xt, setSize = 0.60)
  ss_60 = logl_60[[1]]
  iter_60 = logl_60[[2]]
  thet_60 = logl_60[[3]]
  df_60 = data.frame(sampleSize = ss_60, iter = iter_60, thetaJ = thet_60)
  df_60$sampleSize[df_60$sampleSize==0.60] = "size_60%"

  logl_65 = q5b_logloss_train35(xt, setSize = 0.65)
  ss_65 = logl_65[[1]]
  iter_65 = logl_65[[2]]
  thet_65 = logl_65[[3]]
  df_65 = data.frame(sampleSize = ss_65, iter = iter_65, thetaJ = thet_65)
  df_65$sampleSize[df_65$sampleSize==0.65] = "size_65%"

  logl_70 = q5b_logloss_train35(xt, setSize = 0.70)
  ss_70 = logl_70[[1]]
  iter_70 = logl_70[[2]]
  thet_70 = logl_70[[3]]
  df_70 = data.frame(sampleSize = ss_70, iter = iter_70, thetaJ = thet_70)
  df_70$sampleSize[df_70$sampleSize==0.70] = "size_70%"

  logl_75 = q5b_logloss_train35(xt, setSize = 0.75)
  ss_75 = logl_75[[1]]
  iter_75 = logl_75[[2]]
  thet_75 = logl_75[[3]]
  df_75 = data.frame(sampleSize = ss_75, iter = iter_75, thetaJ = thet_75)
  df_75$sampleSize[df_75$sampleSize==0.75] = "size_75%"

  logl_80 = q5b_logloss_train35(xt, setSize = 0.80)
  ss_80 = logl_80[[1]]
  iter_80 = logl_80[[2]]
  thet_80 = logl_80[[3]]
  df_80 = data.frame(sampleSize = ss_80, iter = iter_80, thetaJ = thet_80)
  df_80$sampleSize[df_80$sampleSize==0.80] = "size_80%"

  logl_85 = q5b_logloss_train35(xt, setSize = 0.85)
  ss_85 = logl_85[[1]]
  iter_85 = logl_85[[2]]
  thet_85 = logl_85[[3]]
  df_85 = data.frame(sampleSize = ss_85, iter = iter_85, thetaJ = thet_85)
  df_85$sampleSize[df_85$sampleSize==0.85] = "size_85%"

  logl_90 = q5b_logloss_train35(xt, setSize = 0.90)
  ss_90 = logl_90[[1]]
  iter_90 = logl_90[[2]]
  thet_90 = logl_90[[3]]
  df_90 = data.frame(sampleSize = ss_90, iter = iter_90, thetaJ = thet_90)
  df_90$sampleSize[df_90$sampleSize==0.90] = "size_90%"

  logl_95 = q5b_logloss_train35(xt, setSize = 0.95)
  ss_95 = logl_95[[1]]
  iter_95 = logl_95[[2]]
  thet_95 = logl_95[[3]]
  df_95 = data.frame(sampleSize = ss_95, iter = iter_95, thetaJ = thet_95)
  df_95$sampleSize[df_95$sampleSize==0.95] = "size_95%"

  logl_1 = q5b_logloss_train35(xt, setSize = 1.0)
  ss_1 = logl_1[[1]]
  iter_1 = logl_1[[2]]
  thet_1 = logl_1[[3]]
  df_1 = data.frame(sampleSize = ss_1, iter = iter_1, thetaJ = thet_1)
  df_1$sampleSize[df_1$sampleSize==1.0] = "size_100%"

  v=rbind(df_05,df_10,df_15,df_20,df_25,df_30,df_35,df_40,df_45,df_50,df_55,df_60,df_65,df_70,df_75,df_80,df_85,df_90,df_95,df_1)
  dfv=data.frame(v)
  dfv_plot=ggplot() + geom_line(aes(y = thetaJ, x = iter, colour = sampleSize), data = dfv, stat="identity") + labs(title="Logistic loss theta per Sample Size Vs Number of iteration", subtitle = "train_3_5")+xlab("max_iter")+ylab("logloss_theta")
  print(dfv_plot)
}

loglossTr01()
logloss_Training35()
