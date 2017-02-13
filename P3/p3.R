################################################################################
#                     PRÁCTICA 3 APRENDIZAJE AUTOMÁTICO
#                            Marta Gómez Macías
#                                 Grupo 1
#                              Mayo de 2016
################################################################################
pause <-function(){
    print("Pulsar una tecla para continuar")
    scan(what=character(), n=1)
    graphics.off()
}
set.seed(561)

# Ejercicio 1
library("ISLR")
pairs(~ ., data=Auto)

pause()

boxplot(mpg ~ displacement, data=Auto, ylab="mpg", xlab="displacement")
pause()
boxplot(mpg ~ horsepower, data=Auto, ylab="mpg", xlab="horsepower")
pause()
boxplot(mpg ~ weight, data=Auto, ylab="mpg", xlab="weight")
pause()
boxplot(mpg ~ acceleration, data=Auto, ylab="mpg", xlab="acceleration")
pause()


attach(Auto)
model1 = lm(mpg ~ displacement)
print(summary(model1))
pause()
plot(x=displacement, y=mpg)
abline(model1$coefficients)
pause()

plot(model1, which=c(1))
pause()

model2 = lm(mpg ~ horsepower)
print(summary(model2))
pause()
plot(x=horsepower, y=mpg)
abline(model2$coefficients)
pause()


plot(model2, which=c(1))
pause()

model3 = lm(mpg ~ weight)
print(summary(model3))
pause()
plot(x=weight, y=mpg)
abline(model3$coefficients)
pause()

plot(model3, which=c(1))
pause()


divide_datos <- function(percent_train = 0.8, dataset=Auto) {
    train = sample(x=1:nrow(dataset), size=nrow(dataset)*percent_train)
    test = c(1:nrow(dataset))[-train]
    l = list(train,test)
    names(l) = c("train", "test")
    l
}

division = divide_datos() # dividimos los datos en test y training con 20 y 80% de tamaño


etiquetas <- function(datos=Auto$mpg) {
    mediana = median(datos) # calculamos la mediana de todos los valores
    mpg01 = (datos >= mediana)*1 # multiplicamos por 1 para que sea numérico
    mpg01
}


Auto = data.frame(Auto, as.factor(etiquetas())) # añadimos la columna de etiquetas
names(Auto)[ncol(Auto)] = "mpg01"
Auto$name = NULL # eliminamos la columna de nombres pues sólo queremos valores numéricos

convert_classification <- function(pred, useprob=TRUE) {
    if (useprob) {
        clasificacion = ifelse(pred > 0.5, 1, 0) # lo convertimos en clasificación
    } else {
        clasificacion = pred
    }
    print(table(clasificacion,Auto$mpg01[division$test])) # generamos la matriz de confusión
    cat("Error = ",clasification_error(clasificacion, Auto$mpg01[division$test]),"\n")   
}

logistic_regression_Auto <- function() {
    model = glm(formula = mpg01 ~ (displacement + horsepower + weight), data=Auto,
                subset=division$train, family="binomial")
    pred = predict(object=model, newdata=Auto[division$test,],type="response") # lo testeamos
    list(pred, model)
}


clasification_error <- function(modelo, etiquetas) {
    mean((modelo != etiquetas)*1) # el error será la media del número de puntos mal clasificado
}


convert_classification(logistic_regression_Auto()[[1]])
pause()


library("class")
library("e1071")

normalizar_01 <- function(datos) {
    apply(X=datos, MARGIN=2, FUN=function(x) {
        max <- max(x)
        min <- min(x)
        sapply(X=x, FUN=function(xi) (xi-min)/(max-min))
    })
}

vars_seleccionadas = data.frame(Auto$displacement, Auto$horsepower, Auto$weight)
names(vars_seleccionadas) = c("displacement", "horsepower", "weight")
vs_train = data.frame(normalizar_01(vars_seleccionadas[division$train,]),Auto$mpg01[division$train])
names(vs_train)[ncol(vs_train)] = "mpg01"
vs_test = data.frame(normalizar_01(vars_seleccionadas[division$test,]), Auto$mpg01[division$test])
names(vs_test)[ncol(vs_test)] = "mpg01"

knn_Auto <- function(kfit=1, useprob=F) {
    knn(train=vs_train[,-4], test=vs_test[,-4], cl=vs_train[,4], k=kfit, prob=useprob)
}

get_best_k <- function() {
    clasificacion = tune.knn(x=subset(vs_train, select=-mpg01), y=vs_train$mpg01, k=1:5) 
    clasificacion$best.parameters$k
}

print("knn con k=1")
convert_classification(knn_Auto(), useprob=F)
pause()

print("knn con el mejor knn calculado por tune.knn")
mejork = get_best_k()
cat("mejor k = ",mejork,"\n")
convert_classification(knn_Auto(kfit=mejork), useprob=F)
pause()

library(ROCR)

rocplot = function(pred, truth=Auto$mpg01[division$test], ...) {
    predob = prediction(pred, truth)
    perf = performance(predob, "tpr", "fpr")
    plot(perf,...)
}
rocplot(pred=logistic_regression_Auto()[[1]],col="red", lwd=2)
pred_knn = knn_Auto(kfit=mejork, useprob=T)
prob_knn = attr(pred_knn, "prob")
# debemos modificar los resultados para que puedan ser interpretados de igual forma que el glm
prob_knn = ifelse(pred_knn == 0, 1 - prob_knn, prob_knn)
rocplot(pred=prob_knn, add=T, lwd=2, col="blue")
legend('bottomright', c("glm","knn"), col=c('red', 'blue'), lwd=2)
pause()

print(tune.knn(x=subset(vs_train, select=-mpg01), y=vs_train$mpg01, k=1:5, 
         tunecontrol=tune.control(cross=5))$best.performance)
pause()

library(boot)
print(cv.glm(data=Auto[division$train,], glmfit = logistic_regression_Auto()[[2]], K=5)$delta[1])
pause()


library(ggplot2)

attach(Auto)
modelf1 = lm(mpg ~ I(horsepower^2), subset=division$train, data=Auto)
print(summary(modelf1))
pause()
print(ggplot() + geom_point(data=Auto, aes(x=horsepower^2, y=mpg)) 
    + geom_line(aes(x=Auto[division$test,]$horsepower^2, y=predict(modelf1, Auto[division$test,]))))

pause()

plot(modelf1, which=c(1))

pause()


modelf2 = lm(mpg ~ I((horsepower * weight * displacement)^2), data=Auto, subset=division$train)
print(summary(modelf2))
pause()
print(ggplot() + geom_point(data=Auto, aes(x=(horsepower * weight * displacement)^2, y=mpg))
    + geom_line(aes(x=(Auto[division$test,]$horsepower 
        * Auto[division$test,]$weight * Auto[division$test,]$displacement)^2,
        y=predict(modelf2, Auto[division$test,]))))

pause()
plot(modelf2, which=c(1))

pause()

# Ejercicio 2
library(MASS)
library(glmnet)

attach(Boston)

divB = divide_datos(dataset=Boston)
x = as.matrix(subset(Boston, select=-crim))
y = Boston$crim

regresion_glmnet <- function(a=1) {
    cv.out = cv.glmnet(x=x[divB$train,], y = y[divB$train], alpha=a)
    plot(cv.out)
    bestlam = cv.out$lambda.min
    cat("Lambda con menor error de validación cruzada:",bestlam,"\n")
    lasso.pred = predict(cv.out, s=bestlam, newx=x[divB$test,])
    MSE = mean((lasso.pred - y[divB$test])^2)
    cat("Mean Squared Error = ", MSE,"\n")
    bestlam
}

bestlam <- regresion_glmnet()
pause()


grid = 10^seq(10, -2, length=100)
out = glmnet(x=x, y=y, alpha=1, lambda=grid)
plot(out, col=rainbow(n=length(rownames(out$beta))))
legend('bottomleft', rownames(out$beta), 
  col=rainbow(n=length(rownames(out$beta))), lwd=2, ncol = 3)
pause()
lasso.coef = predict(out, type="coefficients", s=bestlam)[1:ncol(x),]
print(lasso.coef[abs(lasso.coef) > 0.5])
pause()

bestlam_ridge <- regresion_glmnet(a=0)

x = cbind(chas, nox, dis, rad)
out = glmnet(x=x[divB$train,], y=y[divB$train], alpha=0, lambda=grid)

get_residual_error <- function(pred, real) {
    pred - real
}

ridge.pred <- predict(out, s=bestlam, newx=x[divB$test,])

error_redidual <- get_residual_error(ridge.pred, crim[divB$test])

print(mean(error_redidual))

pause()

plot(error_redidual, pch=19, col="red")
pause()


crim01 <- etiquetas(datos=Boston$crim)

Boston = data.frame(Boston, as.factor(crim01))
names(Boston)[ncol(Boston)] = "crim01"


support_vector_machine <- function(k="linear") {
    tune_svm = tune(svm, crim01 ~ ., data=Boston, kernel=k,
                    ranges=list(cost=c(0.001,0.01,0.1,1,5)))
    bestmod = tune_svm$best.model
    ypred = predict(bestmod, Boston[divB$test,])
    print(table(predict=ypred, truth=Boston$crim01[divB$test]))
    cat("Tamaño total del conjunto de test:",length(divB$test),"\n")
    cat("Error por validación cruzada de 10 particiones del modelo:", tune_svm$best.performance)
}
support_vector_machine()
pause()

support_vector_machine(k="polynomial")
pause()

support_vector_machine(k="radial")
pause()

support_vector_machine(k="sigmoid")
pause()

tune_svm = tune(svm, crim01 ~ ., data=Boston[divB$train,], kernel="radial", 
                    cost=5, tunecontrol = tune.control(cross=5))
cat("Error por validación cruzada de 5 particiones del modelo:", tune_svm$best.performance)
pause()

ypred = predict(tune_svm$best.model, Boston[divB$test,])
print(table(predict=ypred, truth=Boston$crim01[divB$test]))
cat("Tamaño total del conjunto de test:",length(divB$test),"\n")
pause()

Boston$crim01 = NULL

library(randomForest)
bag.boston = randomForest(medv ~ ., data = Boston, subset=divB$train, 
                          mtry=ncol(Boston)-1)
print(bag.boston)
pause()

error_test <- function(model=bag.boston,...) {
    yhat = predict(model, newdata=Boston[divB$test,],...)
    plot(yhat, Boston$medv[divB$test],pch=19, col="blue",xlab="Predicción",ylab="Valor real")
    abline(0,1)
    mean((yhat - Boston$medv[divB$test])^2)
}

print(error_test())
pause()

modelo_rf = tuneRF(x=subset(Boston, select=-medv), y=Boston$medv, doBest=T, subset=divB$train)
print(modelo_rf)
pause()

print(error_test(model=modelo_rf))
pause()

plot(modelo_rf)
pause()

library(gbm)

boost.boston = gbm(medv ~ ., data=Boston[divB$train,], distribution="gaussian", 
                   n.trees=20000, cv.folds=10, interaction.depth=floor(sqrt(ncol(Boston)-1)))

print(summary(boost.boston))
pause()

print(gbm.perf(object=boost.boston,method="cv"))
pause()

print(error_test(model=boost.boston,n.trees=boost.boston$n.trees))
pause()

divOJ = divide_datos(percent_train = 800/nrow(OJ), dataset = OJ)

library(tree)
attach(OJ)
tree.oj = tree(Purchase ~ ., OJ, subset=divOJ$train)
print(summary(tree.oj))
pause()

plot(tree.oj)
text(tree.oj, pretty = 0)
pause()

print(length(c(Purchase[divOJ$train])[c(Purchase[divOJ$train]) == 2]))
print(length(c(Purchase[divOJ$train])[c(Purchase[divOJ$train]) == 1]))
pause()

tree.pred = predict(tree.oj, OJ[divOJ$test,], type="class")
t = table(tree.pred, OJ$Purchase[divOJ$test])
print(t)
cat("Tamaño del conjunto de test: ", length(divOJ$test), "\n")
cat("Error de test del modelo: ", (t[2]+t[3])/length(divOJ$test))
cat("Precisión del test: ", (t[1]+t[4])/length((divOJ$test)))
pause()

cv.oj = cv.tree(tree.oj, FUN=prune.misclass)
print(data.frame(cv.oj$size, cv.oj$dev))
pause()

plot(cv.oj$size, cv.oj$dev, type="b", xlab="Tamaño", ylab="Error", col="red", lwd=2)

