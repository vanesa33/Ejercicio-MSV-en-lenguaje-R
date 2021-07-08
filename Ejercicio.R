h <- read.csv("patron3.csv")
head(h)
plot(h$x,h$y,col=h$clase)

#Hacemos la transformación de las coordenadas a polares
r <- sqrt(h$x^2+h$y^2)  #radio
a <- atan2(h$y,h$x)

#Lo pongo uno en hh
hh <- cbind(r,a, h$clase)
head(hh)
colnames(hh) <- c("r","a","clase") #le cambio el nombre
head(hh)
str(hh)

#Convertimos a un dataframe
hh<-as.data.frame(hh)
str(hh)

#Visualizamos
plot(hh$r, hh$a, col= hh$clase)

#Empecemos entonces con MSV
library(e1071)

#Dividimos en train y test
x <- runif(nrow(hh))
train <- hh[which(x<.7),]
test <- hh[which(x>=.7),]

nrow(train)
nrow(test)

#Aplicamos el modelo
svm.model <- svm(formula = clase ~ r+a, data = train)

#Obtenemos su prediccion
svm.pred <- predict(svm.model,newdata=test)

#Recortamos los datos y los volvemos enteros
q <- floor(svm.pred+.5)
q
table(q)

#Vemos de vuelta nuestro grafico
plot(test$r, test$a, col=q )

#Construimos la matriz
m <- matrix(nrow=4, ncol=4)

for(i in 1:4) {
  for(j in 1:4) {
    m[i,j] <- length(which(test$clase == i & q == j))
  }
}
m
