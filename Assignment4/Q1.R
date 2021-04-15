library(tensorflow)
library(keras)

mnist <- dataset_fashion_mnist()

x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

dim(x_train)
dim(x_test)

y_test  <- y_test[1:2560]
x_train <- x_train[1:10240,,]
x_test  <- x_test[1:2560,, ]

y_train <- y_train[1:10240]
y_test  <- y_test[1:2560]

# 1.2 Plot 28 x 28 images
digit <- x_train[58,28:1,1:28]
par(pty="s") # for keeping the aspect ratio 1:1
image(t(digit), col = gray.colors(256), axes = FALSE)

# 1.3 Process the data
# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255

# convert binary to categorical
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)
head(y_train)
