
####################################################################################
## Create model ##
model <- keras_model_sequential()

## Define and compile the RNN model ##
model %>% 
  layer_dense(units = 64, activation = 'relu', batch_input_shape = c(length(listed_x_train), dim(x_train)[[-1]], 5)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(
    optimizer = optimizer_rmsprop(),
    loss = "mae"
    #loss = 'binary_crossentropy',
    #optimizer = 'rmsprop',
    #metrics = c('accuracy')
  )


# train 
model %>% fit(listed_x_train, listed_y_train, epochs = 20, batch_size = 1)

## evaluate ##
score = model %>% evaluate(x_test, y_test, batch_size=128)

## get predictions from a Keras model ##
predictions = model %>% predict(x_test, batch_size=128)






plot(y_test ,type="l",pch=16,col="blue",
     ylab="y_test",xlab=NA,main="RNN ¸ðµ¨",lwd=3)
lines(predictions,col="red",lwd=3)




####################################################################################
lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (300000 - 200001 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 300001 - lookback) / batch_size

####################################################################################


model <- keras_model_sequential() %>%
  layer_gru(units = 32,
            dropout = 0.1,
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout =  0.5) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 5,
  epochs = 2,
  validation_data = val_gen,
  validation_steps = val_steps
)

model %>% predict_generator(
  test_gen,
  steps = test_steps
)

plot(history)

####################################################################################
model <- keras_model_sequential() %>% 
  bidirectional(
    layer_gru(units = 32), input_shape = list(NULL, dim(data)[[-1]])
  ) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 10,
  epochs = 2,
  validation_data = val_gen,
  validation_steps = val_steps
)

train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

test_data <- data[300001:300100,]

mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

x_test <- array(0, dim = c(length(test_data), 
                           lookback / step,
                           dim(data)[[-1]]))

for (j in 1:length(test_data)) {
  x_test[j,,] <- data[300000+j,]
}  

predictions <- model %>% predict(
  x_test,
  batch_size = 1
)

predictions <- (predictions * std[[2]]) + mean[[2]]
y_test <- (data[300001:300100,2] * std[[2]]) + mean[[2]]

cbind(predictions, y_test)

plot(y_test ,type="l",pch=16,col="blue",
     ylab="y_test",xlab=NA,main="RNN ¸ðµ¨",lwd=3, ylim = c(14, 20))
lines(predictions,col="red",lwd=3)

####################################################################################
predictions <- model %>% predict_generator(
  test_gen,
  steps = test_steps
)

# model %>% fit(
#   train_gen,
#   batch_size = batch_size,
#   epochs = 4,
#   validation_data = test_gen
# )

evaluate_naive_method <- function() {
  batch_maes <- c()
  for (step in 1:val_steps) {
    c(samples, targets) %<-% val_gen()
    preds <- samples[,dim(samples)[[2]],2]
    mae <- mean(abs(preds - targets))
    batch_maes <- c(batch_maes, mae)
  }
  print(mean(batch_maes))
}

evaluate_naive_method()

####################################################################################
####################################################################################
# def seq2dataset(seq, window_size):
#   dataset = []
# for i in range(len(seq)-window_size):
#   subset = seq[i:(i+window_size+1)]
# dataset.append([code2idx[item] for item in subset])
# return np.array(dataset)

gen <- function(input_data, seq_size){
  dataset = data.frame()
  for(i in range(length(input_data) - seq_size)){
    subset = input_data[1:(i + seq_size)]
    rbind(dataset, subset)
  }
  return(dataset)
}

gen(train_data, seq_size = 5)

train_data <- data[1:200000,]
test_data <- data[300001:nrow(data),]

x_train <- as.matrix(subset(train_data, select = c(colnames(test_data)[-2])))
y_train <- as.matrix(train_data[,2])
x_test <- as.matrix(subset(test_data, select = c(colnames(test_data)[-2])))
y_test <- as.matrix(test_data[,2])

# create model
model <- keras_model_sequential()

# define and compile the model
model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = (dim(data)[[-1]]-1)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(
    optimizer = optimizer_rmsprop(),
    loss = "mae"
    #loss = 'binary_crossentropy',
    #optimizer = 'rmsprop',
    #metrics = c('accuracy')
  )

# train 
model %>% fit(x_train, y_train, epochs = 20, batch_size = 128)

# evaluate
score = model %>% evaluate(x_test, y_test, batch_size=128)

# get predictions from a Keras model
predictions = model %>% predict(x_test, batch_size=128)

plot(y_test ,type="l",pch=16,col="blue",
     ylab="y_test",xlab=NA,main="RNN ¸ðµ¨",lwd=3)
lines(predictions,col="red",lwd=3)

####################################################################################
####################################################################################
# constants
data_dim <- 16
timesteps <- 8
num_classes <- 10

# define and compile model
# expected input data shape: (batch_size, timesteps, data_dim)
model <- keras_model_sequential() 
model %>% 
  layer_lstm(units = 32, return_sequences = TRUE, input_shape = c(timesteps, data_dim)) %>% 
  layer_lstm(units = 32, return_sequences = TRUE) %>% 
  layer_lstm(units = 32) %>% # return a single vector dimension 32
  layer_dense(units = 10, activation = 'softmax') %>% 
  compile(
    loss = 'categorical_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('accuracy')
  )

# generate dummy training data
x_train <- array(runif(1000 * timesteps * data_dim), dim = c(1000, timesteps, data_dim))
y_train <- matrix(runif(1000 * num_classes), nrow = 1000, ncol = num_classes)

# generate dummy validation data
x_val <- array(runif(100 * timesteps * data_dim), dim = c(100, timesteps, data_dim))
y_val <- matrix(runif(100 * num_classes), nrow = 100, ncol = num_classes)

# train
model %>% fit( 
  x_train, y_train, batch_size = 64, epochs = 5, validation_data = list(x_val, y_val)
)

####################################################################################
####################################################################################

# generate dummy data
x_train <- matrix(runif(1000*20), nrow = 1000, ncol = 20)
y_train <- matrix(round(runif(1000, min = 0, max = 1)), nrow = 1000, ncol = 1)
x_test <- matrix(runif(100*20), nrow = 100, ncol = 20)
y_test <- matrix(round(runif(100, min = 0, max = 1)), nrow = 100, ncol = 1)

# create model
model <- keras_model_sequential()

# define and compile the model
model %>% 
  layer_dense(units = 64, activation = 'relu', input_shape = c(20)) %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(
    loss = 'binary_crossentropy',
    optimizer = 'rmsprop',
    metrics = c('accuracy')
  )

# train 
model %>% fit(x_train, y_train, epochs = 20, batch_size = 128, verbose = 1)

# evaluate
score = model %>% evaluate(x_test, y_test, batch_size=128)

# get predictions from a Keras model
predictions = round(model %>% predict(x_test, batch_size=128))

# for(i in 1:length(predictions)){
#   if(predictions[i] >= 0.5){
#     predictions[i] <- 1
#   }
#   else{
#     predictions[i] <- 0
#   }
# }

plot(y_test ,type="l",pch=16,col="blue",
     ylab="y_test",xlab=NA,main="RNN ¸ðµ¨",lwd=3)
lines(predictions,col="red",lwd=3)
