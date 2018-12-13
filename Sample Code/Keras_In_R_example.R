install.packages("keras")
install.packages("kerasR")
install.packages("devtools")
install.packages("reticulate")
install.packages("memoise")
install.packages("curl")
install.packages("RCurl")
install.packages("RcppCNPy")
install.packages("ggplot2")
install.packages("etl")
library(keras)
library(kerasR)
library(reticulate)
library(memoise)
library(curl)
library(RCurl)
library(RcppCNPy)

use_python("C:/ProgramData/Anaconda3/python.exe")
keras_init()

# Compatibility check
reticulate::py_module_available("keras")
reticulate::py_module_available("numpy")
reticulate::py_module_available("scipy")
reticulate::py_module_available("tensorflow")
reticulate::py_available()
reticulate::py_config()
reticulate::import("keras.models")

# devtools::install_github("rstudio/tensorflow")
# library(tensorflow)

###############################################################################
## Climate Prediction With RNN Algorithm                                     ##
###############################################################################

library(tibble)
library(readr)
library(ggplot2)

dir.create("D:/Downloads/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "D:/Downloads/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "D:/Downloads/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "D:/Downloads/jena_climate"
)


data_dir <- "D:/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)
glimpse(data)

#ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()
#ggplot(data[1:1440,], aes(x = 1:1440, y = `T (degC)`)) + geom_line()

data <- data.matrix(data[,-1])

train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

####################################################################################
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
   #list(samples[,ncol(samples):1, ], targets))
  }
}

####################################################################################
lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (300000 - 200001 - lookback) / batch_size

# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 300001 - lookback) / batch_size


####################################################################################
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
celsius_mae <- 0.29 * std[[2]]

####################################################################################
model <- keras_model_sequential() %>% 
  layer_flatten(input_shape = c(lookback / step, dim(data)[-1])) %>% 
  layer_dense(units = 32, activation = "relu") %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 20,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)

####################################################################################
model <- keras_model_sequential() %>%
  layer_gru(units = 32, input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 10,
  validation_data = val_gen,
  validation_steps = val_steps
)

####################################################################################
model <- keras_model_sequential() %>%
  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
            input_shape = list(NULL, dim(data)[[-1]])) %>%
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

history <- model %>% fit_generator(
  train_gen,
  steps_per_epoch = 500,
  epochs = 5,
  validation_data = val_gen,
  validation_steps = val_steps
)

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
  steps_per_epoch = 200,
  epochs = 2,
  validation_data = val_gen,
  validation_steps = val_steps
)

plot(history)

####################################################################################
# Define maximum number of input features
max_features <- 20000

# Cut texts after this number of words
# (among top max_features most common words)
maxlen <- 100

batch_size <- 32

# Load imdb dataset 
cat('Loading data...\n')
imdb <- dataset_imdb(num_words = max_features)

# Define training and test sets
x_train <- imdb$train$x
y_train <- imdb$train$y
x_test <- imdb$test$x
y_test <- imdb$test$y

model <- keras_model_sequential() %>% 
  layer_embedding(input_dim = max_features, output_dim = 32) %>% 
  bidirectional(
    layer_lstm(units = 32)
  ) %>% 
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = "rmsprop",
  loss = "binary_crossentropy",
  metrics = c("acc")
)

history <- model %>% fit(
  x_train, y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = 0.2
)

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

###############################################################################
## Simple RNN                                                                ##
###############################################################################

## Here’s the definition of a model along with the compilation step 
# (the compile() function has arguments appropriate for a multi-class classification problem):

# For a multi-class classification problem
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 32, input_shape = c(784)) %>% 
  layer_activation('relu') %>% 
  layer_dense(units = 10) %>% 
  layer_activation('softmax')

model %>% compile(
  optimizer = 'rmsprop',
  loss = 'categorical_crossentropy',
  metrics = c('accuracy')
)

## Here’s what compilation might look like for a mean squared error regression problem:
model %>% compile(
  optimizer = optimizer_rmsprop(lr = 0.002),
  loss = 'mse'
)

## Here’s compilation for a binary classification problem:
model %>% compile( 
  optimizer = optimizer_rmsprop(),
  loss = loss_binary_crossentropy,
  metrics = metric_binary_accuracy
)

## Here’s compilation with a custom metric:
# create metric using backend tensor functions
metric_mean_pred <- function(y_true, y_pred) {
    k_mean(y_pred) 
}

model %>% compile( 
  optimizer = optimizer_rmsprop(),
  loss = loss_binary_crossentropy,
  metrics = c('accuracy', 
              'mean_pred' = metric_mean_pred)
)


## Here’s a single-input model with 2 classes (binary classification):
  
# create model
model <- keras_model_sequential()

# add layers and compile the model
model %>% 
  layer_dense(units = 32, activation = 'relu', input_shape = c(100)) %>% 
  layer_dense(units = 1, activation = 'sigmoid') %>% 
  compile(
    optimizer = 'rmsprop',
    loss = 'binary_crossentropy',
    metrics = c('accuracy')
  )

# Generate dummy data
data <- matrix(runif(1000*100), nrow = 1000, ncol = 100)
labels <- matrix(round(runif(1000, min = 0, max = 1)), nrow = 1000, ncol = 1)

# Train the model, iterating on the data in batches of 32 samples
model %>% fit(data, labels, epochs=10, batch_size=32)


## Here’s a single-input model with 10 classes (categorical classification):
  
# create model
  model <- keras_model_sequential()

# define and compile the model
model %>% 
  layer_dense(units = 32, activation = 'relu', input_shape = c(100)) %>% 
  layer_dense(units = 10, activation = 'softmax') %>% 
  compile(
    optimizer = 'rmsprop',
    loss = 'categorical_crossentropy',
    metrics = c('accuracy')
  )

# Generate dummy data
data <- matrix(runif(1000*100), nrow = 1000, ncol = 100)
labels <- matrix(round(runif(1000, min = 0, max = 9)), nrow = 1000, ncol = 1)

# Convert labels to categorical one-hot encoding
one_hot_labels <- to_categorical(labels, num_classes = 10)

# Train the model, iterating on the data in batches of 32 samples
model %>% fit(data, one_hot_labels, epochs=10, batch_size=32)


###############################################################################
## MNIST Example                                                             ##
###############################################################################

devtools::install_github("stillmatic/MNIST")
#devtools::install_github("statsmaths/kerasR")
#devtools::install_github("rstudio/keras")
#options(RCurlOptions = list(verbose = FALSE, capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"), ssl.verifypeer = FALSE))

use_python("C:/ProgramData/Anaconda3/python.exe")
keras_init()

# Compatibility check
reticulate::py_module_available("keras")
reticulate::py_module_available("numpy")
reticulate::py_module_available("scipy")
reticulate::py_module_available("tensorflow")
reticulate::py_available()
reticulate::py_config()
reticulate::import("keras.models")

#View(MNIST::mnist_train)
#mnist <- dataset_mnist()

#table(MNIST::mnist_train$y)
#str(MNIST::mnist_train)

#setwd("D:/work/Rcode/SinterBoiler_SuctionFanRPM_Control")
#np <- import("numpy")

#mnist <- np$load("D:/work/Rcode/Sample Code/mnist.npz")

devtools::install_github("itsrainingdata/mnistR")
MNIST <-mnistR::autoloadMNIST()

str(MNIST)

x_train <- subset(MNIST$train, select = c(-y)) / 255
x_test <- subset(MNIST$test, select = c(-y)) / 255
y_train <- to_catego(MNIST$train$y)
y_test <- as.factor(MNIST$test$y)

str(x_train)

#x_train <- array_reshape(x_train, c(nrow(x_train), 784))
#x_test <- array_reshape(x_test, c(nrow(x_test), 784))

#y_train <- to_categorical(y_train, 10)
#y_test <- to_categorical(y_test, 10)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)

plot(history)

model %>% evaluate(x_test, y_test)
model %>% predict_classes(x_test)



###############################################################################
## Addition_RNN                                                              ##
###############################################################################

install.packages("stringi")
library(keras)
library(stringi)

# Function Definitions ----------------------------------------------------

# Creates the char table and sorts them.
learn_encoding <- function(chars){
  sort(chars)
}

# Encode from a character sequence to a one hot integer representation.
# > encode("22+22", char_table)
# [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10] [,11] [,12]
# 2    0    0    0    0    1    0    0    0    0     0     0     0
# 2    0    0    0    0    1    0    0    0    0     0     0     0
# +    0    1    0    0    0    0    0    0    0     0     0     0
# 2    0    0    0    0    1    0    0    0    0     0     0     0
# 2    0    0    0    0    1    0    0    0    0     0     0     0
encode <- function(char, char_table){
  strsplit(char, "") %>%
    unlist() %>%
    sapply(function(x){
      as.numeric(x == char_table)
    }) %>% 
    t()
}

# Decode the one hot representation/probabilities representation
# to their character output.
decode <- function(x, char_table){
  apply(x,1, function(y){
    char_table[which.max(y)]
  }) %>% paste0(collapse = "")
}

# Returns a list of questions and expected answers.
generate_data <- function(size, digits, invert = TRUE){
  
  max_num <- as.integer(paste0(rep(9, digits), collapse = ""))
  
  # generate integers for both sides of question
  x <- sample(1:max_num, size = size, replace = TRUE)
  y <- sample(1:max_num, size = size, replace = TRUE)
  
  # make left side always smaller than right side
  left_side <- ifelse(x <= y, x, y)
  right_side <- ifelse(x >= y, x, y)
  
  results <- left_side + right_side
  
  # pad with spaces on the right
  questions <- paste0(left_side, "+", right_side)
  questions <- stri_pad(questions, width = 2*digits+1, 
                        side = "right", pad = " ")
  if(invert){
    questions <- stri_reverse(questions)
  }
  # pad with spaces on the left
  results <- stri_pad(results, width = digits + 1, 
                      side = "left", pad = " ")
  
  list(
    questions = questions,
    results = results
  )
}

# Parameters --------------------------------------------------------------

# Parameters for the model and dataset
TRAINING_SIZE <- 50000
DIGITS <- 2

# Maximum length of input is 'int + int' (e.g., '345+678'). Maximum length of
# int is DIGITS
MAXLEN <- DIGITS + 1 + DIGITS

# All the numbers, plus sign and space for padding
charset <- c(0:9, "+", " ")
char_table <- learn_encoding(charset)


# Data Preparation --------------------------------------------------------

# Generate Data
examples <- generate_data(size = TRAINING_SIZE, digits = DIGITS)

# Vectorization
x <- array(0, dim = c(length(examples$questions), MAXLEN, length(char_table)))
y <- array(0, dim = c(length(examples$questions), DIGITS + 1, length(char_table)))

for(i in 1:TRAINING_SIZE){
  x[i,,] <- encode(examples$questions[i], char_table)
  y[i,,] <- encode(examples$results[i], char_table)
}

# Shuffle
indices <- sample(1:TRAINING_SIZE, size = TRAINING_SIZE)
x <- x[indices,,]
y <- y[indices,,]


# Explicitly set apart 10% for validation data that we never train over
split_at <- trunc(TRAINING_SIZE/10)
x_val <- x[1:split_at,,]
y_val <- y[1:split_at,,]
x_train <- x[(split_at + 1):TRAINING_SIZE,,]
y_train <- y[(split_at + 1):TRAINING_SIZE,,]

print('Training Data:')
print(dim(x_train))
print(dim(y_train))

print('Validation Data:')
print(dim(x_val))
print(dim(y_val))


# Training ----------------------------------------------------------------

HIDDEN_SIZE <- 128
BATCH_SIZE <- 128
LAYERS <- 1

# Initialize sequential model
model <- keras_model_sequential() 

model %>%
  # "Encode" the input sequence using an RNN, producing an output of HIDDEN_SIZE.
  # Note: In a situation where your input sequences have a variable length,
  # use input_shape=(None, num_feature).
  layer_lstm(HIDDEN_SIZE, input_shape=c(MAXLEN, length(char_table))) %>%
  # As the decoder RNN's input, repeatedly provide with the last hidden state of
  # RNN for each time step. Repeat 'DIGITS + 1' times as that's the maximum
  # length of output, e.g., when DIGITS=3, max output is 999+999=1998.
  layer_repeat_vector(DIGITS + 1)

# The decoder RNN could be multiple layers stacked or a single layer.
# By setting return_sequences to True, return not only the last output but
# all the outputs so far in the form of (num_samples, timesteps,
# output_dim). This is necessary as TimeDistributed in the below expects
# the first dimension to be the timesteps.
for(i in 1:LAYERS)
  model %>% layer_lstm(HIDDEN_SIZE, return_sequences = TRUE)

model %>% 
  # Apply a dense layer to the every temporal slice of an input. For each of step
  # of the output sequence, decide which character should be chosen.
  time_distributed(layer_dense(units = length(char_table))) %>%
  layer_activation("softmax")

# Compiling the model
model %>% compile(
  loss = "categorical_crossentropy", 
  optimizer = "adam", 
  metrics = "accuracy"
)

# Get the model summary
summary(model)

# Fitting loop
model %>% fit( 
  x = x_train, 
  y = y_train, 
  batch_size = BATCH_SIZE, 
  epochs = 1,
  validation_data = list(x_val, y_val)
)

# Predict for a new observation
new_obs <- encode("55+22", char_table) %>%
  array(dim = c(1,5,12))
result <- predict(model, new_obs)
result <- result[1,,]
decode(result, char_table)


###############################################################################
## babi_RNN                                                                  ##
###############################################################################

install.packages("readr")
install.packages("stringr")
install.packages("purrr")
install.packages("tibble")
install.packages("dplyr")
library(keras)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(RCurl)

# Function definition -----------------------------------------------------

tokenize_words <- function(x){
  x <- x %>% 
    str_replace_all('([[:punct:]]+)', ' \\1') %>% 
    str_split(' ') %>%
    unlist()
  x[x != ""]
}

parse_stories <- function(lines, only_supporting = FALSE){
  lines <- lines %>% 
    str_split(" ", n = 2) %>%
    map_df(~tibble(nid = as.integer(.x[[1]]), line = .x[[2]]))
  
  lines <- lines %>%
    mutate(
      split = map(line, ~str_split(.x, "\t")[[1]]),
      q = map_chr(split, ~.x[1]),
      a = map_chr(split, ~.x[2]),
      supporting = map(split, ~.x[3] %>% str_split(" ") %>% unlist() %>% as.integer()),
      story_id = c(0, cumsum(nid[-nrow(.)] > nid[-1]))
    ) %>%
    select(-split)
  
  stories <- lines %>%
    filter(is.na(a)) %>%
    select(nid_story = nid, story_id, story = q)
  
  questions <- lines %>%
    filter(!is.na(a)) %>%
    select(-line) %>%
    left_join(stories, by = "story_id") %>%
    filter(nid_story < nid)
  
  if(only_supporting){
    questions <- questions %>%
      filter(map2_lgl(nid_story, supporting, ~.x %in% .y))
  }
  
  questions %>%
    group_by(story_id, nid, question = q, answer = a) %>%
    summarise(story = paste(story, collapse = " ")) %>%
    ungroup() %>% 
    mutate(
      question = map(question, ~tokenize_words(.x)),
      story = map(story, ~tokenize_words(.x)),
      id = row_number()
    ) %>%
    select(id, question, answer, story)
}

vectorize_stories <- function(data, vocab, story_maxlen, query_maxlen){
  
  questions <- map(data$question, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  stories <- map(data$story, function(x){
    map_int(x, ~which(.x == vocab))
  })
  
  # "" represents padding
  answers <- sapply(c("", vocab), function(x){
    as.integer(x == data$answer)
  })
  
  
  list(
    questions = pad_sequences(questions, maxlen = query_maxlen),
    stories   = pad_sequences(stories, maxlen = story_maxlen),
    answers   = answers
  )
}

# Parameters --------------------------------------------------------------

max_length <- 99999
embed_hidden_size <- 50
batch_size <- 32
epochs <- 40

# Data Preparation --------------------------------------------------------

path <- "D:/work/Rcode/Sample Code/babi_tasks_1-20_v1-2.tar.gz"
untar(path, exdir = str_replace(path, fixed(".tar.gz"), "/"))
path <- str_replace(path, fixed(".tar.gz"), "/")

# Default QA1 with 1000 samples
# challenge = '%stasks_1-20_v1-2/en/qa1_single-supporting-fact_%s.txt'
# QA1 with 10,000 samples
# challenge = '%stasks_1-20_v1-2/en-10k/qa1_single-supporting-fact_%s.txt'
# QA2 with 1000 samples
challenge <- "%stasks_1-20_v1-2/en/qa2_two-supporting-facts_%s.txt"
# QA2 with 10,000 samples
# challenge = '%stasks_1-20_v1-2/en-10k/qa2_two-supporting-facts_%s.txt'

train <- read_lines(sprintf(challenge, path, "train")) %>%
  parse_stories() %>%
  filter(map_int(story, ~length(.x)) <= max_length)

test <- read_lines(sprintf(challenge, path, "test")) %>%
  parse_stories() %>%
  filter(map_int(story, ~length(.x)) <= max_length)

# extract the vocabulary
all_data <- bind_rows(train, test)
vocab <- c(unlist(all_data$question), all_data$answer, 
           unlist(all_data$story)) %>%
  unique() %>%
  sort()

# Reserve 0 for masking via pad_sequences
vocab_size <- length(vocab) + 1
story_maxlen <- map_int(all_data$story, ~length(.x)) %>% max()
query_maxlen <- map_int(all_data$question, ~length(.x)) %>% max()

# vectorized versions of training and test sets
train_vec <- vectorize_stories(train, vocab, story_maxlen, query_maxlen)
test_vec <- vectorize_stories(test, vocab, story_maxlen, query_maxlen)

# Defining the model ------------------------------------------------------

sentence <- layer_input(shape = c(story_maxlen), dtype = "int32")
encoded_sentence <- sentence %>% 
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

question <- layer_input(shape = c(query_maxlen), dtype = "int32")
encoded_question <- question %>%
  layer_embedding(input_dim = vocab_size, output_dim = embed_hidden_size) %>%
  layer_dropout(rate = 0.3) %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_repeat_vector(n = story_maxlen)

merged <- list(encoded_sentence, encoded_question) %>%
  layer_add() %>%
  layer_lstm(units = embed_hidden_size) %>%
  layer_dropout(rate = 0.3)

preds <- merged %>%
  layer_dense(units = vocab_size, activation = "softmax")

model <- keras_model(inputs = list(sentence, question), outputs = preds)
model %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = "accuracy"
)

model

# Training ----------------------------------------------------------------

model %>% fit(
  x = list(train_vec$stories, train_vec$questions),
  y = train_vec$answers,
  batch_size = batch_size,
  epochs = epochs,
  validation_split=0.05
)

evaluation <- model %>% evaluate(
  x = list(test_vec$stories, test_vec$questions),
  y = test_vec$answers,
  batch_size = batch_size
)

evaluation

###############################################################################
## stateful_lstm                                                             ##
###############################################################################

library(keras)

# since we are using stateful rnn tsteps can be set to 1
tsteps <- 1
batch_size <- 25
epochs <- 25
# number of elements ahead that are used to make the prediction
lahead <- 1

# Generates an absolute cosine time series with the amplitude exponentially decreasing
# Arguments:
#   amp: amplitude of the cosine function
#   period: period of the cosine function
#   x0: initial x of the time series
#   xn: final x of the time series
#   step: step of the time series discretization
#   k: exponential rate
gen_cosine_amp <- function(amp = 100, period = 1000, x0 = 0, xn = 50000, step = 1, k = 0.0001) {
  n <- (xn-x0) * step
  cos <- array(data = numeric(n), dim = c(n, 1, 1))
  for (i in 1:length(cos)) {
    idx <- x0 + i * step
    cos[[i, 1, 1]] <- amp * cos(2 * pi * idx / period)
    cos[[i, 1, 1]] <- cos[[i, 1, 1]] * exp(-k * idx)
  }
  cos
}

cat('Generating Data...\n')
cos <- gen_cosine_amp()
cat('Input shape:', dim(cos), '\n')

expected_output <- array(data = numeric(length(cos)), dim = c(length(cos), 1))
for (i in 1:(length(cos) - lahead)) {
  expected_output[[i, 1]] <- mean(cos[(i + 1):(i + lahead)])
}

cat('Output shape:', dim(expected_output), '\n')

cat('Creating model:\n')
model <- keras_model_sequential()
model %>%
  layer_lstm(units = 50, input_shape = c(tsteps, 1), batch_size = batch_size,
             return_sequences = TRUE, stateful = TRUE) %>% 
  layer_lstm(units = 50, return_sequences = FALSE, stateful = TRUE) %>% 
  layer_dense(units = 1)
model %>% compile(loss = 'mse', optimizer = 'rmsprop')

cat('Training\n')
for (i in 1:epochs) {
  model %>% fit(cos, expected_output, batch_size = batch_size,
                epochs = 1, verbose = 1, shuffle = FALSE)
  
  model %>% reset_states()
}

cat('Predicting\n')
predicted_output <- model %>% predict(cos, batch_size = batch_size)

cat('Plotting Results\n')
op <- par(mfrow=c(2,1))
plot(expected_output, xlab = '')
title("Expected")
plot(predicted_output, xlab = '')
title("Predicted")
par(op)

#################################################################################
install.packages("keras")
install.packages("kerasR")
install.packages("devtools")
install.packages("reticulate")
install.packages("memoise")
install.packages("curl")
install.packages("RCurl")
install.packages("RcppCNPy")
install.packages("ggplot2")
library(keras)
library(kerasR)
library(reticulate)
library(memoise)
library(curl)
library(RCurl)
library(RcppCNPy)

use_python("C:/ProgramData/Anaconda3/python.exe")
keras_init()

# Compatibility check
reticulate::py_module_available("keras")
reticulate::py_module_available("numpy")
reticulate::py_module_available("scipy")
reticulate::py_module_available("tensorflow")
reticulate::py_available()
reticulate::py_config()
reticulate::import("keras.models")

# devtools::install_github("rstudio/tensorflow")
# library(tensorflow)

###############################################################################
## Climate Prediction With RNN Algorithm                                     ##
###############################################################################

library(tibble)
library(readr)
library(ggplot2)

dir.create("D:/Downloads/jena_climate", recursive = TRUE)
download.file(
  "https://s3.amazonaws.com/keras-datasets/jena_climate_2009_2016.csv.zip",
  "D:/Downloads/jena_climate/jena_climate_2009_2016.csv.zip"
)
unzip(
  "D:/Downloads/jena_climate/jena_climate_2009_2016.csv.zip",
  exdir = "D:/Downloads/jena_climate"
)


data_dir <- "D:/Downloads/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)
glimpse(data)

#ggplot(data, aes(x = 1:nrow(data), y = `T (degC)`)) + geom_line()
#ggplot(data[1:1440,], aes(x = 1:1440, y = `T (degC)`)) + geom_line()

data <- data.matrix(data[,-1])

train_data <- data[1:200000,]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

####################################################################################
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index))
    max_index <- nrow(data) - delay - 1
  i <- min_index + lookback
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index)
        i <<- min_index + lookback
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows), 
                                lookback / step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]]-1, 
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay,2]
    }            
    
    list(samples, targets)
    #list(samples[,ncol(samples):1, ], targets))
  }
}

####################################################################################
lookback <- 1440
step <- 6
delay <- 144
batch_size <- 128

train_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 200000,
  shuffle = TRUE,
  step = step, 
  batch_size = batch_size
)

val_gen = generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 200001,
  max_index = 300000,
  step = step,
  batch_size = batch_size
)

test_gen <- generator(
  data,
  lookback = lookback,
  delay = delay,
  min_index = 300001,
  max_index = NULL,
  step = step,
  batch_size = batch_size
)

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
     ylab="y_test",xlab=NA,main="RNN 모델",lwd=3, ylim = c(14, 20))
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
     ylab="y_test",xlab=NA,main="RNN 모델",lwd=3)
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
     ylab="y_test",xlab=NA,main="RNN 모델",lwd=3)
lines(predictions,col="red",lwd=3)

