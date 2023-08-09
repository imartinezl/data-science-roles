# %%
import tensorflow as tf
import numpy as np
import tensorflow_datasets as tfds

# %%
# Load the IMDB reviews dataset using tfds. This is the raw data.
imdb_reviews = tfds.load('imdb_reviews')

# The IMDB dataset contains a train split and a test split; we create a separate
# handle for each.
train_raw = imdb_reviews['train']
test_raw = imdb_reviews['test']

# Once we have our handles, we format the datasets in a Keras-fit compatible
# format: a tuple of the form (text_data, label).
def format_dataset(input_data):
  return (input_data['text'], input_data['label'])

train_dataset = train_raw.map(format_dataset)
test_dataset = test_raw.map(format_dataset)

# We also create a dataset with only the textual data in it. This will be used
# to build our vocabulary later on.
text_dataset = train_raw.map(lambda data: data['text'])

# %%

import string
import re

# Having looked at our data above, we see that the raw text contains HTML break
# tags of the form '<br />'. These tags will not be removed by the default
# standardizer (which doesn't strip HTML). Because of this, we will need to 
# create a custom standardization function.
def custom_standardization(input_data):
  lowercase = tf.strings.lower(input_data)
  stripped_html = tf.strings.regex_replace(lowercase, '<br />', ' ')
  return tf.strings.regex_replace(stripped_html,
                                  '[%s]' % re.escape(string.punctuation),
                                  '')

# %%
from tensorflow.keras.layers.experimental.preprocessing import TextVectorization

# Model constants.
max_features = 20000
embedding_dim = 128
vectorize_layer = TextVectorization(
    standardize=custom_standardization,
    max_tokens=max_features,
    output_mode='int',
    output_sequence_length=500)

vectorize_layer.adapt(text_dataset.batch(64))

# %%
from tensorflow.keras import layers

text_input = tf.keras.Input(shape=(1,), dtype=tf.string, name='text')

x = vectorize_layer(text_input)
x = layers.Embedding(max_features + 1, embedding_dim)(x)
x = layers.Dropout(0.5)(x)
# x = layers.Conv1D(128, 7, padding='valid', activation='relu', strides=3)(x)
# x = layers.Conv1D(128, 7, padding='valid', activation='relu', strides=3)(x)
x = layers.Dense(128, activation='relu')(x)
x = layers.GlobalMaxPooling1D()(x)
x = layers.Dense(128, activation='relu')(x)
x = layers.Dropout(0.5)(x)
predictions = layers.Dense(1, activation='sigmoid', name='predictions')(x)

model = tf.keras.Model(text_input, predictions)

model.compile(loss='binary_crossentropy', optimizer='adam', metrics=['accuracy'])

# %%

batch_size = 64
epochs = 1

# Fit the model using the train and test datasets.
model.fit(
    train_dataset.batch(batch_size),
    validation_data=test_dataset.batch(batch_size),
    epochs=epochs)

# %%
# model.save('my_model.h5')
model.save('my_model', save_format="tf")

# %%
# import tensorflowjs as tfjs
# tfjs.converters.save_keras_model(model, "test")

# %%
# tensorflowjs_converter \
#     --input_format=tf_saved_model \
#     my_model \
#     your_model

# %%
# import tensorflow as tf
new_model = tf.keras.models.load_model('my_model')
new_model.summary()

# %%
example_texts = ["This was an absolutely terrible movie. Don't be lured in by Christopher Walken or Michael Ironside. Both are great actors, but this must simply be their worst role in history. Even their great acting could not redeem this movie's ridiculous storyline. This movie is an early nineties US propaganda piece. The most pathetic scenes were those when the Columbian rebels were making their cases for revolutions. Maria Conchita Alonso appeared phony, and her pseudo-love affair with Walken was nothing but a pathetic emotional plug in a movie that was devoid of any real meaning. I am disappointed that there are movies like this, ruining actor's like Christopher Walken's good name. I could barely sit through it.",
"This is the kind of film for a snowy Sunday afternoon when the rest of the world can go ahead with its own business as you descend into a big arm-chair and mellow for a couple of hours. Wonderful performances from Cher and Nicolas Cage (as always) gently row the plot along. There are no rapids to cross, no dangerous waters, just a warm and witty paddle through New York life at its best. A family film in every sense and one that deserves the praise it received."]
labels = [0,1]
new_model.predict(example_texts)

# %%
