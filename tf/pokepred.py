### Load required packages
from tensorflow.keras.models import Model
from tensorflow.keras.layers import Dense, GlobalAveragePooling2D, Dropout
from tensorflow.keras.applications.inception_v3 import InceptionV3, preprocess_input
from tensorflow.keras.preprocessing.image import ImageDataGenerator
import random
from tensorflow.keras.preprocessing import image
from tensorflow.keras.models import load_model
import numpy as np
import pandas as pd

import tensorflow as tf
#gpus = tf.config.list_physical_devices('GPU')
#tf.config.experimental.set_virtual_device_configuration(
#        gpus[0],
#        [tf.config.experimental.VirtualDeviceConfiguration(memory_limit=1000)])
        

        
        
### All the same for each prediction
model = load_model('tf/my_model.h5')
labels = ['bulbasaur','charmander','chikorita','cyndaquil',
     'gengar','groudon','kyogre','latios','machop','metagross','mew','mewtwo','mightyena',
     'milotic','mudkip','pikachu','rayquaza','salamence','squirtle','torchic','totodile','treecko']
WIDTH = 2**8
HEIGHT = 2**8

def poke_prediction(file):    
    
    img = image.load_img(file, target_size=(HEIGHT, WIDTH))
    x = image.img_to_array(img)
    x = np.expand_dims(x, axis=0)
    x = preprocess_input(x)
    y = model.predict(x)
    
    result = pd.DataFrame({'label':labels,'prob':y[0]}).sort_values(
              by=['prob']).tail()
    return result
