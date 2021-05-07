from flask import Flask
from flask import jsonify

import os
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

from google.cloud import storage

proj_id = "directed-asset-312701"
bucket_name =   'poke-images2'      
### All the same for each prediction

storage_client = storage.Client(proj_id)
# Create a bucket object for our bucket
bucket = storage_client.get_bucket(bucket_name)
# Create a blob object from the filepath
blob = bucket.blob('my_model.h5')
# Download the file to a destination
blob.download_to_filename('my_model.h5')

model = load_model('my_model.h5')



labels = ['bulbasaur','charmander','chikorita','cyndaquil',
     'gengar','groudon','kyogre','latios','machop','metagross','mew','mewtwo','mightyena',
     'milotic','mudkip','pikachu','rayquaza','salamence','squirtle','torchic','totodile','treecko']
WIDTH = 2**8
HEIGHT = 2**8
#os.environ['GOOGLE_APPLICATION_CREDENTIALS'] = 'auth.json'


app = Flask(__name__)



@app.route("/guess/<file>", methods=["GET"])
def hello_name(file):
    storage_client = storage.Client(proj_id)
    # Create a bucket object for our bucket
    bucket = storage_client.get_bucket(bucket_name)
    # Create a blob object from the filepath
    blob = bucket.blob(file)
    # Download the file to a destination
    blob.download_to_filename(file)
    
    img = image.load_img(file, target_size=(HEIGHT, WIDTH))
    x = image.img_to_array(img)
    x = np.expand_dims(x, axis=0)
    x = preprocess_input(x)
    y = model.predict(x)
    
    result = pd.DataFrame({'label':labels,'prob':y[0]}).sort_values(
              by=['prob']).tail().to_dict('records')
    
    resp = jsonify(result)
    resp.status_code = 200
    return resp



if __name__ == "__main__":
    app.run(host="0.0.0.0", port=int(os.environ.get("PORT", 8080)))
