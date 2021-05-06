FROM python:3.7

WORKDIR /app

COPY . ./
  
  RUN pip install Flask gunicorn
RUN pip install keras pandas numpy tensorflow-cpu pillow
RUN pip install --upgrade google-cloud-storage



# Expose port 5000
EXPOSE 5000
ENV PORT 5000

CMD gunicorn --bind :$PORT app:app --timeout 240

#gcloud builds submit --tag gcr.io/PROJECT/pokemon

#gcloud run deploy pokeguess2 \
#  --image gcr.io/PROJECT/pokemon \
#  --platform managed \
#  --region us-central1 \
#  --allow-unauthenticated \
#  --memory 1028Mi