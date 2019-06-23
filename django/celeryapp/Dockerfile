FROM python:3
ENV PYTHONUNBUFFERED 1
EXPOSE 8000

# Create a celery group/user for the celery worker which run in a container based off this image
#RUN groupadd -r celery && useradd --no-log-init -r -g celery celery

RUN mkdir /code
WORKDIR /code
COPY requirements.txt .
RUN pip install -r requirements.txt

COPY . .

COPY docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]