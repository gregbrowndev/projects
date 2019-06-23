FROM python:3
ENV PYTHONUNBUFFERED 1
EXPOSE 8000
RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
	   binutils libproj-dev gdal-bin  \
	&& rm -rf /var/lib/apt/lists/*

RUN mkdir /code
WORKDIR /code
COPY requirements.txt .
RUN pip install -r requirements.txt
COPY . .

COPY docker-entrypoint.sh /
RUN chmod +x /docker-entrypoint.sh
CMD ["/docker-entrypoint.sh"]