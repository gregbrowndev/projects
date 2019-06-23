FROM node:6

# Create app directory
WORKDIR /app

# Install app dependencies
COPY package.json package-lock.json ./
# For npm@5 or later, copy package-lock.json as well
# COPY package.json package-lock.json ./

# Install dependecies
RUN npm install

# Bundle app source
COPY . .

# Expose the port the app runs in
EXPOSE 4200

# Serve the app
CMD [ "npm", "start" ]

# Note - a final step in Dockerising the Angular client app is to ensure the app is served from the host created
# by the docker image. To do this go into package.json and change 'start' under 'scripts' to
# '"start": "ng serve -H 0.0.0.0"'. This will run using the built in server, analogous to Django's runserver
# In production this app would be deployed using a proper server, such as Express.
