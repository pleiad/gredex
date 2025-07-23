# Use a base image with Scala, SBT, and Node.js preinstalled
FROM hseeberger/scala-sbt:11.0.19_1.9.9_3.3.1

# Install Node.js (v16+) and npm manually
RUN curl -fsSL https://deb.nodesource.com/setup_16.x | bash - && \
    apt-get install -y nodejs git && \
    npm install -g npm

# Set working directory
WORKDIR /gredex

# Copy source code (assumes you copy the full repo into the Docker build context)
COPY . .

# Install frontend dependencies and build frontend
RUN sbt buildFrontend

# Compile backend
RUN sbt compile

# Expose the port used by the backend
EXPOSE 8080

# Start the server
CMD ["sbt", "run"]