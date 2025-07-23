# Dockerfile  (root of the repo)
FROM sbtscala/scala-sbt:eclipse-temurin-jammy-11.0.20.1_1_1.9.7_3.3.1

# system tools + Node 18-LTS
RUN apt-get update && \
    apt-get install -y curl gnupg && \
    curl -fsSL https://deb.nodesource.com/setup_18.x | bash - && \
    apt-get install -y nodejs

WORKDIR /gredex
COPY . .

# build frontend and backend
RUN sbt buildFrontend
RUN sbt compile

EXPOSE 8080
CMD ["sbt","run"]