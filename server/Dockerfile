FROM haskell:latest

WORKDIR /guide
ADD . /guide

RUN stack setup
RUN stack build ---copy-bins --allow-different-user

EXPOSE 80

RUN stack install