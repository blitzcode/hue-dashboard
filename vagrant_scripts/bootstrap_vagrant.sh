#!/bin/bash
set -x

# Update to latest packages
sudo apt-get update
sudo apt-get upgrade

# Essentials
sudo apt-get -y install \
  autoconf \
  build-essential \
  curl \
  gcc \
  haskell-stack \
  htop \
  make \
  screen \
  vim \
  xsel \
  zlib1g-dev

# Unused Stuff
# newrelic-sysmond
# libc
# mysql-server
# git
# libreadline-gplv2-dev
# libssl-dev
# libxslt-dev
# libxml2-dev
# libsqlite3-dev
# nodejs
# postgresql
# postgresql-client
# libpq-dev
# redis-server
