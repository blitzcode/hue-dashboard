#!/usr/bin/env bash

# set -x # debug

cd /vagrant
stack upgrade
stack setup
stack build
# stack exec hue-dashboard
echo "Now login to the machine with 'vagrant ssh' then run 'cd /vagrant'"
echo "and run 'stack exec hue-dashbord'"
