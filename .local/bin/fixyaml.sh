#!/bin/bash
#

filepath=$1

sed -e 's/: True/: true/g' $filepath | sed -e 's/: False/: false/g' | sed -e 's/{ /{/g' | sed -e 's/ }/}/g' | sed -e 's/\[ /\[/g' | sed -e 's/ \]/\]/g' > /tmp/fix.yml

mv /tmp/fix.yml $filepath
