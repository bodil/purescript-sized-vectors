#!/bin/bash

purty src/Data/Vec.purs > /tmp/Vec.purs.purty

if cmp -s "src/Data/Vec.purs" "/tmp/Vec.purs.purty";
then
  pulp test
else
  exit 1
fi
