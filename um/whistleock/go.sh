#!/bin/sh

rm -f wrapped.*
cp um.uma wrapped.uma
cp um.sym wrapped.sym
cp um.bins wrapped.bins
cat um.um fastmark.umz > wrapped.um
um wrapped.um
