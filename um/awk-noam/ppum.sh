#!/bin/sh
od -vt u1 -w4 | cut -sd' ' -f 2- | sed -e 's/^ //' | tr -s ' '
