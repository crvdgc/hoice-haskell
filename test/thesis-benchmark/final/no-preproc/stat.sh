#!/bin/bash
trap "exit" INT
tee() { if test "$1" != "${1%/*}"; then mkdir -p ${1%/*}; fi &&
   command tee "$1"; }
teea() { if test "$1" != "${1%/*}"; then mkdir -p ${1%/*}; fi &&
   command tee -a "$1"; }

while read line
do
    hoice-ex $line --stat
    printf '%s\n' ----------------------------------
done < $1
