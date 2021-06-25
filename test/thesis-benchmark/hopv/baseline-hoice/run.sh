#!/bin/bash
trap "exit" INT
tee() { if test "$1" != "${1%/*}"; then mkdir -p ${1%/*}; fi &&
   command tee "$1"; }
teea() { if test "$1" != "${1%/*}"; then mkdir -p ${1%/*}; fi &&
   command tee -a "$1"; }

while read line
do
    OUT=${line#"../lia/"}
    OUT="./results/${OUT/.smt2/}"
    printf "$OUT\n\n"

    TIMEOUTFILES="./timeout"
    # timeout 100 ./hoice $line | tee $OUT
    echo "hoicing $line" | tee $OUT
    exit_status=${PIPESTATUS[0]}
    # printf "$exit_status\n"
    if [[ $exit_status -eq 124 ]]; then
        echo $line | teea $TIMEOUTFILES;
        printf "Timeout" | teea $OUT
    fi
    printf '%s\n' ----------------------------------
done < $1
