#!/bin/bash

if [ $# -ne 1 ]; then
    printf "usage: ./runfrenetic.sh <kat-index> \n"
    exit 0
fi

INDEX=$1
DIR=$(dirname $1)

check_result() {
    expected=$3
    got=$4
    if [ $3 == $4 ]; then
        printf "\e[32msuccess for $1, $2 - expected: $expected \e[0m\n"
    else
        # print failure message in red to stderr along with time
        printf "\e[31mfailure for $1, $2 - expected: $expected, got: $got \e[0m\n"
	printf "Case failed!!" >&2
	exit 1
    fi
}

check_bisim() {

    # run frenetic dump bisim and capture the output and time
    local output
    frenetic dump bisim $DIR/{$1,$2} | (read output; check_result $1 $2 $3 $output)
}


while IFS='\n' read line; do
    unset IFS; read -a fields <<< $line
    printf "Running frenetic dump bisim on ${fields[0]}\n"
    check_bisim ${fields[1]} ${fields[2]} ${fields[3]}
    printf "\n"
done < "$INDEX"
