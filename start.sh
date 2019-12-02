#!/bin/bash

DAY="${1}"
FOLDER="./$(printf day%02d ${DAY})"

mkdir "${FOLDER}"
curl "https://adventofcode.com/2019/day/${DAY}/input" -H "cookie: session=${AOC_SESSION_COOKIE}" -o "${FOLDER}/input.txt"
