#!/bin/sh
# Run
sbt run | tee ../.bench.tex~
# Cleanup output
grep '\\' ../.bench.tex~ > ../bench.tex
