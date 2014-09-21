#!/bin/sh
# Run
sbt -J-Xss1024m run | tee ../.bench.tex~
# Cleanup output
grep '\\' ../.bench.tex~ > ../bench.tex
