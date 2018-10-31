#!/bin/bash

for a in *.tex
do
	for ((i=0; i<2; ++i))
	do
		pdflatex $a
	done
done
