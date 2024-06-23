PROJECT=flare_cpu

build/${PROJECT}.pdf: build/${PROJECT}.bbl
	lualatex --output-directory=build ${PROJECT}
	cp build/${PROJECT}.pdf .

build/${PROJECT}.bbl: build/${PROJECT}.bcf
	biber build/${PROJECT}

build/${PROJECT}.bcf: ${PROJECT}.tex
	lualatex --output-directory=build ${PROJECT}

clean:
	rm -f build/*
