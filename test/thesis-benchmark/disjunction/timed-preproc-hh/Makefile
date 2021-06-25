input:
	find ../lia/ -type f > input.txt

run: input
	./run.sh input.txt

no-preproc: input
	./run-no-preproc.sh input.txt

clean:
	rm -r results timeout
