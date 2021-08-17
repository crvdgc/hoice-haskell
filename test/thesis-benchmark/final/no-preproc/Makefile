input:
	find ../lia/ -type f > input.txt

run: input
	./run.sh input.txt

stat: input
	./stat.sh input.txt

clean:
	rm -r results timeout
