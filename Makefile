all:
	@gcc -o build/output tests/output.s
	@build/output
