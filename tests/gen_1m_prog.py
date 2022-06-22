print("import \"./print.nib\";\n")
print("proc main() => int {")

for i in range(10000):
    print("\tprint_out(\"Hello world\\n\");");

print("\treturn 0;")
print("}")
