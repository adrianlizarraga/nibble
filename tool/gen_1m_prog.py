print("import \"std/basic\" as stdb;\n")
print("proc main() => int {")

for i in range(10000):
    print("\tstdb::print_out(\"Hello world\\n\");");

print("\treturn 0;")
print("}")
