items = [
    ("X64_INSTR_SUB_R_R", "X64_InstrSub_R_R"),
]

def find_replace(glob_pattern : str, find : str, replace : str):
    for filepath in glob.iglob(glob_pattern, recursive=True):
        s = None
        with open(filepath) as file:
            s = file.read()
        
        if s:
            s = s.replace(find, replace)
            with open(filepath, "w") as file:
                file.write(s)


def main():
    num_items = len(items)
    c_files = "./src/x64_gen/*.c"
    h_files = "./src/x64_gen/*.h"

    for index, item in enumerate(items):
        print(f"[{index + 1}/{num_items}] replacing {item[0]} with {item[1]}")
        #find_replace(h_files, item[0], item[1])
        #find_replace(c_files, item[0], item[1])

if __name__ == "__main__":
    main()
