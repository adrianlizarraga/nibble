import glob
import json
import re
import sys

def find_replace(glob_pattern : str, find : str, replace : str):
    for filepath in glob.iglob(glob_pattern, recursive=True):
        s = None
        with open(filepath) as file:
            s = file.read()
        
        if s:
            s = re.sub(r"\b%s\b" % find, replace, s)
            with open(filepath, "w") as file:
                file.write(s)

def print_usage(exe_name: str):
    print(f"Usage: {exe_name} <items.json> <targets.json>")
    print("    <items.json>       JSON file containing \"word\" items to find and replace (list of [2]str items)")
    print("    <targets.json>     JSON file containing a list of glob patterns for the target files")
    print("")

def main():
    exe_name, *argv = sys.argv

    if len(argv) != 2:
        print("[ERROR]: Incorrect number of command-line arguments", file=sys.stderr)
        print_usage(exe_name)
        exit(1)

    items_filepath= argv[0]
    targets_filepath = argv[1]

    try:
        f = open(items_filepath)
        items = json.load(f)
    except Exception as e:
        print("[ERROR]: %s" % str(e), file=sys.stderr)
        print_usage(exe_name)
        exit(1)

    try:
        f = open(targets_filepath)
        targets = json.load(f)
    except Exception as e:
        print("[ERROR]: %s" % str(e), file=sys.stderr)
        print_usage(exe_name)
        exit(1)

    num_items = len(items)

    for index, item in enumerate(items):
        print(f"[{index + 1}/{num_items}] replacing {item[0]} with {item[1]}")

        for target in targets:
            print(f"\t{target} ...")

            try:
                find_replace(target, item[0], item[1])
            except Exception as e:
                print("[ERROR]: %s" % str(e), file=sys.stderr)
                exit(1)


if __name__ == "__main__":
    main()
