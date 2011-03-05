import sys

def main(file):
    lines = open(file).readlines()

    used = 0
    for line in lines:
        if line[:5] == "pause":
            l = [token.split("=") for token in line.split(" ")[:-1]]
            d = dict(l)
            used += int(d["allocated"])

    print "Memory used: %f MB"%(used/1000000.0)

if __name__ == "__main__":
    if len(sys.argv) > 1:
        file = sys.argv[1]
    else:
        file = "gc_trace.txt"
    main(file)
