def task1(infile):
    f = open(infile)
    lines = list(map(lambda s: int(s.strip()), f.readlines()))

    for i in lines:
        for j in lines:
            if i+j == 2020:
                print("Numbers " + str(i) + " and " + str(j) + " multiply to " + str(i*j))

def task2(infile):
    f = open(infile)
    lines = list(map(lambda s: int(s.strip()), f.readlines()))

    for i in lines:
        for j in lines:
            for k in lines:
                if i+j+k == 2020:
                    print("Numbers " + str(i) + ", "+ str(j) +" and " + str(k) + " multiply to " + str(i*j*k))

print("Test")
task1("./test")
task2("./test")

print("Final")
task1("./input")
task2("./input")
