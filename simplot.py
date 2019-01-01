import matplotlib.pyplot as plt
import subprocess, os, sys

def runSim(args):
    subprocess.run(["fsharpc", "animalsSmall.fs"])
    print("Compile done ========================================================")
    subprocess.run(["mono","animalsSmall.exe", args[1], args[2], args[3], args[4], args[5], args[6], args[7]])
    print("Running complete ====================================================")

def readOutput (filename):
    file = open(filename,'r')
    lines = file.readlines()
    lst = []
    for string in lines:
        lst.append(string.replace(" \n","").split(":"))
    ticks = []
    moose = []
    wolves = []
    for tick in lst:
        ticks.append(tick[0])
        moose.append(tick[1])
        wolves.append(tick[2])
    return (ticks,moose,wolves)
        
def writePlot(lst):
    ticks = lst[0]
    moose = lst[1]
    wolves = lst[2]

    graph = plt.figure()

    graph1 = graph.add_subplot(2,1,1)
    graph1.plot(ticks,moose)
    graph1.set_ylabel("moose")

    graph2 = graph.add_subplot(2,1,2)
    graph2.plot(ticks,wolves)
    graph2.set_ylabel("wolves")
    graph2.set_xlabel("ticks")
    plt.show()

def main(args):
    print(sys.argv)
    if len(args) != 8:
        return "Param Error"
    else:
        runSim(args)
        writePlot(readOutput("output.txt"))
        return "Success"

print(main(sys.argv))
    
