import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import subprocess
import os
import sys
import hashlib

SOURCECODE = "animalsSmall.fs"


def detect_sourcecode_change():
    """ Basic md5sum checking. """
    md5 = hashlib.md5()
    try:
        with open("last_checksum.md5", "rb") as chksum:
            last_read_digest = chksum.read()
    except FileNotFoundError:
        print("[!] No checksum detected. Forcing recompile")
        last_read_digest = None
    except Exception as ex:
        print("[!] Got error trying to read old checksum. Check file permissions")
        last_read_digest = None
    try:
        with open(SOURCECODE, "r") as source:
            current_source = source.read()
            source_bytes = current_source.encode("UTF8")
            md5.update(source_bytes)
            current_digest = md5.digest()
    except Exception as ex:
        print("[!] Got error while calculating checksum. Forcing recompile now")
        return False

    with open("last_checksum.md5", "wb") as chksum:
        chksum.write(current_digest)

    return (last_read_digest != current_digest)


def runSim(args):
    print("[*] Checking if compile is neccesary")
    try:
        should_compile = detect_sourcecode_change()
    except:
        should_compile = None
    if should_compile:
        print("[*] Compiling")
        subprocess.run(
            ["fsharpc", "--nologo", SOURCECODE, "--out:animalsim.exe"]
        )
        print("[*] Compile done")

    print("[*] Running sim")
    subprocess.run(["mono", "animalsim.exe", *args[1:]])
    print("[*] Simulation run")


def readOutput(filename):
    """ Reads filename and parses as ints """
    with open(filename, "r") as file:
        # Python list comprehension to parse output of simulation
        parsed = [line.replace(" \n", "").split(":")
                  for line in file]
        ticks = [int(x[0]) for x in parsed]
        moose = [int(x[1]) for x in parsed]
        wolves = [int(x[2]) for x in parsed]
        return (ticks, moose, wolves)


def writePlot(ticks, moose, wolves):
    """ This takes the values as integer arrays, shows a plot then returns the figure"""
    max_moose_amount = max(moose)
    max_wolf_amount = max(wolves)
    y_max, x_max = max([max_moose_amount, max_wolf_amount])+0.5, max(ticks)

    def formatPlt(axes):
        """ Helper function to get the same formatting across plots 
            Takes axes: a matplotlib Axes object
            returns: nothing
        """
        axes.set_xlim([0, x_max])
        axes.set_ylim(0, y_max)
        axes.xaxis.set_major_locator(ticker.MaxNLocator(integer=True))
        axes.yaxis.set_major_locator(ticker.MaxNLocator(integer=True))
        axes.set_xlabel("Ticks")

    graph = plt.figure()

    moose_graph = graph.add_subplot(2, 1, 1)
    moose_graph.plot(ticks, moose)
    moose_graph.set_title(label="Moose")
    formatPlt(moose_graph)

    wolf_graph = graph.add_subplot(2, 1, 2)
    wolf_graph.plot(ticks, wolves)
    wolf_graph.set_title(label="Wolves")
    formatPlt(wolf_graph)

    # Can be saved after
    plt.show()
    return graph


paramErrorText = """
Parameter Error. Expected the following structure:
width:          width of sim environment
nmoose:         Number of moose to start with
moosereplen:    Moose replication timer
nwolves:        How many wolves to start with
wolvesreplen:   Wolf replication timer
wolveshunglen:  Wolf hunger timer
numberofticks:  How long to run the simulation for
"""


def main(args):
    assert len(args) == 8, paramErrorText
    try:
        print("[*] Compiling and running")
        runSim(args)
    except:
        print("Error in compiling and running simulation")
        return 1
    try:
        output = readOutput("output.txt")
    except Exception as ex:
        print("Unexpected error in file parsing\n"+str(ex))
    try:
        print("[*] Plotting")
        plot = writePlot(*output)
    except Exception as ex:
        print("Error in plotting sim:\n"+str(ex))
        return 1
    save = input("Finished plot. Enter filename to save:\n>")
    if save:
        plot.savefig(save)
    return 0


if __name__ == "__main__":
    main(sys.argv)
