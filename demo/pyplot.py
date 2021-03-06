
import matplotlib.pyplot as plt
import pandas as pd
import sys

msft = pd.read_csv('mozplot.dat', sep="\t")
header = msft.columns.values
fig,ax = plt.subplots()
for y in header[1:-1]:
    ax.plot(msft['time'],msft[y], label=y)

ax.legend()
fig.suptitle(sys.argv[1])
fig.canvas.set_window_title('')
plt.show()
