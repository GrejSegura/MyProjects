import tkinter as tk
import subprocess as sub
from tkinter.ttk import Progressbar
from tkinter import filedialog as fd


#WINDOW_SIZE = "300x200"

root = tk.Tk()
#root.geometry(WINDOW_SIZE)


def openData_1():
    filename1 = fd.askopenfilename()
    print(filename1)

######################################

def openData_2():
    filename2 = fd.askopenfilename()
    print(filename2)


def bar():
    import time
    progress['value']=20
    root.update_idletasks()
    time.sleep(1)
    progress['value']=50
    root.update_idletasks()
    time.sleep(1)
    progress['value']=80
    root.update_idletasks()
    time.sleep(1)
    progress['value']=100


def subcall():
    sub.call('C:/Users/Grejell/AnacondaProjects/IDMatchingProject/data_match.py', shell = True)    

#def quitcall():
#    global root
#    root.destroy()
w = tk.Label(root, text="Match the IDs")
w.grid(padx=5, pady=5, row=0, sticky='W')

tk.Button(root, text="Open Data 1", command=openData_1).grid(padx=5, pady=5, row=1, columnspan=2, sticky='W')
tk.Button(root, text="Open Data 2", command=openData_2).grid(padx=5, pady=5, row=2, columnspan=2, sticky='W')
#tk.Button(root, text="Open Data 2", command=openData_2).pack(padx=35, pady=5, side=tk.LEFT)

#tk.Button(root, text="Match", command=lambda:[bar(), subcall()]).pack(padx=35, pady=5, side=tk.LEFT)
#tk.Button(root, text="Close", command=root.destroy).pack(padx=35, pady=5, side=tk.RIGHT)
tk.Button(root, text="Match", command=lambda:[bar(), subcall()]).grid(padx=5, pady=8, row=3, column=0, sticky='W')
tk.Button(root, text="Close", command=root.destroy).grid(padx=5, pady=8, row=3, column=1, sticky='E')
progress = Progressbar(root, orient='horizontal', mode='determinate')
progress.grid(row=4, padx=1, pady = 2, columnspan=2)
