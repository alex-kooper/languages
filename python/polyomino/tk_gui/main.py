from datetime import datetime

from Tkinter import Tk, N, S, W, E, StringVar, HORIZONTAL
from ttk import Frame, Button, Label, Entry, Separator

from generator import generate

class MainFrame(Frame):

    def __init__(self, parent):
        Frame.__init__(self, parent)
        self.parent = parent

	self.cell_number_var = StringVar()
        self.elapsed_time_var = StringVar()
        self.polyomino_number_var = StringVar()

        number_label = Label(self, text='Number of cells:')
        self.add_to_grid(0, 0, number_label, sticky=E)

        number_entry = Entry(self, width=5, textvariable=self.cell_number_var)
        self.cell_number_var.set(4)
        self.add_to_grid(1, 0, number_entry, sticky=W)

        generate_button = Button(self, 
                                 text='Generate Polyominoes', 
                                 command=self.generate_polyominoes)

        self.add_to_grid(2, 0, generate_button, sticky=E)

        self.add_separator(1)

	time_message = Label(self, text='Elapsed time:')
        self.add_to_grid(0, 2, time_message, sticky=E)

	elapsed_time  = Label(self, textvariable=self.elapsed_time_var, width=15)
        self.add_to_grid(1, 2, elapsed_time, sticky=W)

	polyomino_message = Label(self, text='Number of generated polyominoes:')
        self.add_to_grid(0, 3, polyomino_message, sticky=E)

	polyomino_number  = Label(self, textvariable=self.polyomino_number_var, width=10)
        self.add_to_grid(1, 3, polyomino_number, sticky=W)

        self.add_separator(4)

        close_button = Button(self,
                              text='Close',
                              command=self.parent.destroy)

        self.add_to_grid(0, 5, close_button, columnspan=3)

    @staticmethod
    def add_to_grid(column, 
                    row,
                    widget, 
                    columnspan=1, 
                    rowspan=1, 
                    sticky=None, 
                    padx=7, 
                    pady=7):

        widget.grid(column=column, 
                    row=row, 
                    columnspan=columnspan,
                    rowspan=rowspan, 
                    sticky=sticky,
                    padx=padx, 
                    pady=pady)

    def add_separator(self, row, padx=7, pady=7):
        s = Separator(self, orient=HORIZONTAL)
        s.grid(row=row, columnspan=3, padx=padx, pady=pady, sticky=(W, E))
 
    def generate_polyominoes(self):
	n = int(self.cell_number_var.get())

        self.elapsed_time_var.set('')
        self.polyomino_number_var.set('')
        self.update_idletasks()

        start = datetime.now()
        polyominoes = generate(n)
        end = datetime.now()

        self.elapsed_time_var.set(end - start)
        self.polyomino_number_var.set(len(polyominoes))
        self.update_idletasks()
        
def main():
    root = Tk()
    root.title("Polyomino Generator")

    main_frame = MainFrame(root)
    main_frame.pack()
 
    root.mainloop()  

if __name__ == '__main__':
    main()  

