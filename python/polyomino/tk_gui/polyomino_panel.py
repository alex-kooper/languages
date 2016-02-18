from Tkinter import Frame
from tk_gui.polyomino_widget import PolyominoWidget

class PolyominoPanel(Frame):
    def __init__(self, parent, polyominoes, n_columns, pad_x=3, pad_y=5):
        Frame.__init__(self, parent)
        
        for i, p in enumerate(polyominoes):
            w = PolyominoWidget(self, p)

            w.grid(column=i % n_columns, 
                   row=i / n_columns, 
                   padx=pad_x, 
                   pady=pad_y)

def main():
    from Tkinter import Tk
    from generator import generate

    root = Tk()
    root.title("Polyomino Panel")

    pp = PolyominoPanel(root, generate(5), 3)
    pp.pack()
 
    root.mainloop()  

if __name__ == '__main__':
    main()  
