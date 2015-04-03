from Tkinter import Frame, RAISED
from itertools import product
from point import Point

class PolyominoWidget(Frame):

    def __init__(self, parent, polyomino, cell_size=30, border_width=3, color='Blue'):
        Frame.__init__(self, parent)

        polyomino = polyomino.move_to_origin()

        for (x, y) in product(xrange(polyomino.width()), xrange(polyomino.height())):
            if Point(x, y) in polyomino.points:
                cell = Frame(
                    parent, 
                    background=color, 
                    borderwidth=border_width, 
                    relief=RAISED, 
                    width=cell_size, 
                    height=cell_size
                )
            else:
                cell = Frame(parent, width=cell_size, height=cell_size)

            cell.grid(column=x, row=y)
           

def main():
    from Tkinter import Tk
    from polyomino import Polyomino

    p = Polyomino((0, 0), (1, 0), (2, 0), (2, 1))

    root = Tk()
    root.title("Polyomino")

    pw = PolyominoWidget(root, p)
    pw.grid(column=0, row=0)
 
    root.mainloop()  

if __name__ == '__main__':
    main()  

            
