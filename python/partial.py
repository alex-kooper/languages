
def part1(function, arg):
    """ Substitute the first argument of a function"""
    def new_function(*args, **kw):
        return function(arg, *args, **kw)

    return new_function


def part(function, *args, **kw):
    """ Substitute many arguments either defined by positions or keywords """
    def new_function(*new_args, **new_kw):
        res_args = args + new_args
        res_kw = dict(kw.items() + new_kw.items())        
        return function(*res_args, **res_kw)

    return new_function

