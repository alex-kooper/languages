

def trampoline(f, *args, **kw):
    result = f(*args, **kw)

    while callable(result):
        result = result()

    return result
