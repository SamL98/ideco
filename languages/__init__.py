from importlib import import_module
import inspect

# from .base import Node

def load_module(module_name):
    from base import Node
    module = import_module(module_name)
    node_types = []

    for d in dir(module):
        a = getattr(module, d)
        # print(a, inspect.getclasstree([a]))
        # print(a, inspect.isclass(a) and issubclass(a, Node))

        if inspect.isclass(a) and issubclass(a, Node):
            node_types.append(a)

    return node_types