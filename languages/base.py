import inspect
import ideco

def convert_type_name(typ):
    if str(typ).startswith('list['):
        return str(typ)
        # inner_typ = str(typ)[5:-1]
        # return inner_typ
    elif typ.__module__ == 'builtins':
        return typ.__name__
    else:
        return typ.__module__ + '.' + typ.__name__

class Node:
    @classmethod
    def superclasses(cls):
        tree = inspect.getclasstree([cls])
        superclasses = []

        while len(tree) > 0:
            entry = tree.pop(0)

            if isinstance(entry, tuple):
                base, derived = entry
                base = str(base)
                base = base[base.index("'")+1:]
                base = base[:base.index("'")]

                if '.' in base:
                    superclasses.append(base.split('.')[1])

            elif isinstance(entry, list):
                tree.extend(entry)

        return superclasses

    @classmethod
    def descends_from(cls, base_type):
        if base_type.startswith('list['):
            base_type = base_type[5:-1]

        tree = inspect.getclasstree([cls])

        while len(tree) > 0:
            entry = tree.pop(0)

            if base_type == 'str':
                print(entry)

            if isinstance(entry, tuple):
                base, derived = entry
                base = convert_type_name(base)

                if base == base_type:
                    return True

                for d in derived:
                    if convert_type_name(d) == base_type:
                        return True

            elif isinstance(entry, list):
                tree.extend(entry)

        return False

    @classmethod
    def child_types(cls):
        types = []

        for name, typ in cls.__annotations__.items():
            typ = convert_type_name(typ)
            types.append((name, typ))

        return types

    @classmethod
    def superclass(cls):
        return convert_type_name(inspect.getclasstree([cls])[-1][0][1][0])

    @staticmethod
    def is_seq():
        return False

    @staticmethod
    def passthrough():
        return ''

    def seq(self):
        return None

    @staticmethod
    def priority():
        return 1

    @staticmethod
    def first_token():
        return ''

    @staticmethod
    def is_hidden():
        return False

    @staticmethod
    def is_trivial():
        return False

    @staticmethod
    def src_superclass():
        return 'None'

    @classmethod
    def inputs(cls):
        outputs = cls.outputs()
        inputs = []

        for name in cls.__annotations__:
            if name not in outputs:
                inputs.append(name)

        return inputs

    @classmethod
    def outputs(cls):
        return []

    def __repr__(self):
        return ideco.eval_repr('$self')

    def init(self):
        return

    @staticmethod
    def references(params):
        return []

    def copy(self):
        return self

    @classmethod
    def set_types(cls, params):
        return

    @classmethod
    def data_layout(cls):
        if hasattr(cls, 'layout'):
            return getattr(cls, 'layout')
        else:
            return []

class DataType(Node):
    pass

class Typealias(DataType):
    pass

class Expr(Node):
    pass

class Stmt(Node):
    pass
