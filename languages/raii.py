import ideco
import hlil
import base

class Destroy(hlil.CallStmt):
    func: hlil.Symbol
    args: list[hlil.Expr]

    @staticmethod
    def first_token():
        return '&outlined destroy of'

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('&$func(${args*<, >})')
        return ok and vars['func'].name.startswith('outlined destroy of')

    @staticmethod
    def is_hidden():
        return True

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'

    def __repr__(self):
        return ideco.eval_repr('')
