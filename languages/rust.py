import ideco
import hlil
import base

class AddAssign(hlil.Stmt):
    func: hlil.Symbol
    lhs: hlil.Expr
    rhs: hlil.Expr

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('$func(&$lhs, $rhs)')
        return ok and 'add_assign' in vars['func'].name.split('::')

    def __repr__(self):
        return ideco.eval_repr('$lhs += $rhs')

    @staticmethod
    def first_token():
        return 'type:Symbol'

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'
