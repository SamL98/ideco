import ideco
import hlil
import base

class StackCanaryAssign(hlil.VarInit):
    dt: base.DataType
    var: hlil.Var
    src: hlil.Symbol

    @staticmethod
    def match():
        (ok, vars) = ideco.eval_tmpl('$dt $var = *($src)')
        return ok and vars['src'].name == '___stack_chk_guard'

    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class StackCanaryCheck(hlil.Stmt):
    saved: hlil.Expr
    current: hlil.Expr

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
if ($saved != $current) {
    ___stack_chk_fail()
}''')[0]

    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')
