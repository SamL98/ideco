import ideco
import hlil
import base

class ReleaseCall(hlil.Call):
    pass

class RetainCall(hlil.Call):
    pass

class IndirectReleaseCall(ReleaseCall):
    args: list[hlil.Expr]

    @staticmethod
    def src_superclass():
        return 'hlil.Call'

    @staticmethod
    def first_token():
        return '&_swift_bridgeObjectRelease'

    @staticmethod
    def match():
        (ok, vars) = ideco.eval_tmpl('&_swift_bridgeObjectRelease(${args*<, >})')
        return ok

    def __repr__(self):
        if self.args != 'INVALID' and len(self.args) > 0:
            return ideco.eval_repr('${args[0]}')
        else:
            return ideco.eval_repr('')

class DirectReleaseCall(ReleaseCall):
    args: list[hlil.Expr]

    @staticmethod
    def src_superclass():
        return 'hlil.Call'

    @staticmethod
    def first_token():
        return '_swift_bridgeObjectRelease'

    @staticmethod
    def match():
        (ok, vars) = ideco.eval_tmpl('_swift_bridgeObjectRelease(${args*<, >})')
        return ok

    def __repr__(self):
        if self.args != 'INVALID' and len(self.args) > 0:
            return ideco.eval_repr('${args[0]}')
        else:
            return ideco.eval_repr('')

class IndirectRetainCall(RetainCall):
    args: list[hlil.Expr]

    @staticmethod
    def src_superclass():
        return 'hlil.Call'

    @staticmethod
    def first_token():
        return '&_swift_bridgeObjectRetain'

    @staticmethod
    def match():
        (ok, vars) = ideco.eval_tmpl('&_swift_bridgeObjectRetain(${args*<, >})')
        return ok

    def __repr__(self):
        if self.args != 'INVALID' and len(self.args) > 0:
            return ideco.eval_repr('${args[0]}')
        else:
            return ideco.eval_repr('')

class DirectRetainCall(RetainCall):
    args: list[hlil.Expr]

    @staticmethod
    def src_superclass():
        return 'hlil.Call'

    @staticmethod
    def first_token():
        return '_swift_bridgeObjectRetain'

    @staticmethod
    def match():
        (ok, vars) = ideco.eval_tmpl('_swift_bridgeObjectRetain(${args*<, >})')
        return ok

    def __repr__(self):
        if self.args != 'INVALID' and len(self.args) > 0:
            return ideco.eval_repr('${args[0]}')
        else:
            return ideco.eval_repr('')

class VoidReleaseCall(hlil.Stmt):
    release_call: ReleaseCall

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'

    @staticmethod
    def first_token():
        return 'type:ReleaseCall'

    @staticmethod
    def match():
        return ideco.eval_tmpl('$release_call')[0]

    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class VoidRetainCall(hlil.Stmt):
    retain_call: RetainCall

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'

    @staticmethod
    def first_token():
        return 'type:RetainCall'

    @staticmethod
    def match():
        return ideco.eval_tmpl('$retain_call')[0]

    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class ReturnRelease(hlil.Return):
    release: ReleaseCall

    @staticmethod
    def src_superclass():
        return 'hlil.Return'

    @staticmethod
    def first_token():
        return 'return'

    @staticmethod
    def match():
        return ideco.eval_tmpl('return $release')[0]

    def init(self):
        func = ideco.get_function()
        ideco.set_type(func, 'void')

    def __repr__(self):
        return ideco.eval_repr('`return`:green')
