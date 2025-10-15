from dataclasses import dataclass
from base import Node, Expr, Stmt, DataType
import ideco

class Opcode(Node):
    pass

class Var(Expr):
    def __repr__(self):
        return ideco.eval_repr('`$self`:blue')

class Symbol(Expr):
    pass

class VarProto(Expr):
    var: Var

    def __repr__(self):
        return ideco.eval_repr('${var.data_type} $var')

class Function(Node):
    name: Symbol
    body: Stmt
    inputs: list[VarProto]

    def __repr__(self):
        return ideco.eval_repr('''
${self.data_type} $name(${inputs*<, >}) {
    $body
}''')

class Block(Stmt):
    stmts: list[Stmt]

    def __repr__(self):
        return ideco.eval_repr('${stmts*<NEWLINE>}')

class If(Stmt):
    condition: Expr
    body: Stmt

    def __repr__(self):
        return ideco.eval_repr('''
`if`:green ($condition) {
    $body
}''')

class IfElse(Stmt):
    condition: Expr
    if_body: Stmt
    else_body: Stmt

    def __repr__(self):
        return ideco.eval_repr('''
`if`:green ($condition) {
    $if_body
}
`else`:green {
    $else_body
}''')

class Switch(Stmt):
    condition: Expr
    cases: list[Stmt]

    def __repr__(self):
        return ideco.eval_repr('''
`switch`:green ($condition) {
    ${cases*<NEWLINE>}
}''')

class Case(Stmt):
    labels: list[Expr]
    body: Stmt

    def __repr__(self):
        return ideco.eval_repr('''
`case`:green ${labels*<, >}:
    $body''')

class DoWhile(Stmt):
    condition: Expr
    body: Stmt

    def __repr__(self):
        return ideco.eval_repr('''
`do`:green {
    $body
} `while`:green ($condition)''')

class While(Stmt):
    condition: Expr
    body: Stmt

    def __repr__(self):
        return ideco.eval_repr('''
`while`:green ($condition) {
    $body
}''')

class For(Stmt):
    init: Expr
    condition: Expr
    modify: Expr
    body: Stmt

    def __repr__(self):
        return ideco.eval_repr('''
`for`:green ($init; $condition; $modify) {
    $body
}''')

class Nop(Stmt):
    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class Trap(Stmt):
    num: Expr

    # @staticmethod
    # def is_hidden():
    #     return True

    def __repr__(self):
        return ideco.eval_repr('`trap`:red($num)')

class NoReturn(Stmt):
    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class VarDecl(Stmt):
    var: Expr

    def __repr__(self):
        return ideco.eval_repr('${var.data_type} $var')

class VarInit(Stmt):
    output: Expr
    rhs: Expr

    @classmethod
    def outputs(cls):
        return ['output']

    def __repr__(self):
        return ideco.eval_repr('${output.data_type} $output = $rhs')

class Assign(Stmt):
    output: Expr
    rhs: Expr

    @classmethod
    def outputs(cls):
        return ['output']

    def __repr__(self):
        return ideco.eval_repr('$output = $rhs')

class ExprList(Expr):
    exprs: list[Expr]

    def __repr__(self):
        return ideco.eval_repr('${exprs*<, >}')

class Intrinsic(Expr):
    op: Opcode
    params: list[Expr]

    def __repr__(self):
        return ideco.eval_repr('$op(${params*<, >})')

class Call(Expr):
    target: Expr
    params: list[Expr]

    def __repr__(self):
        return ideco.eval_repr('$target(${params*<, >})')

class Label(Expr):
    def __repr__(self):
        return ideco.eval_repr('`$self`:yellow')

class Goto(Stmt):
    label: Label

    def __repr__(self):
        return ideco.eval_repr('`goto`:blue $label')

class Jump(Stmt):
    dest: Expr

    def __repr__(self):
        return ideco.eval_repr('`jump`:blue $dest')

class LabelDecl(Stmt):
    label: Label

    def __repr__(self):
        return ideco.eval_repr('$label:')

class CallStmt(Stmt):
    call: Call

    def __repr__(self):
        return ideco.eval_repr('$call')

class Return(Stmt):
    retvals: list[Expr]

    def __repr__(self):
        if len(self.retvals) == 0:
            return ideco.eval_repr('`return`:green')
        else:
            return ideco.eval_repr('`return`:green ${retvals*<, >}')

class Break(Stmt):
    def __repr__(self):
        return ideco.eval_repr('`break`:green')

class Continue(Stmt):
    def __repr__(self):
        return ideco.eval_repr('`continue`:green')

class Int(Expr):
    pass

class Data(Symbol):
    pass

class String(Expr):
    def __repr__(self):
        s = self.orig_str.replace('\n', '\\n').replace('\t', '\\t')
        return ideco.eval_repr('`"%s"`:purple' % s)

class Import(Expr):
    pass

class Deref(Expr):
    operand: Expr

    def __repr__(self):
        return ideco.eval_repr('*($operand)')
        # if ideco.eval_expr('$operand isa hlil.Binary'):
        #     return ideco.eval_repr('*($operand)')
        # else:
        #     return ideco.eval_repr('*$operand')

class Unary(Expr):
    op: Opcode
    operand: Expr

    def __repr__(self):
        return ideco.eval_repr('$op$operand')

class UnaryFunc(Expr):
    op: Opcode
    operand: Expr

    def __repr__(self):
        return ideco.eval_repr('$op($operand)')

class LowPart(Expr):
    src: Expr

    def __repr__(self):
        return ideco.eval_repr('($src).d')

class Binary(Expr):
    op: Opcode
    lhs: Expr
    rhs: Expr

    def __repr__(self):
        return ideco.eval_repr('$lhs $op $rhs')

class BinaryFunc(Expr):
    op: Opcode
    lhs: Expr
    rhs: Expr

    def __repr__(self):
        return ideco.eval_repr('$op($lhs, $rhs)')

class StructAccess(Expr):
    struct: Expr
    field: Expr

    def __repr__(self):
        return ideco.eval_repr('$struct.$field')

class StructFieldDeref(Expr):
    struct: Expr
    field: Expr

    def __repr__(self):
        return ideco.eval_repr('$struct->$field')

class ArrayAccess(Expr):
    array: Expr
    index: Expr

    def __repr__(self):
        return ideco.eval_repr('$array[$index]')
