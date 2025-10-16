import ideco
import hlil
import base

# from rust_demangler import demangle

class RustSymbol(hlil.Symbol):
    sym: hlil.Symbol

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('$sym')
        if not ok or vars['sym'].node_type == 'RustSymbol':
            return False

        comps = vars['sym'].name.split('::')
        last = comps[-1]
        return len(comps) > 1 and len(last) == 17 and last[0] == 'h'

    def init(self):
        n = self.sym.name
        if n.startswith('_'):
            n = n[1:]
        n = n.replace('$LT$', '<')
        n = n.replace('$GT$', '>')
        n = n.replace('$u20$', ' ')
        n = n.replace('$RF$', '&')
        n = n.replace('$SP$', '@')
        n = '::'.join(n.split('::')[:-1])
        self.set_attr('name', n)

    @staticmethod
    def first_token():
        return 'type:Symbol'

    @staticmethod
    def src_superclass():
        return 'hlil.Symbol'

    def __repr__(self):
        return ideco.eval_repr(self.name)

# class AddAssign(hlil.Stmt):
#     func: hlil.Symbol
#     lhs: hlil.Expr
#     rhs: hlil.Expr

#     @staticmethod
#     def match():
#         ok, vars = ideco.eval_tmpl('$func(&$lhs, $rhs)')
#         return ok and 'add_assign' in vars['func'].name.split('::')

#     def __repr__(self):
#         return ideco.eval_repr('$lhs += $rhs')

#     @staticmethod
#     def first_token():
#         return 'type:Symbol'

#     @staticmethod
#     def src_superclass():
#         return 'hlil.CallStmt'

class MethodCall(hlil.Call):
    receiver: hlil.Expr
    method: hlil.Expr
    args: list[hlil.Expr]

    def __repr__(self):
        return ideco.eval_repr('$receiver.%s(${args*<, >})' % self.method.name)

class DictionaryIteratorLoop(hlil.Stmt):
    dt1: base.DataType
    dt2: base.DataType
    dt3: base.DataType
    dt4: base.DataType
    dt5: base.DataType
    dict: hlil.Var
    iterator_: hlil.Var
    foo: hlil.Var
    key: hlil.Var
    val: hlil.Var
    bar: hlil.Var
    opt: hlil.Var
    size: hlil.Expr
    body_stmts: list[hlil.Stmt]
    iter_method: hlil.Symbol
    next_method: hlil.Symbol

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
$dt1 $iterator_ = <&std..collections..hash..map..HashMap<K> as core..iter..traits..collect..IntoIterator>::into_iter($dict)
$dt2 $foo
_memcpy(&$foo, &$iterator_, $size)
loop {
    $dt3 $opt = <std..collections..hash..map..Iter<K> as core..iter..traits..iterator..Iterator>::next(&$foo)
    if ($opt == 0x0) {
        break
    }
    ($dt4, $dt5) ($key, $val) = ($opt, $bar)
    ${body_stmts*<NEWLINE>}
}''')[0]

    @staticmethod
    def first_token():
        return 'type:DataType'

    def __repr__(self):
        return ideco.eval_repr('''
`for`:green ($key, $val) `in`:green $dict {
    #%d
}''' % self.body.node_index)

    def init(self):
        body_seq_idx = ideco.make_seq([e.node_index for e in self.body_stmts])
        body = ideco.make_node('hlil.Block', 0, {'stmts': body_seq_idx})
        self.set_attr('body', body)

        # TODO: Figure out why AddAssign needs to be applied after this.
        def match_add_assign():
            ok, vars = ideco.eval_tmpl('$func(&$lhs, $rhs)')
            return ok and 'add_assign' in vars['func'].name.split('::')

        def __repr___add_assign(self):
            return ideco.eval_repr('$lhs += $rhs')

        def init_add_assign(self):
            pass

        add_assign = type(
            'AddAssign',
            (hlil.Stmt,),
            {
                'match': match_add_assign,
                '__repr__': __repr___add_assign,
                'init': init_add_assign,
                'first_token': lambda: 'type:Symbol',
                'src_superclass': lambda: 'hlil.CallStmt',
            }
        )

        add_assign.__annotations__['func'] = hlil.Symbol
        add_assign.__annotations__['lhs'] = hlil.Var
        add_assign.__annotations__['rhs'] = hlil.Var

        def match_count():
            return ideco.eval_tmpl('std::collections::hash::map::HashMap<K>::len($dict)')[0]

        def __repr___count(self):
            return ideco.eval_repr('$dict.len()')

        def init_count(self):
            pass

        count = type(
            'DictCount',
            (hlil.Stmt,),
            {
                'match': match_count,
                '__repr__': __repr___count,
                'init': init_count,
                'first_token': lambda: 'std::collections::hash::map::HashMap<K>::len',
                'src_superclass': lambda: 'hlil.Call',
            }
        )

        count.__annotations__['dict'] = hlil.Var

        module_name = __file__.split('/')[-1].strip('.py')
        ideco.register(module_name, count)
        ideco.register(module_name, add_assign)
