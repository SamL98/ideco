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

class MethodCall(hlil.Call):
    receiver: hlil.Expr
    method: hlil.Expr
    args: list[hlil.Expr]

    def __repr__(self):
        return ideco.eval_repr('$receiver.%s(${args*<, >})' % self.method.name)

#class DictionaryIteratorConstruct(hlil.Stmt):
#    dt: base.DataType
#    iterator: hlil.Var
#    make_iter_method: hlil.Symbol
#    dict: hlil.Expr

#    @staticmethod
#    def match():
#        return ideco.eval_tmpl('$dt $iterator = $make_iter_method($dict)')[0]

#    @staticmethod
#    def first_token():
#        return '<&std..collections..hash..map..HashMap<K> as core..iter..traits..collect..IntoIterator>::into_iter'

#    @staticmethod
#    def src_superclass():
#        return 'hlil.VarInit'

#    def init(self):
#        self.make_iter_method.set_attr('name', 'into_iter')

#        output = self.iterator
#        rhs = ideco.make_node('rust.MethodCall', 0, {
#            'receiver': self.dict,
#            'method': self.make_iter_method,
#            'args': [],
#        })

#        self.set_attr('output', output)
#        self.set_attr('rhs', rhs)

#        tuple_type = '(%s, %s)' % (key_type, val_type)
#        opt_type = 'Option<%s>' % tuple_type
#        iter_type = 'Iterator<%s>' % tuple_type
#        dict_type = 'HashMap<%s, %s>' % (key_type, val_type)

#        ideco.set_type(self.iterator, iter_type)
#        ideco.set_type(self.dict, dict_type)

#        def match_iter_copy():
#            (ok, vars) = ideco.eval_tmpl('''
#$dt $dst
#_memcpy(&$dst, &$src, $size)''')
#            return ok and vars['src'].data_type.name == iter_type

#        def __repr___iter_copy(self):
#            return ideco.eval_repr('')

#        def init_iter_copy(self):
#            ideco.set_type(self.dst, iter_type)
#            ideco.propagate(self.dst.node_index, self.src.node_index)

#        iter_copy = type(
#            '%s_iter_copy' % iter_type,
#            (hlil.Stmt,),
#            {
#                'match': match_iter_copy,
#                '__repr__': __repr___iter_copy,
#                'init': init_iter_copy,
#                'first_token': lambda: 'type:DataType',
#                'is_hidden': lambda: True,
#            }
#        )

#        iter_copy.__annotations__['dt'] = base.DataType
#        iter_copy.__annotations__['dst'] = hlil.Var
#        iter_copy.__annotations__['src'] = hlil.Var
#        iter_copy.__annotations__['size'] = hlil.Expr

#        def match_opt_nil_check():
#            (ok, vars) = ideco.eval_tmpl('''
#if ($opt == 0x0) {
#    ^$body
#}''')
#            return ok and vars['opt'].data_type.name == opt_type

#        def __repr___opt_nil_check(self):
#            return ideco.eval_repr('''
#`if`:green ($opt.is_none()) {
#    $body
#}''')

#        def init_opt_nil_check(self):
#            pass

#        opt_nil_check = type(
#            '%s_opt_nil_check' % opt_type,
#            (hlil.Stmt,),
#            {
#                'match': match_opt_nil_check,
#                '__repr__': __repr___opt_nil_check,
#                'init': init_opt_nil_check,
#                'first_token': lambda: 'type:DataType',
#            }
#        )

#        opt_nil_check.__annotations__['opt'] = hlil.Var
#        opt_nil_check.__annotations__['body'] = hlil.Block

#        def match_iter_next():
#            (ok, vars) = ideco.eval_tmpl('$dt $tup = $next_method(&$iterator)')
#            return ok and vars['iterator'].data_type.name == iter_type

#        def __repr___iter_next(self):
#            return ideco.eval_repr('#%d = #%d' % (self.output.node_index, self.rhs.node_index))

#        def init_iter_next(self):
#            ideco.set_type(self.tup, opt_type)
#            self.next_method.set_attr('name', 'next')

#            output = self.tup
#            rhs = ideco.make_node('swift.MethodCall', 0, {
#                'receiver': self.iterator,
#                'method': self.next_method,
#                'args': [],
#            })

#            self.set_attr('output', output)
#            self.set_attr('rhs', rhs)

#        iter_next = type(
#            '%s_iter_next' % iter_type,
#            (hlil.Stmt,),
#            {
#                'match': match_iter_next,
#                '__repr__': __repr___iter_next,
#                'init': init_iter_next,
#                'first_token': lambda: '&Dictionary.Iterator.next()',
#                'src_superclass': lambda: 'hlil.CallStmt',
#            }
#        )

#        iter_next.__annotations__['dt'] = base.DAtaType
#        iter_next.__annotations__['tup'] = hlil.Var
#        iter_next.__annotations__['iterator'] = hlil.Var
#        iter_next.__annotations__['next_method'] = hlil.Symbol

#        def match_loop():
#            ok, vars = ideco.eval_tmpl('''
#%s $iterator = $dict.into_iter()
#loop {
#    %s $opt = $iterator.next()
#    if ($opt.is_none()) {
#        break
#    }
#    ${body_stmts*<NEWLINE>}
#}''' % (iter_type, opt_type, tuple_type))
#            return ok

#        def __repr___loop(self):
#            return ideco.eval_repr('''
#`for`:green ($key, $val) in $dict {
#    #%d
#}''' % self.body.node_index)

#        def init_loop(self):
#            body_seq_idx = ideco.make_seq([e.node_index for e in self.body_stmts])
#            body = ideco.make_node('hlil.Block', 0, {'stmts': body_seq_idx})
#            self.set_attr('body', body)

#        loop = type(
#            '%s_loop' % dict_type,
#            (hlil.Stmt,),
#            {
#                'match': match_loop,
#                '__repr__': __repr___loop,
#                'init': init_loop,
#                'first_token': lambda: iter_type,
#            }
#        )

#        loop.__annotations__['iterator'] = hlil.Var
#        loop.__annotations__['dict'] = hlil.Var
#        loop.__annotations__['opt'] = hlil.Var
#        loop.__annotations__['body_stmts'] = list[hlil.Stmt]

#        def match_count():
#            ok, vars = ideco.eval_tmpl('std::collections::hash::map::HashMap<K>::len($dict)')
#            return ok and vars['dict'].data_type.name == dict_type

#        def __repr___count(self):
#            return ideco.eval_repr('$dict.len()')

#        def init_count(self):
#            pass

#        count = type(
#            '%s_count' % dict_type,
#            (hlil.Stmt,),
#            {
#                'match': match_count,
#                '__repr__': __repr___count,
#                'init': init_count,
#                'first_token': lambda: 'std::collections::hash::map::HashMap<K>::len',
#                'src_superclass': lambda: 'hlil.Call',
#            }
#        )

#        count.__annotations__['dict'] = hlil.Var

#        # iter_decl_assign = make_decl_assign_rule(iter_type)
#        # opt_decl_assign = make_decl_assign_rule(opt_type)

#        module_name = __file__.split('/')[-1].strip('.py')
#        ideco.register(module_name, iter_copy)
#        ideco.register(module_name, iter_next)
#        ideco.register(module_name, opt_copy)
#        ideco.register(module_name, opt_nil_check)
#        ideco.register(module_name, tup_copy_rev)
#        ideco.register(module_name, iter_decl_assign)
#        ideco.register(module_name, opt_decl_assign)
#        ideco.register(module_name, loop)
#        ideco.register(module_name, count)

#    def __repr__(self):
#        return ideco.eval_repr('#%d = #%d' % (self.output.node_index, self.rhs.node_index))
