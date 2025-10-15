import ideco
import hlil
import base

class MethodCall(hlil.Call):
    receiver: hlil.Expr
    method: hlil.Expr
    args: list[hlil.Expr]

    def __repr__(self):
        return ideco.eval_repr('$receiver.%s(${args*<, >})' % self.method.name)

class SwiftString(hlil.Expr):
    pass

class StringLiteral(SwiftString):
    literal: hlil.Expr
    size: hlil.Expr

    @staticmethod
    def match():
        return ideco.eval_tmpl('&String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)($literal, $size, 0x1)')[0]

    @staticmethod
    def src_superclass():
        return 'hlil.Call'

    @staticmethod
    def first_token():
        return '&String.init(_builtinStringLiteral:utf8CodeUnitCount:isASCII:)'

    def __repr__(self):
        if self.size == 0:
            return ideco.eval_repr('`""`:purple')
        else:
            return ideco.eval_repr('$literal')

    @staticmethod
    def priority():
        return 10

class StringLiteralInit(hlil.VarInit):
    dt: hlil.DataType
    output: hlil.Var
    rhs: StringLiteral

    def init(self):
        ideco.set_type(self.output, 'String')
        ideco.set_type(self.rhs, 'String')

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('$dt $output = $rhs')
        return ok and vars['dt'].name != 'String'

    @staticmethod
    def first_token():
        return 'type:DataType'

    @staticmethod
    def src_superclass():
        return 'hlil.VarInit'

    # @staticmethod
    # def is_hidden():
    #     return True

    # def __repr__(self):
    #     return ideco.eval_repr('')

    @staticmethod
    def priority():
        return int(1e5)

# NOTE: We want this to happen *after* the Use of the value type is matched.
class StringAssignProp(hlil.Stmt):
    output: hlil.Var
    rhs: StringLiteral

    def init(self):
        ideco.propagate(self.output.node_index, self.rhs.node_index)

    @staticmethod
    def match():
        return ideco.eval_tmpl('String $output = $rhs')[0]

    @staticmethod
    def first_token():
        return 'String'

    @staticmethod
    def src_superclass():
        return 'swift.StringLiteralInit'

    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class StringInterpInit(hlil.VarInit):
    dt: base.DataType
    output: hlil.Var
    rhs: hlil.Var

    def init(self):
        ideco.set_type(self.output, 'String')

    @staticmethod
    def first_token():
        return 'type:DataType'

    @staticmethod
    def match():
        return ideco.eval_tmpl('$dt $output = &String.init(stringInterpolation:)($rhs)')[0]

    @staticmethod
    def src_superclass():
        return 'hlil.VarInit'

    @staticmethod
    def __repr__(self):
        return ideco.eval_repr('${output.data_type} $output = String($rhs)')

class InterpOp(hlil.Stmt):
    pass

class LiteralInterpOp(InterpOp):
    interp_obj: hlil.Expr
    literal: StringLiteral

    @staticmethod
    def match():
        return ideco.eval_tmpl('&DefaultStringInterpolation.appendLiteral(_:)(&$interp_obj, $literal)')[0]

    @staticmethod
    def first_token():
        return '&DefaultStringInterpolation.appendLiteral'

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'

    def __repr__(self):
        return ideco.eval_repr('$interp_obj.append($literal)')

class ObjInterpOp(InterpOp):
    interp_obj: hlil.Expr
    obj: hlil.Expr
    other_args: list[hlil.Expr]

    @staticmethod
    def match():
        return ideco.eval_tmpl('&DefaultStringInterpolation.appendInterpolation<A>(_:)(&$interp_obj, &$obj, ${other_args*<, >})')[0]

    @staticmethod
    def first_token():
        return '&DefaultStringInterpolation.appendInterpolation'

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'

    def __repr__(self):
        return ideco.eval_repr('$interp_obj.append($obj)')

class InterpStringLiteral(SwiftString):
    pieces: list[hlil.Expr]

    def __repr__(self):
        # return ideco.eval_repr('${pieces*<>}')
        s = ideco.eval_repr('`"`:purple')

        for piece in self.pieces:
            if piece.node_type == 'StringLiteral':
                if piece.size > 1:
                    s += ideco.eval_repr('`%s`:purple' % piece.literal.orig_str)
            else:
                s += ideco.eval_repr('`\\(`:purple')
                s += ideco.eval_repr('#%d' % piece.node_index)
                s += ideco.eval_repr('`)`:purple')

        s += ideco.eval_repr('`"`:purple')
        return s

class InterpConstruct(hlil.VarInit):
    interp_dt: base.DataType
    string_dt: base.DataType
    interp_obj: hlil.Var
    output: hlil.Var
    total_len: hlil.Expr
    interp_count: hlil.Expr
    ops: list[InterpOp]
    leftovers: list[hlil.Stmt]

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
$interp_dt $interp_obj = &DefaultStringInterpolation.init(literalCapacity:interpolationCount:)($total_len, $interp_count)
${ops*~<NEWLINE>}String $output = String($interp_obj)
''')[0]

    @staticmethod
    def first_token():
        return 'type:DataType'

    def init(self):
        ideco.set_type(self.output, 'String')
        pieces = []

        # TODO: Make it so that we don't need to get the index first.
        for op in self.ops:
            if op.node_type == 'LiteralInterpOp':
                pieces.append(op.literal)
            else:
                pieces.append(op.obj)

        # TODO: Get the address.
        rhs = ideco.make_node('swift.InterpStringLiteral', 0, {'pieces': pieces})
        self.set_attr('rhs', rhs)

    def __repr__(self):
        rhs_idx = self.rhs.node_index
        return ideco.eval_repr('${output.data_type} $output = #%d' % rhs_idx)

    @staticmethod
    def priority():
        return int(1e4)

class InterpConstructProp(hlil.Stmt):
    output: hlil.Var
    rhs: InterpStringLiteral

    def init(self):
        ideco.propagate(self.output.node_index, self.rhs.node_index)

    @staticmethod
    def match():
        return ideco.eval_tmpl('String $output = $rhs')[0]

    @staticmethod
    def first_token():
        return 'String'

    @staticmethod
    def src_superclass():
        return 'swift.InterpConstruct'

    @staticmethod
    def is_hidden():
        return True

    def __repr__(self):
        return ideco.eval_repr('')

class Print(hlil.CallStmt):
    dt1: base.DataType
    dt2: base.DataType
    dt3: base.DataType
    dt4: base.DataType
    init_arr: hlil.Expr
    final_arr: hlil.Expr
    args: list[hlil.Expr]
    buf: hlil.Expr
    string_to_print: SwiftString
    separator: hlil.Expr
    terminator: hlil.Expr

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
$dt1 $init_arr = &_allocateUninitializedArray<A>(_:)(${args*<, >})
$buf[0x3] = type metadata for String
*($buf) = $string_to_print
$dt2 $final_arr = &_finalizeUninitializedArray<A>(_:)($init_arr)
$dt3 $separator = &default argument 1 of print(_:separator:terminator:)()
$dt4 $terminator = &default argument 2 of print(_:separator:terminator:)()
&print(_:separator:terminator:)($final_arr, $separator, $terminator)
''')[0]

    @staticmethod
    def first_token():
        return 'type:DataType'

    def __repr__(self):
        return ideco.eval_repr('print($string_to_print)')

def make_decl_assign_rule(dt):
    def match_decl_assign():
        (ok, vars) = ideco.eval_tmpl('''
%s $var
$var = $expr''' % dt)
        return ok and vars['var'].data_type.name == dt

    def __repr___decl_assign(self):
        return ideco.eval_repr('%s $var = $expr' % dt)

    def init_decl_assign(self):
        pass

    decl_assign = type(
        '%s_decl_assign' % dt,
        (hlil.Stmt,),
        {
            'match': match_decl_assign,
            '__repr__': __repr___decl_assign,
            'init': init_decl_assign,
            'first_token': lambda: dt,
        }
    )

    decl_assign.__annotations__['var'] = hlil.Var
    decl_assign.__annotations__['expr'] = MethodCall
    return decl_assign

class AddWithOverflowCheck(hlil.Stmt):
    dt: base.DataType
    v: hlil.Var
    output: hlil.Var
    op_lhs: hlil.Var
    op_rhs: hlil.Var

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
$dt $v
$v.0x0 = add_overflow($op_lhs, $op_rhs)
if ($v.0x0 && 0x1 != 0x0) {
    trap(0x6)
}
$output = $op_lhs + $op_rhs''')[0]

    def init(self):
        lhs_type = self.op_lhs.data_type.name
        ideco.set_type(self.op_rhs, lhs_type)

    def __repr__(self):
        return ideco.eval_repr('$output = $op_lhs + $op_rhs')

    @staticmethod
    def first_token():
        return 'type:DataType'

class Tuple2(hlil.Expr):
    field1: hlil.Var
    field2: hlil.Var

    def __repr__(self):
        return ideco.eval_repr('($field1, $field2)')

## class StringIteratorConstruct(hlil.Stmt):
##     make_iter_method: hlil.Expr
##     args: list[hlil.Expr]

##     @staticmethod
##     def match():
##         return ideco.eval_tmpl('$make_iter_method(${args*<, >})')[0]

##     @staticmethod
##     def first_token():
##         return 'String.makeIterator()'

##     @staticmethod
##     def src_superclass():
##         return 'hlil.Call'

##     def init(self):
##         self.make_iter_method.set_attr('name', 'makeIterator')

##         lhs = self.iterator
##         rhs = ideco.make_node('swift.MethodCall', 0, {
##             'receiver': self.dict,
##             'method': self.make_iter_method,
##             'args': [],
##         })

##         self.set_attr('lhs', lhs)
##         self.set_attr('rhs', rhs)

class DictionaryIteratorConstruct(hlil.Stmt):
    make_iter_method: hlil.Expr
    iterator: hlil.Var
    dict: hlil.Var
    key_type: hlil.Symbol
    val_type: hlil.Symbol
    proto: hlil.Symbol

    @staticmethod
    def match():
        return ideco.eval_tmpl('&$make_iter_method(&$iterator, $dict, $key_type, $val_type, $proto)')[0]

    @staticmethod
    def first_token():
        return '&Dictionary.makeIterator()'

    @staticmethod
    def src_superclass():
        return 'hlil.CallStmt'

    def init(self):
        self.make_iter_method.set_attr('name', 'makeIterator')

        output = self.iterator
        rhs = ideco.make_node('swift.MethodCall', 0, {
            'receiver': self.dict,
            'method': self.make_iter_method,
            'args': [],
        })

        self.set_attr('output', output)
        self.set_attr('rhs', rhs)

        prefix = 'type metadata for '
        key_type = self.key_type.name[len(prefix):]
        val_type = self.val_type.name[len(prefix):]

        # tuple_type = 'Tuple_of_%s_%s' % (key_type, val_type)
        # opt_type = 'Optional_of_%s' % tuple_type
        # iter_type = 'Iterator_of_%s' % tuple_type
        # dict_type = 'Dictionary_of_%s_%s' % (key_type, val_type)

        tuple_type = '(%s, %s)' % (key_type, val_type)
        opt_type = '%s?' % tuple_type
        iter_type = 'Iterator<%s>' % tuple_type
        dict_type = '[%s:%s]' % (key_type, val_type)

        ideco.set_type(self.iterator, iter_type)
        ideco.set_type(self.dict, dict_type)

        def match_iter_copy():
            (ok, vars) = ideco.eval_tmpl('&_memcpy(&$dst, &$src, $size)')
            return ok and vars['src'].data_type.name == iter_type

        def __repr___iter_copy(self):
            return ideco.eval_repr('')

        def init_iter_copy(self):
            ideco.set_type(self.dst, iter_type)
            ideco.propagate(self.dst.node_index, self.src.node_index)

        iter_copy = type(
            '%s_iter_copy' % iter_type,
            (hlil.Stmt,),
            {
                'match': match_iter_copy,
                '__repr__': __repr___iter_copy,
                'init': init_iter_copy,
                'first_token': lambda: '&_memcpy',
                'src_superclass': lambda: 'hlil.CallStmt',
                'is_hidden': lambda: True,
            }
        )

        iter_copy.__annotations__['dst'] = hlil.Var
        iter_copy.__annotations__['src'] = hlil.Var
        iter_copy.__annotations__['size'] = hlil.Expr

        def match_opt_copy():
            (ok, vars) = ideco.eval_tmpl('''
$dt1 $dst1 = $opt.0x0
$dt2 $dst2 = $opt.0x8''')
            return ok and vars['opt'].data_type.name == opt_type

        def __repr___opt_copy(self):
            return ideco.eval_repr('${dst1.data_type} ($dst1, $dst2) = $opt')

        def init_opt_copy(self):
            ideco.set_type(self.dst1, tuple_type)
            ideco.set_type(self.dst2, val_type)

        opt_copy = type(
            '%s_opt_copy' % opt_type,
            (hlil.Stmt,),
            {
                'match': match_opt_copy,
                '__repr__': __repr___opt_copy,
                'init': init_opt_copy,
                'first_token': lambda: 'type:DataType',
            }
        )

        opt_copy.__annotations__['dt1'] = base.DataType
        opt_copy.__annotations__['dt2'] = base.DataType
        opt_copy.__annotations__['dst1'] = hlil.Var
        opt_copy.__annotations__['dst2'] = hlil.Var
        opt_copy.__annotations__['opt'] = hlil.Var

        def match_tup_copy_rev():
            (ok, vars) = ideco.eval_tmpl('''
$dt1 $dst1 = $src1
$dt2 $dst2 = $src2''')
            return ok and vars['src2'].data_type.name == tuple_type

        def __repr___tup_copy_rev(self):
            # return ideco.eval_repr('${dst2.data_type} ($dst2, $dst1) = ($src2, $src1)')
            # return ideco.eval_repr('${dst2.data_type} #%d = #%d' % (self.dst.node_index, self.src.node_index))
            return ideco.eval_repr('')

        def init_tup_copy_rev(self):
            ideco.set_type(self.dst2, tuple_type)
            ideco.set_type(self.dst1, val_type)

            dst = ideco.make_node('swift.Tuple2', 0, {'field1': self.dst2, 'field2': self.dst1})
            src = ideco.make_node('swift.Tuple2', 0, {'field1': self.src2, 'field2': self.src1})

            self.set_attr('dst', dst)
            self.set_attr('src', src)

            ideco.propagate(dst.node_index, src.node_index)
            ideco.propagate(self.dst1.node_index, self.src1.node_index)
            ideco.propagate(self.dst2.node_index, self.src2.node_index)

        tup_copy_rev = type(
            '%s_tup_copy_rev' % opt_type,
            (hlil.Stmt,),
            {
                'match': match_tup_copy_rev,
                '__repr__': __repr___tup_copy_rev,
                'init': init_tup_copy_rev,
                'first_token': lambda: 'type:DataType',
                'is_hidden': lambda: True,
            }
        )

        tup_copy_rev.__annotations__['dt1'] = base.DataType
        tup_copy_rev.__annotations__['dt2'] = base.DataType
        tup_copy_rev.__annotations__['dst1'] = hlil.Var
        tup_copy_rev.__annotations__['dst2'] = hlil.Var
        tup_copy_rev.__annotations__['src1'] = hlil.Var
        tup_copy_rev.__annotations__['src2'] = hlil.Var

        def match_opt_nil_check():
            (ok, vars) = ideco.eval_tmpl('''
$dt $v
$v.0x0 = $opt.0x10
if ($v.0x0 && 0x1 != 0x0) {
    ^$body
}''')
            return ok and vars['opt'].data_type.name == opt_type

        def __repr___opt_nil_check(self):
            return ideco.eval_repr('''
`if`:green ($opt == `nil`:orange) {
    $body
}''')

        def init_opt_nil_check(self):
            pass

        opt_nil_check = type(
            '%s_opt_nil_check' % opt_type,
            (hlil.Stmt,),
            {
                'match': match_opt_nil_check,
                '__repr__': __repr___opt_nil_check,
                'init': init_opt_nil_check,
                'first_token': lambda: 'type:DataType',
            }
        )

        opt_nil_check.__annotations__['dt'] = base.DataType
        opt_nil_check.__annotations__['v'] = hlil.Var
        opt_nil_check.__annotations__['opt'] = hlil.Var
        opt_nil_check.__annotations__['body'] = hlil.Block

        def match_iter_next():
            (ok, vars) = ideco.eval_tmpl('&$next_method(&$tup, &$iterator, $md)')
            return ok and vars['iterator'].data_type.name == iter_type

        def __repr___iter_next(self):
            return ideco.eval_repr('#%d = #%d' % (self.output.node_index, self.rhs.node_index))

        def init_iter_next(self):
            ideco.set_type(self.tup, opt_type)
            self.next_method.set_attr('name', 'next')

            output = self.tup
            rhs = ideco.make_node('swift.MethodCall', 0, {
                'receiver': self.iterator,
                'method': self.next_method,
                'args': [],
            })

            self.set_attr('output', output)
            self.set_attr('rhs', rhs)

        iter_next = type(
            '%s_iter_next' % iter_type,
            (hlil.Stmt,),
            {
                'match': match_iter_next,
                '__repr__': __repr___iter_next,
                'init': init_iter_next,
                'first_token': lambda: '&Dictionary.Iterator.next()',
                'src_superclass': lambda: 'hlil.CallStmt',
            }
        )

        iter_next.__annotations__['tup'] = hlil.Var
        iter_next.__annotations__['iterator'] = hlil.Var
        iter_next.__annotations__['next_method'] = hlil.Symbol
        iter_next.__annotations__['md'] = hlil.Expr

        def match_loop():
            ok, vars = ideco.eval_tmpl('''
%s $iterator = $dict.makeIterator()
loop {
    %s $opt = $iterator.next()
    %s ($key, $val) = $opt
    if ($opt == nil) {
        break
    }
    ${body_stmts*<NEWLINE>}
}''' % (iter_type, opt_type, tuple_type))
            return ok

        def __repr___loop(self):
            return ideco.eval_repr('''
`for`:green ($key, $val) in $dict {
    #%d
}''' % self.body.node_index)

        def init_loop(self):
            body_seq_idx = ideco.make_seq([e.node_index for e in self.body_stmts])
            body = ideco.make_node('hlil.Block', 0, {'stmts': body_seq_idx})
            self.set_attr('body', body)

        loop = type(
            '%s_loop' % dict_type,
            (hlil.Stmt,),
            {
                'match': match_loop,
                '__repr__': __repr___loop,
                'init': init_loop,
                'first_token': lambda: iter_type,
            }
        )

        loop.__annotations__['iterator'] = hlil.Var
        loop.__annotations__['dict'] = hlil.Var
        loop.__annotations__['opt'] = hlil.Var
        loop.__annotations__['key'] = hlil.Var
        loop.__annotations__['val'] = hlil.Var
        loop.__annotations__['body_stmts'] = list[hlil.Stmt]

        def match_count():
            ok, vars = ideco.eval_tmpl('&Dictionary.count.getter($dict, ${md*<, >})')
            return ok and vars['dict'].data_type.name == dict_type

        def __repr___count(self):
            return ideco.eval_repr('$dict.count')

        def init_count(self):
            pass

        count = type(
            '%s_count' % dict_type,
            (hlil.Stmt,),
            {
                'match': match_count,
                '__repr__': __repr___count,
                'init': init_count,
                'first_token': lambda: '&Dictionary.count.getter',
                'src_superclass': lambda: 'hlil.Call',
            }
        )

        count.__annotations__['dict'] = hlil.Var
        count.__annotations__['md'] = list[hlil.Expr]

        iter_decl_assign = make_decl_assign_rule(iter_type)
        opt_decl_assign = make_decl_assign_rule(opt_type)

        module_name = __file__.split('/')[-1].strip('.py')
        ideco.register(module_name, iter_copy)
        ideco.register(module_name, iter_next)
        ideco.register(module_name, opt_copy)
        ideco.register(module_name, opt_nil_check)
        ideco.register(module_name, tup_copy_rev)
        ideco.register(module_name, iter_decl_assign)
        ideco.register(module_name, opt_decl_assign)
        ideco.register(module_name, loop)
        ideco.register(module_name, count)

    def __repr__(self):
        return ideco.eval_repr('#%d = #%d' % (self.output.node_index, self.rhs.node_index))

class SwiftVarProto(hlil.Expr):
    var: hlil.Var

    def __repr__(self):
        return ideco.eval_repr('$var: %s' % self.var.data_type.name)

class SwiftFunction(hlil.Function):
    output_type: base.DataType
    name: hlil.Symbol
    inputs: list[hlil.VarProto]
    body: hlil.Block

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('''
$output_type $name(${inputs*<, >}) {
    ^$body
}''')
        # breakpoint()
        return ok

    def init(self):
        swift_inputs = [ideco.make_node('swift.SwiftVarProto', 0, {'var': i.var}) for i in self.inputs]
        self.set_attr('swift_inputs', swift_inputs)
        ideco.set_type(self, self.output_type.name)

    def __repr__(self):
        func_name = self.name.name

        if '(' in func_name:
            func_name = func_name[:func_name.index('(')]

        s = ideco.eval_repr('`func`:green %s(' % func_name)

        for (i, param) in enumerate(self.swift_inputs):
            # s += ideco.eval_repr('`%s`:blue: %s' % (param.name, param.data_type.name))
            s += ideco.eval_repr('#%d' % param.node_index)

            if i < len(self.swift_inputs) - 1:
                s += ideco.eval_repr(', ')

        s += ideco.eval_repr('''
) -> ${self.data_type} {
    $body
}''')
        return s

    @staticmethod
    def first_token():
        return 'type:Function'

    @staticmethod
    def src_superclass():
        return 'hlil.Function'

class SwiftVarDecl(hlil.VarDecl):
    dt: base.DataType
    var: hlil.Var

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('$dt $var')
        return ok and vars['var'].data_type.name in {'Int', 'String'}

    def __repr__(self):
        return ideco.eval_repr('`var`:green $var: ${var.data_type}')

    @staticmethod
    def first_token():
        return 'type:VarDecl'

    @staticmethod
    def src_superclass():
        return 'hlil.VarDecl'
