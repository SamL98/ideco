import ideco
import hlil
import base

class ValueType(object):
    @staticmethod
    def new_type(node_name, layout, vars):
        ident = layout[0][0]

        def match_use():
            tmpl = ', '.join(['$%s' % n for (n, _) in layout])
            (ok, vars) = ideco.eval_tmpl(tmpl)
                    # vars[ident].data_type.name in {node_name, 'String'} and \
            ok = ok and \
                    vars[ident].data_type.name == node_name and \
                    all([vars[n].data_type.name == t.name for (n, t) in layout[1:]])

            if ok:
                parents = set(ideco.get_parents(vars[ident].node_index))
                ok &= all([len(set(ideco.get_parents(vars[n].node_index)).intersection(parents)) > 0 for (n, _) in layout[1:]])

            return ok

        def __repr___use(self):
            return ideco.eval_repr('$%s' % ident)

        def match_partial_use():
            return ideco.eval_tmpl('%s' % vars[1])[0]

        def __repr___partial_use(self):
            return ideco.eval_repr('%s.field1' % vars[0])

        def match_copy():
            tmpl = '\n'.join(['$dst%d = $%s' % (i, n) for (i, (n, _)) in enumerate(layout)])
            (ok, vars) = ideco.eval_tmpl(tmpl)
            return ok and vars[ident].data_type.name == node_name

        def __repr___copy(self):
            return ideco.eval_repr('$dst0 = $%s' % ident)

        def init_copy(self):
            ideco.set_type(self.dst0, node_name)

        def match_copy_struct():
            lines = []
            size = 0
            for (i, (n, t)) in enumerate(layout):
                lines.append('$dst.%s = $%s' % (hex(size), n))
                size += t.size
            tmpl = '\n'.join(lines)
            (ok, vars) = ideco.eval_tmpl(tmpl)
            if ok and len(lines) == 4:
                breakpoint()
            return ok and vars[ident].data_type.name == node_name

        def __repr___copy_struct(self):
            return ideco.eval_repr('$dst = $%s' % ident)

        def init_copy_struct(self):
            ideco.set_type(self.dst, node_name)

        def match_decl_copy_struct():
            lines = []
            size = 0
            for (i, (_, t)) in enumerate(layout):
                lines.append('$dt%d $dst%d = $src.%s' % (i, i, hex(size)))
                size += t.size
            tmpl = '\n'.join(lines)
            (ok, vars) = ideco.eval_tmpl(tmpl)
            return ok and vars['src'].data_type.name == node_name

        def __repr___decl_copy_struct(self):
            return ideco.eval_repr('${dst0.data_type} $dst0 = $src')

        def init_decl_copy_struct(self):
            ideco.set_type(self.dst0, node_name)

        def match_partial_copy():
            tmpl = '$dst0 = $%s' % ident
            (ok, vars) = ideco.eval_tmpl(tmpl)
            return ok and vars[ident].data_type.name == node_name

        def __repr___partial_copy(self):
            return ideco.eval_repr('')

        def init_partial_copy(self):
            ideco.set_type(self.dst0, node_name)
            ideco.propagate(self.dst0.node_index, getattr(self, ident).node_index)

        def match_partial_copy_decl():
            tmpl = '$dt $dst0 = $%s' % ident
            (ok, vars) = ideco.eval_tmpl(tmpl)
            return ok and vars[ident].data_type.name == node_name

        def match_decl():
            tmpl = '\n'.join(['$dt%d $var%d = $%s' % (i, i, n) for (i, (n, _)) in enumerate(layout)])
            (ok, vars) = ideco.eval_tmpl(tmpl)
            return ok and vars[ident].data_type.name == node_name

        def __repr___decl(self):
            return ideco.eval_repr('${var0.data_type} $var0 = $%s' % ident)

        def init_decl(self):
            ideco.set_type(self.var0, node_name)

        use = type(
            '%s_Use' % node_name,
            (hlil.Expr,),
            {
                'match': match_use,
                '__repr__': __repr___use,
                'first_token': lambda: 'type:Var'
            }
        )
        # partial_use = type(
        #     '%s_PartialUse' % node_name,
        #     (hlil.Expr,),
        #     {
        #         'match': match_partial_use,
        #         '__repr__': __repr___partial_use,
        #         'first_token': lambda: 'type:Var',
        #         'src_superclass': lambda: 'hlil.Var',
        #         'priority': lambda: 50000000,
        #     }
        # )
        copy = type(
            '%s_Copy' % node_name,
            (hlil.Stmt,),
            {
                'match': match_copy,
                '__repr__': __repr___copy,
                'init': init_copy,
                'first_token': lambda: 'type:Expr',
                'priority': lambda: 10,
            }
        )
        copy_struct = type(
            '%s_CopyStruct' % node_name,
            (hlil.Stmt,),
            {
                'match': match_copy_struct,
                '__repr__': __repr___copy_struct,
                'init': init_copy_struct,
                'first_token': lambda: 'type:Expr',
                'priority': lambda: 10,
            }
        )
        decl_copy_struct = type(
            '%s_DeclCopyStruct' % node_name,
            (hlil.Stmt,),
            {
                'match': match_decl_copy_struct,
                '__repr__': __repr___decl_copy_struct,
                'init': init_decl_copy_struct,
                'first_token': lambda: 'type:DataType',
                'priority': lambda: 10,
            }
        )
        partial_copy = type(
            '%s_PartialCopy' % node_name,
            (hlil.Stmt,),
            {
                'match': match_partial_copy,
                '__repr__': __repr___partial_copy,
                'init': init_partial_copy,
                'first_token': lambda: 'type:Expr',
                'is_hidden': lambda: True,
                'priority': lambda: int(1e4),
                'src_superclass': lambda: 'base.Stmt',
            }
        )
        partial_copy_decl = type(
            '%s_PartialCopyDecl' % node_name,
            (hlil.Stmt,),
            {
                'match': match_partial_copy_decl,
                '__repr__': __repr___partial_copy,
                'init': init_partial_copy,
                'first_token': lambda: 'type:DataType',
                'is_hidden': lambda: True,
                'priority': lambda: int(1e4),
                'src_superclass': lambda: 'base.Stmt',
            }
        )
        decl = type(
            '%s_Decl' % node_name,
            (hlil.Stmt,),
            {
                'match': match_decl,
                '__repr__': __repr___decl,
                'init': init_decl,
                'first_token': lambda: 'type:DataType'
            }
        )

        for (i, (name, _)) in enumerate(layout):
            use.__annotations__[name] = hlil.Var
            copy.__annotations__[name] = hlil.Var
            copy.__annotations__['dst%d' % i] = hlil.Expr
            copy_struct.__annotations__[name] = hlil.Var
            decl_copy_struct.__annotations__['dst%d' % i] = hlil.Expr
            decl_copy_struct.__annotations__['dt%d' % i] = base.DataType
            decl.__annotations__[name] = hlil.Var
            decl.__annotations__['dt%d' % i] = base.DataType
            decl.__annotations__['var%d' % i] = hlil.Var

        copy_struct.__annotations__['dst'] = hlil.Expr
        decl_copy_struct.__annotations__['src'] = hlil.Var
        partial_copy.__annotations__[ident] = hlil.Var
        partial_copy.__annotations__['dst0'] = hlil.Var
        partial_copy_decl.__annotations__[ident] = hlil.Var
        partial_copy_decl.__annotations__['dst0'] = hlil.Var
        partial_copy_decl.__annotations__['dt'] = base.DataType

        def __repr___type(self):
            return ideco.eval_repr(node_name)

        type_desc = type(node_name, (hlil.DataType,), {'name': node_name, 'layout': layout, '__repr__': __repr___type})
        return (type_desc, [use, copy, copy_struct, decl_copy_struct, partial_copy, partial_copy_decl, decl])

TYPE_COUNTER = 1

def create_value_type(dts, vars):
    global TYPE_COUNTER
    layout = [('field%d' % (i + 1), dt) for (i, dt) in enumerate(dts)]
    # (dt, counter) = ideco.get_data_type(layout)
    module_name = __file__.split('/')[-1].strip('.py')

    (dt, new_rules) = ValueType.new_type('Type_%d' % TYPE_COUNTER, layout, vars)
    TYPE_COUNTER += 1

    ideco.register(module_name, dt)

    for rule in new_rules:
        ideco.register(module_name, rule)

    return dt

class DeclareSmallValueType2(hlil.VarInit):
    dt1: base.DataType
    dt2: base.DataType
    output: hlil.Var
    var2: hlil.Var
    rhs: hlil.Call

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
$dt1 $output
$dt2 $var2
$output, $var2 = $rhs''')[0]

    def init(self):
        dt1 = self.output.data_type
        dt2 = self.var2.data_type

        dt = create_value_type([dt1, dt2], [self.output.name, self.var2.name])
        ideco.set_type(self.output, dt.name)

    @staticmethod
    def first_token():
        return 'type:DataType'

class DeclareLargeValueType(hlil.VarDecl):
    dt: base.DataType
    var: hlil.Var
    size: hlil.Expr

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
$dt $var
&_memset(&$var, 0x0, $size)''')[0]

    def init(self):
        global TYPE_COUNTER
        dt_name = 'Type_%d' % TYPE_COUNTER

        def __repr___type(self):
            return ideco.eval_repr(dt_name)

        dt = type(dt_name, (hlil.DataType,), {'name': dt_name, '__repr__': __repr___type})
        TYPE_COUNTER += 1

        ideco.set_type(self.var, dt.name)

    @staticmethod
    def first_token():
        return 'type:DataType'
