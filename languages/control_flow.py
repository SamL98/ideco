import ideco
import hlil
import base

class InfiniteLoop(hlil.Stmt):
    body: hlil.Block

    @staticmethod
    def src_superclass():
        return 'hlil.While'

    @staticmethod
    def first_token():
        return 'while'

    @staticmethod
    def match():
        return ideco.eval_tmpl('''
while (0x1) {
    ^$body
}''')[0]

    def __repr__(self):
        return ideco.eval_repr('''
`loop`:green {
    $body
}''')

class ReversedInfiniteLoop(hlil.Stmt):
    loop: InfiniteLoop
    ft: list[hlil.Stmt]

    # @staticmethod
    # def src_superclass():
    #     return 'control_flow.InfiniteLoop'

    @staticmethod
    def first_token():
        return 'loop'

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('''
$loop
${ft*<NEWLINE>}''')
        return ok and \
                len(vars['ft']) < 5 and \
                len(ideco.find_all('hlil.Break', vars['loop'].body)) > 0

    def init(self):
        breaks = ideco.find_all('hlil.Break', self.loop.body)
        ft_blk_idx = ideco.make_seq([e.node_index for e in self.ft])

        for brk in breaks:
            ideco.propagate(brk, ft_blk_idx)

    @staticmethod
    def passthrough():
        return 'loop'

    def __repr__(self):
        breakpoint()

class InvertedLoop(base.Node):
    loop: InfiniteLoop

    @staticmethod
    def is_seq():
        return True

    def seq(self):
        return [self.loop.node_index] + self.ft

    @staticmethod
    def first_token():
        return 'loop'

    # @staticmethod
    # def src_superclass():
    #     return 'control_flow.ReversedInfiniteLoop'

    @staticmethod
    def match():
        ok, vars = ideco.eval_tmpl('$loop')
        return ok and \
                len(ideco.find_all('hlil.Break', vars['loop'].body)) == 0 and \
                len(ideco.find_all('hlil.Return', vars['loop'].body)) == 1

    def init(self):
        ret_idx = ideco.find_all('hlil.Return', self.loop.body)[0]
        ret_seq_idx = ideco.get_parents(ret_idx)[0]
        ret_blk_idx = ideco.get_parents(ret_seq_idx)[0]

        ft = ideco.get_seq(ret_seq_idx)
        self.set_attr('ft', ft)

        brk = ideco.make_node('hlil.Break', 0, {})
        brk_seq_idx = ideco.make_seq([brk.node_index])
        ideco.propagate(ret_seq_idx, brk_seq_idx)

    def __repr__(self):
        breakpoint()
