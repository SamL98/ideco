from collections import defaultdict
from functools import partial, reduce
from math import log2
import string

from binaryninja.enums import VariableSourceType, StructureVariant
from binaryninja.variable import Variable, ConstantData
from binaryninja.exceptions import ILException
from binaryninja.types import *
from binaryninja import *

import json
from binaryninja import *
from binaryninja.lowlevelil import *
from binaryninja.highlevelil import *

def hlil_node_to_dict(node, bv):
    if isinstance(node, HighLevelILImport):
        a = int(node.operands[0])
        return {
            'name': 'Import',
            'address': node.address,
            'operands': [str(node), bv.read_pointer(a)]
        }
    elif isinstance(node, HighLevelILConstPtr):
        n = str(node)
        d = {
            'name': 'ConstPtr',
            'address': node.address,
            'operands': [n.strip('&'), int(node.operands[0])]
        }
        if n.startswith('&'):
            return {
                'name': 'AddressOf',
                'address': node.address,
                'operands': [d]
            }
        else:
            d['name'] = 'Symbol'
            return d
    elif isinstance(node, Variable):
        ops = [node.name]
        if node.type is None:
            ops.append('unk')
            ops.append(8)
        else:
            ops.append(str(node.type))
            ops.append(len(node.type))
        return {
            'name': 'Var',
            'operands': ops,
            'address': 0,
        }
    elif isinstance(node, HighLevelILVar):
        return hlil_node_to_dict(node.operands[0], bv)
    elif isinstance(node, HighLevelILIntrinsic):
        return {
            'name': 'Intrinsic',
            'address': node.address,
            'operands': [str(node.operands[0])] + [hlil_node_to_dict(x, bv) for x in node.operands[1:] if x is not None]
        }
    elif isinstance(node, GotoLabel):
        return str(node)
    elif isinstance(node, HighLevelILInstruction):
        name = str(node.operation)[len('HighLevelILOperation.HLIL_'):]
        # print(name)
        comps = [c.lower().capitalize() for c in name.split('_')]
        result = {
            "name": ''.join(comps),
            'address': node.address,
        }
        result["operands"] = [hlil_node_to_dict(x, bv) for x in node.operands if x is not None]
        return result
    elif isinstance(node, list):
        return [
            hlil_node_to_dict(x, bv)
            for x in node if x is not None
        ]
    elif isinstance(node, int):
        return int(node)
        # return { 'int': int(node) }
    elif isinstance(node, float):
        return float(node)
    elif node is None:
        return {}
    elif type(node) == ConstantData:
        return hlil_node_to_dict(node.value, bv)
    # elif type(node) == ILIntrinsic:
    #     return {
    #         'name': 'Intrinsic',
    #         'address': 0,
    #         'operands
    #     }
    else:
        print(type(node), str(node), dir(node))
        exit()
        return node

def function_to_json(f, bv):
    func_dict = {
        "name": f.name,
        "address": f.address_ranges[0].start,
        "input_types": [hlil_node_to_dict(p, bv) for p in f.parameter_vars if p is not None],
    }

    if f.return_type is not None:
        func_dict['return_type'] = [
            str(f.return_type),
            len(f.return_type),
        ]

    func_dict["hlil"] = hlil_node_to_dict(f.hlil.root, bv)
    return func_dict

def dump(bv, json_path, filter=None):
    functions_json = []
    for f in bv.functions:
        if filter is None or filter(f):
            print(f.name)
            try:
                functions_json.append(function_to_json(f, bv))
            except ILException:
                pass

    with open(json_path, "w") as outfile:
        json.dump(functions_json, outfile, indent=2)
