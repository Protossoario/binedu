# -*- coding: utf-8 -*-

import sys
import ply.lex as lex

tokens = [
    'T_STOP',
    'T_COMMA',
    'T_ASSIGN',
    'T_BLOCK_START',
    'T_BLOCK_END',
    'T_CONCAT',
    'T_EXP_START',
    'T_EXP_END',
    'T_ARR_START',
    'T_ARR_END',
    'T_ID',
    'T_FLOAT_CONST',
    'T_INT_CONST',
    'T_STRING_CONST',
    'T_OPARIT',
    'T_OPFACT',
    'T_OPCOMP',
    'T_OPREL',
    'T_COLON'
]

reserved = {
    'if': 'T_IF',
    'else': 'T_ELSE',
    'program': 'T_PROGRAM',
    'int': 'T_INT',
    'float': 'T_FLOAT',
    'string': 'T_STRING',
    'boolean': 'T_BOOLEAN',
    'struct': 'T_STRUCT',
    'print': 'T_PRINT',
    'for': 'T_FOR',
    'print': 'T_PRINT',
    'graph': 'T_GRAPH',
    'load': 'T_LOAD',
    'input': 'T_INPUT',
    'while': 'T_WHILE',
    'do': 'T_DO',
    'true': 'T_TRUE',
    'false': 'T_FALSE',
    'main': 'T_MAIN',
    'return': 'T_RETURN',
    'void': 'T_VOID',
}

tokens += reserved.values()

t_T_STOP = r'\;'
t_T_COMMA = r'\,'
t_T_ASSIGN = r'\='
t_T_BLOCK_START = r'\{'
t_T_BLOCK_END = r'\}'
t_T_CONCAT = r'\.'
t_T_EXP_START = r'\('
t_T_EXP_END = r'\)'
t_T_ARR_START = r'\['
t_T_ARR_END = r'\]'

t_T_FLOAT_CONST = r'[+-]?[0-9]+\.[0-9]+f?'
t_T_INT_CONST = r'[+-]?[0-9]+'
t_T_STRING_CONST = r'\".*\"'
t_T_OPARIT = r'[+-]'
t_T_OPFACT = r'[*/]'
t_T_OPCOMP = r'!=|==|<=|>=|<|>'
t_T_OPREL = r'&&|\|\|'

t_T_COLON = r'\:'

t_ignore = ' \t'

def t_T_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

lineNumber = 1
def t_endl(t):
    r'\r?\n'
    global lineNumber
    lineNumber += 1

def t_error(t):
    print("Illegal character '%s' in line #%d." % (t.value[0], lineNumber))
    sys.exit()

lexer = lex.lex()

import ply.yacc as yacc

from operations import ops

class SymbolTable:
    def __init__(self):
        self.symbols = dict()
        self.children = list()
        self.parent = None

    def insert(self, id, type, memID):
        self.symbols[id] = { 'type': type, 'memID': memID }

    def insertFunction(self, id, type, memID):
        self.symbols[id] = { 'type': 'FUNCTION', 'memID': memID, 'returnType': type }

    def insertArray(self, id, type, memID, size):
        self.symbols[id] = { 'type': type, 'memID': memID, 'size': size }

    def lookup(self, id):
        result = self.symbols.get(id)
        if not result is None or self.parent is None:
            return result
        return self.parent.lookup(id)

    def addChild(self, child):
        self.children.append(child)

    def setParent(self, parent):
        self.parent = parent

    def __str__(self):
        result = ''
        tab = '| =>%9s' % (' ')
        for key, symbol in self.symbols.iteritems():
            result += '| %10s | %10s |\n' % (key, symbol['memID'])
        for child in self.children:
            childStr = str(child)
            if childStr:
                result += tab
                result += tab.join(childStr.splitlines(True))
        return result

currentSymbolTable = SymbolTable()

def filterNone(token):
    if token is None:
        return '-'
    return token

class QuadrupleList:
    def __init__(self):
        self.quadruples = list()

    def insertQuad(self, name, arg1, arg2=None, dest=None):
        self.quadruples.append((ops[name], arg1, arg2, dest))

    def insertOperation(self, op, arg1, arg2=None, memID=None):
        self.quadruples.append((ops[op], arg1, arg2, memID))

    def insertAssign(self, val, dest):
        self.quadruples.append((ops['='], val, None, dest))

    def insertJump(self, jump, destination=None):
        self.quadruples.append((ops[jump], None, None, destination))
        return len(self.quadruples) - 1

    def updateJump(self, index, expression=None, destination=None):
        if destination is None :
            destination = len(self.quadruples)
        jump = (self.quadruples[index][0], expression, None, destination)
        self.quadruples[index] = jump

    def getListSize(self):
        return len(self.quadruples)

    def getLastQuad(self):
        return self.quadruples[-1]

    def moveQuadRangeToEnd(self, begin, end):
        self.quadruples = self.quadruples[:begin] + self.quadruples[end:] + self.quadruples[begin:end]

    def printQuadruples(self):
        index = 0
        for quad in self.quadruples:
            print('| %3d| %3s | %7s | %7s | %7s |' % (index, filterNone(quad[0]), filterNone(quad[1]), filterNone(quad[2]), filterNone(quad[3])))
            index += 1

quadList = QuadrupleList()

class MemoryMap:
    def __init__(self, range_start=0):
        self.int_start = range_start + 10000
        self.float_start = range_start + 20000
        self.boolean_start = range_start + 30000
        self.string_start = range_start + 40000

        self.int_count = 0
        self.float_count = 0
        self.boolean_count = 0
        self.string_count = 0

    def generateID(self, type):
        type = type.translate(None, '[]')
        if type == 'INT':
            return self.generateIntID()
        if type == 'FLOAT':
            return self.generateFloatID()
        if type == 'STRING':
            return self.generateStringID()
        if type == 'BOOLEAN':
            return self.generateBooleanID()

    def generateArrayID(self, type, size):
        type = type.translate(None, '[]')
        if type == 'INT':
            return self.generateArrayIntID(size)
        if type == 'FLOAT':
            return self.generateArrayFloatID(size)
        if type == 'STRING':
            return self.generateArrayStringID(size)
        if type == 'BOOLEAN':
            return self.generateArrayBooleanID(size)

    def generateIntID(self):
        self.int_count += 1
        return self.int_count + self.int_start

    def generateFloatID(self):
        self.float_count += 1
        return self.float_count + self.float_start

    def generateStringID(self):
        self.string_count += 1
        return self.string_count + self.string_start

    def generateBooleanID(self):
        self.boolean_count += 1
        return self.boolean_count + self.boolean_start

    def generateArrayIntID(self, size):
        memID = self.int_count + 1
        self.int_count += size
        return memID + self.int_start

    def generateArrayFloatID(self, size):
        memID = self.float_count + 1
        self.float_count += size
        return memID + self.float_start

    def generateArrayStringID(self, size):
        memID = self.string_count + 1
        self.string_count += size
        return memID + self.string_start

    def generateArrayBooleanID(self, size):
        memID = self.boolean_count + 1
        self.boolean_count += size
        return memID + self.boolean_start

variables = MemoryMap(50000)
constants = MemoryMap(100000)
temps = MemoryMap(150000)
functions = MemoryMap(200000)

class ConstantTable:
    def __init__(self):
        self.symbols = dict()

    def insert(self, token, type, memID):
        tok = self.symbols.get(token)
        if not tok:
            self.symbols[token] = dict()
        self.symbols[token][type] = memID

    def lookup(self, token, type):
        if token in self.symbols:
            return self.symbols[token].get(type)
        return None

constantTable = ConstantTable()

class StructManager:
    def __init__(self):
        self.structs = dict()
        self.instances = dict()
        self.arrays = dict()

    def createStruct(self, structID, attrList):
        self.structs[structID] = attrList

    def getAttributes(self, structID):
        return self.structs.get(structID)

    def createInstance(self, structInstanceID):
        self.instances[structInstanceID] = dict()

    def createArray(self, structInstanceID, size):
        self.arrays[structInstanceID] = { 'size': size, 'attributes': dict() }

    def addInstanceAttribute(self, structInstanceID, attrType, attrID, memID):
        self.instances[structInstanceID][attrID] = { 'type': attrType, 'memID': memID}

    def getInstanceAttribute(self, structInstanceID, attrID):
        instance = self.instances.get(structInstanceID)
        if instance is None:
            return None
        return instance.get(attrID)

    def getInstance(self, structInstanceID):
        return self.instances.get(structInstanceID)

    def addArrayAttribute(self, structInstanceID, attrType, attrID, memID):
        self.arrays[structInstanceID]['attributes'][attrID] = { 'type': attrType, 'memID': memID }

    def getArray(self, structInstanceID):
        return self.arrays.get(structInstanceID)

    def getArrayAtrribute(self, structInstanceID, attrID):
        array = self.arrays.get(structInstanceID)
        if array is None:
            return None
        return array['attributes'].get(attrID)

structManager = StructManager()

class VirtualStack:
    def __init__(self):
        self.functions = dict()
        self.stack = list()
        self.constants = dict()
        self.retValue = None

    def createFunction(self, funcMemID, funcData):
        self.functions[funcMemID] = funcData

    def lookupFunction(self, memID):
        func = self.functions.get(memID)
        if func is None:
            print('Error: function not found %d.' % (memID))
            raise Exception
        return func

    def createActivationRecord(self, funcMemID):
        self.funcData = self.lookupFunction(funcMemID)
        self.newStack = self.getCurrentStack().copy()
        self.newStack['funcMemID'] = funcMemID

    def setParam(self, memID, index):
        paramID = self.funcData['params'][index - 1]
        self.newStack[paramID] = self.getAddressValue(memID)

    def replaceActivationRecord(self, quad):
        currStack = self.getCurrentStack()
        currStack['quad'] = quad
        self.stack.append(self.newStack)
        return self.funcData['start']

    def setReturnValue(self, memID):
        self.retValue = self.getAddressValue(memID)

    def endActivationRecord(self):
        lastStack = self.stack.pop()
        assert len(self.stack) > 0, "Virtual memory stack is empty!"
        currStack = self.getCurrentStack()
        currStack[lastStack['funcMemID']] = self.retValue
        return currStack['quad']

    def getCurrentStack(self):
        if len(self.stack) == 0:
            self.stack.append(dict())
        return self.stack[-1]

    def insertConstantValue(self, id, val):
        self.constants[id] = val

    def getAddressValue(self, address):
        stack = self.getCurrentStack()
        if type(address) is str and address.startswith('*'):
            address = int(address[1:])
            return stack.get(stack[address])
        elif (address > 100000 and address < 150000):
            return self.constants.get(address)
        else:
            return stack.get(address)

    def updateAddressValue(self, address, value):
        stack = self.getCurrentStack()
        if type(address) is str and address.startswith('*'):
            address = int(address[1:])
            stack[stack[address]] = value
        else:
            stack[address] = value

virtualStack = VirtualStack()

def p_program(p):
    '''
    program : prog_token T_ID T_STOP structs functions main_token block
            | prog_token T_ID T_STOP functions main_token block
            | prog_token T_ID T_STOP structs main_token block
            | prog_token T_ID T_STOP main_token block
    '''
    quadList.insertJump('END')
    print('Program syntax parsed correctly')
    print('Symbols Tables:')
    print(currentSymbolTable)
    print('Quadruples:')
    quadList.printQuadruples()

def p_prog_token(p):
    '''
    prog_token : T_PROGRAM
    '''
    quadList.insertJump('GOTO')

def p_main_token(p):
    '''
    main_token : T_MAIN
    '''
    quadList.updateJump(0)

def p_functions(p):
    '''
    functions : func functions
              | func
    '''

def p_func(p):
    '''
    func : function_signature T_EXP_START parameters T_EXP_END block
         | function_signature T_EXP_START T_EXP_END block
    '''
    pLen = len(p)
    func_sign, block = p[1], p[pLen - 1]
    assert not block is None, "Incorrectly parsed function '%s' block start quadruple, line #%d." % (id, func_sign['lineNumber'])
    if pLen == 6:
        params = p[3]
    else:
        params = list()
    virtualStack.createFunction(func_sign['memID'], { 'start': block['start'], 'params': params })
    if quadList.getLastQuad()[0] != 7:
        quadList.insertJump('RET')

def p_function_signature(p):
    'function_signature : function_type T_ID'
    type, id = p[1], p[2]
    symbol = currentSymbolTable.lookup(id)
    if not symbol is None and symbol['type'] == 'FUNCTION':
        print('Semantic Error: duplicated function with ID "%s" in line #%d.' % (id, lineNumber))
        sys.exit()
    memID = functions.generateIntID()
    currentSymbolTable.insertFunction(id, type, memID)
    p[0] = { 'memID': memID, 'lineNumber': lineNumber }

def p_parameters(p):
    'parameters : param T_COMMA parameters'
    p[0] = [ p[1] ] + p[3]

def p_parameters_single(p):
    'parameters : param'
    p[0] = [ p[1] ]

def p_param(p):
    '''
    param : type T_ID
    '''
    type, id = p[1], p[2]
    symbol = currentSymbolTable.lookup(id)
    if not symbol is None and symbol['type'] == type:
        print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
        sys.exit()
    memID = variables.generateID(type)
    currentSymbolTable.insert(id, type, memID)
    p[0] = memID

def p_var_declare_array(p):
    'var_declare : type T_ID T_ARR_START T_INT_CONST T_ARR_END'
    type, id, size = p[1], p[2], int(p[4])
    symbol = currentSymbolTable.lookup(id)
    if not symbol is None and symbol['type'] == type:
        print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
        sys.exit()
    memID = variables.generateArrayID(type, size)
    currentSymbolTable.insertArray(id, p[1], memID, size)

def p_var_declare_matrix(p):
    'var_declare : type T_ID T_ARR_START T_INT_CONST T_ARR_END T_ARR_START T_INT_CONST T_ARR_END'
    type, id, rows, columns = p[1], p[2], int(p[4]), int(p[7])
    symbol = currentSymbolTable.lookup(id)
    if not symbol is None and symbol['type'] == type:
        print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
        sys.exit()
    size = rows * columns
    memID = variables.generateArrayID(type, size)
    currentSymbolTable.insertArray(id, p[1], memID, [ rows, columns ])

def p_var_declare_struct_array(p):
    'var_declare : T_STRUCT T_ID T_ID T_ARR_START T_INT_CONST T_ARR_END'
    structID, arrID, size = p[2], p[3], int(p[5])
    attributes = structManager.getAttributes(structID)
    if attributes is None:
        print('Semantic Error: undeclared struct type "%s" in line #%d.' % (structID, lineNumber))
        sys.exit()
    elif not structManager.getArray(arrID) is None:
        print('Semantic Error: duplicated struct array "%s" in line #%d.' % (structID, lineNumber))
        sys.exit()
    structManager.createArray(arrID, size)
    for attribute in attributes:
        memID = variables.generateArrayID(attribute['type'], size)
        structManager.addArrayAttribute(arrID, attribute['type'], attribute['id'], memID)

def p_var_declare(p):
    'var_declare : type var_ids'
    type = p[1]
    for id in p[2]:
        symbol = currentSymbolTable.lookup(id)
        if not symbol is None and symbol['type'] == type:
            print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
            sys.exit()
        memID = variables.generateID(type)
        currentSymbolTable.insert(id, type, memID)

def p_function_type(p):
    '''
    function_type : type
                  | T_VOID
    '''
    p[0] = p[1]

def p_type(p):
    '''
    type : T_BOOLEAN
         | T_STRING
         | T_INT
         | T_FLOAT
    '''
    p[0] = p[1].upper()

def p_type_struct(p):
    '''
    type_struct : struct_id T_ID
    '''
    structID = p[2]
    if structManager.getInstance(structID):
        print('Semantic Error: duplicated struct instance name "%s" in line #%d' %(structID, lineNumber))
        sys.exit()
    attributeList = structManager.getAttributes(p[1])
    structManager.createInstance(structID)
    for attribute in attributeList:
        memID = variables.generateID(attribute['type'])
        structManager.addInstanceAttribute(structID, attribute['type'], attribute['id'], memID)

def p_var_ids(p):
    '''
    var_ids : T_ID T_COMMA var_ids
            | T_ID
    '''
    if len(p) < 4:
        p[0] = [ p[1] ]
    else:
        p[3].append(p[1])
        p[0] = p[3]

def p_block(p):
    '''
    block : block_start process block_end
    '''
    p[0] = { 'start' : p[1], 'end' : p[3] }

def p_block_empty(p):
    '''
    block : T_BLOCK_START T_BLOCK_END
    '''
    quadNumber = quadList.getListSize()
    p[0] = { 'start': quadNumber, 'end': quadNumber }

def p_block_start(p):
    '''
    block_start : T_BLOCK_START
    '''
    p[0] = quadList.getListSize()
    # crear nuevo scope local dentro del bloque, solo si el scope anterior no esta vacio
    global currentSymbolTable
    newScope = SymbolTable()
    currentSymbolTable.addChild(newScope)
    newScope.setParent(currentSymbolTable)
    currentSymbolTable = newScope

def p_block_end(p):
    '''
    block_end : T_BLOCK_END
    '''
    p[0] = quadList.getListSize()
    # cambiar al scope del bloque de afuera
    global currentSymbolTable
    currentSymbolTable = currentSymbolTable.parent

def p_call_func(p):
    '''
    call_func : id_token T_EXP_START T_EXP_END
    '''
    symbol = p[1]
    quadList.insertQuad('GOSUB', symbol['memID'])
    p[0] = symbol

def p_call_func_args(p):
    '''
    call_func : id_token T_EXP_START args T_EXP_END
    '''
    symbol, args = p[1], p[3]
    count = 1
    for param in args :
        quadList.insertQuad('PARAM', param['id'], None, count)
        count += 1
    quadList.insertQuad('GOSUB', symbol['memID'])
    p[0] = symbol

def p_return(p):
    '''
    return : T_RETURN
    '''
    quadList.insertJump('RET')

def p_return_value(p):
    '''
    return : T_RETURN value
    '''
    value = p[2]
    quadList.insertQuad('RETURN', value['id'])
    quadList.insertJump('RET')

def p_structs(p):
    '''
    structs : stru structs
            | stru
    '''

def p_stru(p):
    '''
    stru : T_STRUCT struct_id T_BLOCK_START struct_declare T_BLOCK_END
    '''
    structManager.createStruct(p[2], p[4])

def p_struct_declare_repeat(p):
    '''
    struct_declare : type T_ID T_STOP struct_declare
    '''
    declare_list = p[4]
    p[0] = [ { 'type' : p[1], 'id' : p[2] } ] + declare_list

def p_struct_declare(p):
    '''
    struct_declare : type T_ID T_STOP
    '''
    p[0] = [ { 'type' : p[1], 'id' : p[2] } ]

def p_struct_id(p):
    '''
    struct_id : T_ID
    '''
    p[0] = p[1]

def p_id_token(p):
    '''
    id_token : T_ID
    '''
    id = p[1]
    symbol = currentSymbolTable.lookup(id)
    if symbol is None or symbol['type'] != 'FUNCTION' :
        print('Semantic Error: "%s" is not a function in line #%d.' % (id, lineNumber))
        sys.exit()
    quadList.insertQuad('ERA', symbol['memID'])
    p[0] = symbol

def p_args(p):
    '''
    args : value T_COMMA args
    '''
    value, args = p[1], p[3]
    p[0] = [ value ] + args

def p_args_value(p):
    '''
    args : value
    '''
    p[0] = [ p[1] ]

def p_process(p):
    '''
    process : proc T_STOP process
            | proc T_STOP
    '''

def p_proc(p):
    '''
    proc : while
         | do_while
         | for
         | assign
         | assign_struct
         | assign_matrix
         | condition
         | write
         | input
         | var_declare
         | type_struct
         | call_func
         | return
         | graph
         | load
    '''

def p_condition(p):
    # if (expression) { };
    '''
    condition : T_IF T_EXP_START expression exp_end block
    '''
    expression, exp_end = p[3], p[4]
    quadList.updateJump(exp_end, expression['id'])

def p_condition_else(p):
    # if (expression) { } else { };
    '''
    condition : T_IF T_EXP_START expression exp_end block else
    '''
    expression, exp_end, block = p[3], p[4], p[5]
    quadList.updateJump(exp_end, expression['id'], block['end'] + 1)

def p_condition_else_if(p):
    # if (expression) { } else if { };
    '''
    condition : T_IF T_EXP_START expression exp_end block else_token condition
    '''
    expression, exp_end, block, else_token = p[3], p[4], p[5], p[6]
    quadList.updateJump(exp_end, expression['id'], block['end'] + 1)
    quadList.updateJump(else_token)

def p_exp_end(p):
    '''
    exp_end : T_EXP_END
    '''
    p[0] = quadList.insertJump('GOTOF')

def p_else(p):
    '''
    else : else_token block
    '''
    else_token, block = p[1], p[2]
    quadList.updateJump(else_token, None, block['end'])

def p_else_token(p):
    '''
    else_token : T_ELSE
    '''
    p[0] = quadList.insertJump('GOTO')

def p_while(p):
    # while (expresion) { };
    '''
    while : while_token T_EXP_START expression exp_end block
    '''
    while_token, expression, exp_end = p[1], p[3], p[4]
    quadList.insertJump('GOTO', while_token)
    quadList.updateJump(exp_end, expression['id'])

def p_while_token(p):
    'while_token : T_WHILE'
    p[0] = quadList.getListSize()

def p_for(p):
    '''
    for : T_FOR T_EXP_START assign first_stop expression second_stop assign T_EXP_END block
    '''
    first_stop, expression, second_stop, block = p[4], p[5], p[6], p[9]
    quadList.moveQuadRangeToEnd(second_stop + 1, block['start'])
    quadList.insertJump('GOTO', first_stop)
    quadList.updateJump(second_stop, expression['id'])

def p_first_stop(p):
    'first_stop : T_STOP'
    p[0] = quadList.getListSize()

def p_second_stop(p):
    'second_stop : T_STOP'
    p[0] = quadList.insertJump('GOTOF')

def p_do_while(p):
    # do { } while ();
    '''
    do_while : T_DO block T_WHILE T_EXP_START expression T_EXP_END
    '''
    block = p[2]
    quadList.insertJump('GOTOV', block['start'])

def p_assign_simple(p):
    '''
    assign : id T_ASSIGN value
    '''
    id, value = p[1], p[3]
    if id['type'] == 'FLOAT' and value['type'] == 'INT':
        quadList.insertAssign(value['id'], id['id'])
    elif id['type'] != value['type']:
        print('Semantic Error: variable "%s" is type %s, but you are trying to assign a value of type %s in line #%d.' % (id['token'], id['type'], value['type'], lineNumber))
        sys.exit()
    else:
        quadList.insertAssign(value['id'], id['id'])

def p_assign_array(p):
    'assign : id T_ASSIGN T_ARR_START array T_ARR_END'
    id, array = p[1], p[4]
    if 'size' not in id or type(id) != int:
        print('Semantic Error: variable "%s" must be an array in line #%d.' % (id['token'], lineNumber))
        sys.exit()
    elif id['size'] != len(array):
        print('Semantic Error: cannot assign an array of size %d to "%s" in line #%d.' % (len(array), id['token'], lineNumber))
        sys.exit()
    elif id['type'] != array[0]['type'] and not (id['type'] == 'FLOAT' and array[0]['type'] == 'INT'):
        print('Semantic Error: array is type %s, but you are trying to assign an array of type %s to it in line #%d.' % (id['type'], array[0]['type'], lineNumber))
        sys.exit()
    else:
        i = 0
        for value in array:
            quadList.insertAssign(value['id'], id['id'] + i)
            i += 1

def p_assign_array_empty(p):
    'assign : id T_ASSIGN T_ARR_START T_ARR_END'
    id = p[1]
    if 'size' not in id or type(id) != int:
        print('Semantic Error: variable "%s" must be an array in line #%d.' % (id['token'], lineNumber))
        sys.exit()
    elif id['size'] != 0:
        print('Semantic Error: cannot assign an empty array to "%s" in line #%d.' % (id['token'], lineNumber))
        sys.exit()

def p_assign_matrix(p):
    'assign_matrix : id T_ASSIGN T_ARR_START arrays T_ARR_END'
    id, arrays = p[1], p[4]
    matrixLen = len(sum(arrays, []))
    if not 'size' in id or len(id.get('size')) != 2:
        print('Semantic Error: variable "%s" must be a matrix in line #%d.' % (id['token'], lineNumber))
        sys.exit()
    elif id['size'] != matrixLen:
        print('Semantic Error: cannot assign an matrix of %d elements to "%s" in line #%d.' % (matrixLen, id['token'], lineNumber))
        sys.exit()
    elif id['type'] != arrays[0][0]['type'] and not (id['type'] == 'FLOAT' and arrays[0][0]['type'] == 'INT'):
        print('Semantic Error: array is type %s, but you are trying to assign an array of type %s to it in line #%d.' % (id['type'], arrays[0][0]['type'], lineNumber))
        sys.exit()
    else:
        i = 0
        for array in arrays:
            j = 0
            for value in array:
                offset = id['size'][1] * i + j
                quadList.insertAssign(value['id'], id['id'] + offset)
                j += 1
            i += 1

def p_arrays(p):
    'arrays : T_ARR_START array T_ARR_END T_COMMA arrays'
    array, arrays = p[2], p[5]
    if array[0]['type'] != arrays[0][0]['type'] and not (array[0]['type'] == 'FLOAT' and arrays[0][0]['type'] == 'INT'):
        print('Semantic Error: array type mismatch between %s and %s in line #%d.' % (array[0]['type'], arrays[0][0]['type'], lineNumber))
        sys.exit()
    p[0] = [ array ] + arrays

def p_arrays_simple(p):
    'arrays : T_ARR_START array T_ARR_END'
    p[0] = [ p[2] ]

def p_assign_struct(p):
    '''
    assign_struct : T_ID T_COLON T_ID T_ASSIGN value
    '''
    instance_id, attribute_id, value = p[1], p[3], p[5]
    attribute = structManager.getInstanceAttribute(instance_id, attribute_id)
    if attribute is None:
        print('Semantic Error: attribute "%s" is not defined in for struct instance "%s" in line #%d.' % (attribute_id, instance_id, lineNumber))
        sys.exit()
    if attribute['type'] == 'FLOAT' and value['type'] == 'INT':
        quadList.insertAssign(value['id'], attribute['memID'])
    elif attribute['type'] != value['type']:
        print('Semantic Error: attribute "%s" is type %s, but you are trying to assign a value of type %s in line #%d.' % (attribute_id, attribute['type'], value['type'], lineNumber))
        sys.exit()
    else:
        quadList.insertAssign(value['id'], attribute['memID'])

def p_id_array_struct(p):
    'id : T_ID T_ARR_START e T_ARR_END T_COLON T_ID'
    structID, e, attrID = p[1], p[3], p[6]
    struct = structManager.getArray(structID)
    if struct is None:
        print('Semantic Error: variable "%s" must be a struct array in line #%d.' % (structID, lineNumber))
        sys.exit()
    elif not e['type'] == 'INT':
        print('Semantic Error: array index for "%s" must be an integer in line #%d.' % (structID, lineNumber))
        sys.exit()
    attr = struct['attributes'].get(attrID)
    if attr is None:
        print('Semantic Error: struct "%s" has no attribute "%s" in line #%d.' % (structID, attrID, lineNumber))
        sys.exit()
    tempID = temps.generateIntID()
    quadList.insertQuad('VER', e['id'], struct['size'])
    quadList.insertOperation('ARRSUM', attr['memID'], e['id'], tempID)
    p[0] = { 'type': attr['type'], 'id': '*' + str(tempID), 'size': struct['size'], 'token': attrID }

def p_id_array(p):
    '''
    id : T_ID T_ARR_START e T_ARR_END
    '''
    symbol, e = currentSymbolTable.lookup(p[1]), p[3]
    if symbol is None:
        print('Semantic Error: undeclared array with ID "%s" in line #%d.' % (p[1], lineNumber))
        sys.exit()
    elif not 'size' in symbol:
        print('Semantic Error: variable with ID "%s" must be an array in line #%d.' % (p[1], lineNumber))
        sys.exit()
    elif not e['type'] == 'INT':
        print('Semantic Error: array index for "%s" must be an integer in line #%d.' % (p[1], lineNumber))
        sys.exit()
    tempID = temps.generateIntID()
    quadList.insertQuad('VER', e['id'], symbol['size'])
    quadList.insertOperation('ARRSUM', symbol['memID'], e['id'], tempID)
    p[0] = { 'type': symbol['type'], 'id': '*' + str(tempID), 'size': symbol['size'], 'token': p[1] }

def p_id_matrix(p):
    'id : T_ID T_ARR_START e T_ARR_END T_ARR_START e T_ARR_END'
    symbol, rowInd, colInd = currentSymbolTable.lookup(p[1]), p[3], p[6]
    if symbol is None:
        print('Semantic Error: undeclared array with ID "%s" in line #%d.' % (p[1], lineNumber))
        sys.exit()
    elif not 'size' in symbol:
        print('Semantic Error: variable with ID "%s" must be an array in line #%d.' % (p[1], lineNumber))
        sys.exit()
    elif not rowInd['type'] == 'INT' or not colInd['type'] == 'INT':
        print('Semantic Error: array index for "%s" must be an integer in line #%d.' % (p[1], lineNumber))
        sys.exit()
    multID, sumID, pointerID = temps.generateIntID(), temps.generateIntID(), temps.generateIntID()
    quadList.insertQuad('VER', rowInd['id'], symbol['size'][0])
    quadList.insertQuad('VER', colInd['id'], symbol['size'][1])
    quadList.insertOperation('ARRMULT', symbol['size'][1], rowInd['id'], multID)
    quadList.insertOperation('+', multID, colInd['id'], sumID)
    quadList.insertOperation('ARRSUM', symbol['memID'], sumID, pointerID)
    p[0] = { 'type': symbol['type'], 'id': '*' + str(pointerID), 'size': symbol['size'], 'token': p[1] }

def p_id(p):
    'id : T_ID'
    symbol = currentSymbolTable.lookup(p[1])
    if symbol is None:
        print('Semantic Error: undeclared variable with ID "%s" in line #%d.' % (p[1], lineNumber))
        sys.exit()
    elif 'size' in symbol:
        p[0] = { 'type': symbol['type'], 'id': symbol['memID'], 'size': symbol['size'], 'token': p[1] }
    else:
        p[0] = { 'type': symbol['type'], 'id': symbol['memID'], 'token': p[1] }

def p_array(p):
    'array : value T_COMMA array'
    value, array = p[1], p[3]
    if value['type'] != array[0]['type']:
        print('Semantic Error: type mismatch in array declaration between %s and %s values in line #%d.' % (value['type'], array[0]['type'], lineNumber))
        sys.exit()
    p[0] = [ value ] + array

def p_array_value(p):
    'array : value'
    p[0] = [ p[1] ]

def p_value_expression(p):
    'value : expression'
    p[0] = p[1]

def p_value_string(p):
    'value : T_STRING_CONST'
    type = 'STRING'
    memID = constantTable.lookup(p[1], type)
    if memID:
        p[0] = { 'type': type, 'id': memID }
    else:
        memID = constants.generateStringID()
        constantTable.insert(p[1], type, memID)
        virtualStack.insertConstantValue(memID, p[1][1:-1])
        p[0] = { 'type': type, 'id': memID }

def p_write(p):
    'write : T_PRINT T_EXP_START concat T_EXP_END'
    concat = p[3]
    quadList.insertQuad('PRINT', concat['id'])

def p_input(p):
    'input : T_INPUT T_EXP_START id T_EXP_END'
    id = p[3]
    quadList.insertQuad('INPUT', id['id'])

def p_graph(p):
    'graph : T_GRAPH T_EXP_START T_ID T_EXP_END'
    structID = p[3]
    struct = structManager.getArray(structID)
    if struct is None:
        print('Semantic Error: variable "%s" must be a struct array in line #%d.' % (structID, lineNumber))
        sys.exit()
    quadList.insertQuad('GRAPH', struct['size'], len(struct['attributes']))
    for attrID in struct['attributes']:
        attribute = struct['attributes'][attrID]
        nameMemID = constantTable.lookup(attrID, 'STRING')
        if nameMemID is None:
            nameMemID = constants.generateStringID()
            constantTable.insert(attrID, 'STRING', nameMemID)
            virtualStack.insertConstantValue(nameMemID, attrID[1:-1])
        quadList.insertQuad('GATTR', attribute['memID'], nameMemID)

def p_load(p):
    'load : T_LOAD T_EXP_START T_STRING_CONST T_COMMA T_ID T_EXP_END'
    string_const, structID = p[3], p[5]
    memID = constantTable.lookup(string_const, 'STRING')
    if memID is None:
        memID = constants.generateStringID()
        constantTable.insert(string_const, 'STRING', memID)
        virtualStack.insertConstantValue(memID, string_const[1:-1])
    struct = structManager.getArray(structID)
    if struct is None:
        print('Semantic Error: variable "%s" must be a struct array in line #%d.' % (structID, lineNumber))
        sys.exit()
    quadList.insertQuad('LOAD', memID, struct['size'], len(struct['attributes']))
    for attrID in struct['attributes']:
        attribute = struct['attributes'][attrID]
        quadList.insertQuad('LATTR', attribute['memID'])

def p_concat_const(p):
    'concat : T_STRING_CONST'
    type = 'STRING'
    memID = constantTable.lookup(p[1], type)
    if memID:
        p[0] = { 'type': type, 'id': memID }
    else:
        memID = constants.generateStringID()
        constantTable.insert(p[1], type, memID)
        virtualStack.insertConstantValue(memID, p[1][1:-1])
        p[0] = { 'type': type, 'id': memID }

def p_concat_expr(p):
    'concat : expression'
    p[0] = p[1]

def p_concat_op_const(p):
    'concat : T_STRING_CONST T_CONCAT concat'
    string_const, op, concat = p[1], p[2], p[3]
    type = 'STRING'
    memID = constantTable.lookup(string_const, type)
    if not memID:
        memID = constants.generateStringID()
        constantTable.insert(string_const, type, memID)
        virtualStack.insertConstantValue(memID, string_const[1:-1])
    operationMemID = temps.generateStringID()
    quadList.insertOperation(op, memID, concat['id'], operationMemID)
    p[0] = { 'type': type, 'id': operationMemID }

def p_concat_op_expr(p):
    'concat : expression T_CONCAT concat'
    expression, op, concat = p[1], p[2], p[3]
    memID = temps.generateStringID()
    quadList.insertOperation(op, expression['id'], concat['id'], memID)
    p[0] = { 'type': 'STRING', 'id': memID }

def p_expression_op(p):
    # Expresiones && y ||
    'expression : exp T_OPREL expression'
    exp, op, expression = p[1], p[2], p[3]
    if exp['type'] != 'BOOLEAN' or expression['type'] != 'BOOLEAN':
        print('Semantic Error: logic operands must be boolean type in line #%d.' % (lineNumber))
        sys.exit()
    memID = temps.generateBooleanID()
    quadList.insertOperation(op, exp['id'], expression['id'], memID)
    p[0] = { 'type': 'BOOLEAN', 'id': memID }

def p_expresion(p):
    'expression : exp'
    p[0] = p[1]

def p_exp_op(p):
    # Expresiones de comparacion (<, >, !=, ==, <=, >=)
    'exp : e T_OPCOMP exp'
    e, op, exp = p[1], p[2], p[3]
    if e['type'] != exp['type'] and ((e['type'] != 'INT' and e['type'] != 'FLOAT') or (exp['type'] != 'INT' and exp['type'] != 'FLOAT')):
        print('Semantic Error: relational operands must be the same type in line #%d.' % (lineNumber))
        sys.exit()
    memID = temps.generateBooleanID()
    quadList.insertOperation(op, e['id'], exp['id'], memID)
    p[0] = { 'type': 'BOOLEAN', 'id': memID }

def p_exp(p):
    'exp : e'
    p[0] = p[1]

def p_e_op(p):
    'e : term T_OPARIT e'
    term, op, e = p[1], p[2], p[3]
    if (term['type'] != 'INT' and term['type'] != 'FLOAT') or (e['type'] != 'INT' and e['type'] != 'FLOAT'):
        print('Semantic Error: arithmetic operands must be of numeric type (int or float) in line #%d.' % (lineNumber))
        sys.exit()
    elif term['type'] == 'FLOAT' or e['type'] == 'FLOAT':
        type = 'FLOAT'
    else:
        type = e['type']
    memID = temps.generateID(type)
    quadList.insertOperation(op, term['id'], e['id'], memID)
    p[0] = { 'type': type, 'id': memID }

def p_e(p):
    'e : term'
    p[0] = p[1]

def p_term_op(p):
    'term : factor T_OPFACT term'
    factor, op, term = p[1], p[2], p[3]
    if (factor['type'] != 'INT' and factor['type'] != 'FLOAT') or (term['type'] != 'INT' and term['type'] != 'FLOAT'):
        print('Semantic Error: arithmetic operands must be of numeric type (int or float) in line #%d.' % (lineNumber))
        sys.exit()
    elif factor['type'] == 'FLOAT' or term['type'] == 'FLOAT':
        type = 'FLOAT'
    else:
        type = term['type']
    memID = temps.generateID(type)
    quadList.insertOperation(op, factor['id'], term['id'], memID)
    p[0] = { 'type': type, 'id': memID }

def p_term(p):
    'term : factor'
    p[0] = p[1]

def p_factor(p):
    'factor : T_EXP_START expression T_EXP_END'
    p[0] = p[2]

def p_factor_int(p):
    'factor : T_INT_CONST'
    int_const = p[1]
    type = 'INT'
    memID = constantTable.lookup(int_const, type)
    if not memID:
        memID = constants.generateIntID()
        constantTable.insert(int_const, type, memID)
        virtualStack.insertConstantValue(memID, int(int_const))
    p[0] = { 'type': type, 'id': memID }

def p_factor_float(p):
    'factor : T_FLOAT_CONST'
    float_const = p[1]
    type = 'FLOAT'
    memID = constantTable.lookup(float_const, type)
    if not memID:
        memID = constants.generateFloatID()
        constantTable.insert(float_const, type, memID)
        virtualStack.insertConstantValue(memID, float(float_const))
    p[0] = { 'type': type, 'id': memID }

def p_factor_boolean(p):
    '''
    factor : T_TRUE
           | T_FALSE
    '''
    bool_const = p[1]
    type = 'BOOLEAN'
    memID = constantTable.lookup(bool_const, type)
    if not memID:
        memID = constants.generateBooleanID()
        constantTable.insert(bool_const, type, memID)
        virtualStack.insertConstantValue(memID, bool_const == 'true')
    p[0] = { 'type': type, 'id': memID }

def p_factor_struct(p):
    '''
    factor : T_ID T_COLON T_ID
    '''
    instance_id, attribute_id = p[1], p[3]
    attribute = structManager.getInstanceAttribute(instance_id, attribute_id)
    p[0] = { 'type': attribute['type'], 'id': attribute['memID'] }

def p_factor_id(p):
    'factor : id'
    id = p[1]
    if id['type'].endswith('[]'):
        p[0] = { 'type': id['type'][:-2], 'id': id['id'] }
    else:
        p[0] = id

def p_factor_func(p):
    'factor : call_func'
    symbol = p[1]
    if symbol is None or not 'returnType' in symbol:
        print("Semantic Error: '%s' is not a function in line #%d." % (id, lineNumber))
    tempID = temps.generateID(symbol['returnType'])
    quadList.insertQuad('COPYRET', symbol['memID'], None, tempID)
    p[0] = { 'type': symbol['returnType'], 'id': tempID }

def p_error(p):
    if p:
        global lineNumber
        print('Syntax error at token "%s" in line #%d.' % (p.value, lineNumber))
    else:
        print('Syntax error at EOF.')
    sys.exit()

parser = yacc.yacc()

if len(sys.argv) < 2:
    file_name = raw_input('Nombre del archivo de entrada: ')
else:
    file_name = sys.argv[1]

with open(file_name) as file_obj:
    parser.parse(file_obj.read())

# Ejecutar cuadruplos
i = 0
lenQuads = len(quadList.quadruples)
while i < lenQuads:
    quad = quadList.quadruples[i]
    i += 1
    if quad[0] == 1:
        i = int(quad[3])
    elif quad[0] == 2:
        condition = virtualStack.getAddressValue(quad[1])
        if not condition:
            i = int(quad[3])
    elif quad[0] == 3:
        condition = virtualStack.getAddressValue(quad[1])
        if condition:
            i = int(quad[3])
    elif quad[0] == 4:
        virtualStack.createActivationRecord(quad[1])
    elif quad[0] == 5:
        # Guardar el cuadruplo al que debemos regresar, y obtener el cuadruplo del inicio de la funcion
        i = virtualStack.replaceActivationRecord(i)
    elif quad[0] == 6:
        virtualStack.setParam(quad[1], quad[3])
    elif quad[0] == 7:
        i = virtualStack.endActivationRecord()
    elif quad[0] == 8:
        virtualStack.setReturnValue(quad[1])
    elif quad[0] == 9:
        print(virtualStack.getAddressValue(quad[1]))
    elif quad[0] == 10:
        value = input()
        virtualStack.updateAddressValue(quad[1], value)
    elif quad[0] == 13:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 + value2)
    elif quad[0] == 14:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 - value2)
    elif quad[0] == 15:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 / value2)
    elif quad[0] == 16:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 * value2)
    elif quad[0] == 17:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], str(value1) + str(value2))
    elif quad[0] == 18:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 and value2)
    elif quad[0] == 19:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 or value2)
    elif quad[0] == 20:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 < value2)
    elif quad[0] == 21:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 > value2)
    elif quad[0] == 22:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 == value2)
    elif quad[0] == 23:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 != value2)
    elif quad[0] == 25:
        value = virtualStack.getAddressValue(quad[1])
        if value is None:
            print('Error: undefined variable in quadruple #%d.' % (i))
            raise Exception
        virtualStack.updateAddressValue(quad[3], value)
    elif quad[0] == 27:
        value = virtualStack.getAddressValue(quad[1])
        if value < 0 or value >= quad[2]:
            print('Error: undefined index %d in quadruple #%d.' % (value, i))
            raise Exception
    elif quad[0] == 28:
        address, index = quad[1], virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], address + index)
    elif quad[0] == 29:
        address, index = quad[1], virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], address * index)
    elif quad[0] == 30:
        retValue = virtualStack.getAddressValue(quad[1])
        virtualStack.updateAddressValue(quad[3], retValue)
    elif quad[0] == 31:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 <= value2)
    elif quad[0] == 32:
        value1, value2 = virtualStack.getAddressValue(quad[1]), virtualStack.getAddressValue(quad[2])
        virtualStack.updateAddressValue(quad[3], value1 >= value2)
