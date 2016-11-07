# -*- coding: utf-8 -*-

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
    'T_INT_ARR',
    'T_FLOAT_ARR',
    'T_BOOLEAN_ARR',
    'T_STRING_ARR',
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
    'function': 'T_FUNCTION',
    'print': 'T_PRINT',
    'graph': 'T_GRAPH',
    'load': 'T_LOAD',
    'input': 'T_INPUT',
    'while': 'T_WHILE',
    'do': 'T_DO',
    'true': 'T_TRUE',
    'false': 'T_FALSE',
    'main': 'T_MAIN',
    'return': 'T_RETURN'
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
t_T_OPCOMP = r'[><]|!=|=='
t_T_OPREL = r'&&|\|\|'

t_T_INT_ARR = r'int\[\]'
t_T_FLOAT_ARR = r'float\[\]'
t_T_STRING_ARR = r'string\[\]'
t_T_BOOLEAN_ARR = r'boolean\[\]'

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
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

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
            print('| %3d| %6s | %20s | %6s | %10s |' % (index, filterNone(quad[0]), filterNone(quad[1]), filterNone(quad[2]), filterNone(quad[3])))
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
        #else:
            #raise Exception

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

variables = MemoryMap(50000)
constants = MemoryMap(100000)
temps = MemoryMap(150000)
functions = MemoryMap(200000)

class ConstantTable:
    def __init__(self):
        self.symbols = dict()
        self.address_value = dict()

    def castValue(self, type, value):
        if type == 'INT':
            return int(value)
        elif type == 'FLOAT':
            return float(value)
        elif type == 'BOOLEAN':
            return value == 'true'
        elif type == 'STRING':
            return value[1:-1]
        else:
            raise Exception

    def insert(self, token, type, memID):
        tok = self.symbols.get(token)
        if not tok:
            self.symbols[token] = dict()
        self.symbols[token][type] = memID
        self.address_value[memID] = self.castValue(type, token)

    def lookup(self, token, type):
        if token in self.symbols:
            return self.symbols[token].get(type)
        return None

constantTable = ConstantTable()

class StructManager:
    def __init__(self):
        self.structs = dict()
        self.instances = dict()

    def createStruct(self, structID, attrList):
        self.structs[structID] = attrList

    def getAttributes(self, structID):
        return self.structs[structID]

    def createInstance(self, structInstanceID):
        self.instances[structInstanceID] = dict()

    def addInstanceAttribute(self, structInstanceID, attrType, attrID, memID):
        self.instances[structInstanceID][attrID] = { 'type' : attrType, 'memID' : memID}

    def getInstanceAttribute(self, structInstanceID, attrID):
        instance = self.instances.get(structInstanceID)
        if instance is None:
            return None
        return instance.get(attrID)

    def getInstance(self, structInstanceID):
        return self.instances.get(structInstanceID)

structManager = StructManager()

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
    func : func_token T_ID T_EXP_START parameters T_EXP_END block
         | func_token T_ID T_EXP_START T_EXP_END block
    '''
    type = 'FUNCTION'
    lineNumber, id = p[1], p[2]
    symbol = currentSymbolTable.lookup(id)
    if not symbol is None and symbol['type'] == type:
        print('Semantic Error: duplicated function with ID "%s" in line #%d.' % (id, lineNumber))
        raise SyntaxError
    else:
        memID = functions.generateIntID()
        currentSymbolTable.insert(id, type, memID)
    if quadList.getLastQuad()[0] != 'RET' :
        quadList.insertJump('RET')

def p_func_token(p):
    '''
    func_token : T_FUNCTION
    '''
    p[0] = lineNumber

def p_parameters(p):
    '''
    parameters : param T_COMMA parameters
               | param
    '''

def p_param(p):
    '''
    param : type T_ID
    '''
    type, id = p[1], p[2]
    symbol = currentSymbolTable.lookup(id)
    if not symbol is None and symbol['type'] == type:
        print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
        raise SyntaxError
    else:
        memID = variables.generateID(type)
        currentSymbolTable.insert(id, type, memID)

def p_var_declare_arr(p):
    'var_declare : type T_ARR_START T_ARR_END var_ids'
    type = p[1]
    for id in p[4]:
        symbol = currentSymbolTable.lookup(id)
        if not symbol is None and symbol['type'] == type:
            print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
            raise SyntaxError
        else:
            memID = variables.generateID(type)
            currentSymbolTable.insert(id, p[1] + '[]', memID)

def p_var_declare(p):
    'var_declare : type var_ids'
    type = p[1]
    for id in p[2]:
        symbol = currentSymbolTable.lookup(id)
        if not symbol is None and symbol['type'] == type:
            print('Semantic Error: duplicated variable of type %s with ID "%s" in line #%d.' % (type, id, lineNumber))
            raise SyntaxError
        else:
            memID = variables.generateID(type)
            currentSymbolTable.insert(id, type, memID)

def p_type(p):
    '''
    type : T_BOOLEAN
         | T_STRING
         | T_INT
         | T_FLOAT
         | T_BOOLEAN_ARR
         | T_STRING_ARR
         | T_INT_ARR
         | T_FLOAT_ARR
    '''
    p[0] = p[1].upper()

def p_type_struct(p):
    '''
    type_struct : struct_id T_ID
    '''
    if structManager.getInstance(p[2]):
        print('Semantic Error: duplicated struct instance name "%s" in line #%d' %(p[2], lineNumber))
        raise SyntaxError
    attributeList = structManager.getAttributes(p[1])
    structManager.createInstance(p[2])
    for attribute in attributeList:
        memID = variables.generateID(attribute['type'])
        structManager.addInstanceAttribute(p[2], attribute['type'], attribute['id'], memID)

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
    id = p[1]
    quadList.insertQuad('GOSUB', id)

def p_call_func_args(p):
    '''
    call_func : id_token T_EXP_START args T_EXP_END
    '''
    id, args = p[1], p[3]
    count = 1
    for param in args :
        quadList.insertQuad('PARAM', param['id'], None, count)
        count += 1
    quadList.insertQuad('GOSUB', id)

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
        raise SyntaxError
    quadList.insertQuad('ERA', symbol['memID'])
    p[0] = symbol['memID']

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
    # for each item X in myArray { };
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
        print('Semantic Error: variable with ID "%s" is type %s, but you are trying to assign a value of type %s in line #%d.' % (id['id'], id['type'], value['type'], lineNumber))
        raise SyntaxError
    else:
        quadList.insertAssign(value['id'], id['id'])

def p_assign_array(p):
    'assign : id T_ASSIGN T_ARR_START array T_ARR_END'
    id, array = p[1], p[4]
    if id['type'] != array['type'] + '[]':
        print('Semantic Error: variable with ID "%s" is type %s, but you are trying to assign an array of type %s to it in line #%d.' % (id['id'], id['type'], array['type'], lineNumber))
        raise SyntaxError
    #else:
        # generar cuadruplo de asignacion de arreglo

def p_assign_array_empty(p):
    'assign : id T_ASSIGN T_ARR_START T_ARR_END'
    id = p[1]
    if not id['type'].endswith('[]'):
        print('Semantic Error: variable with ID "%s" must be an array in line #%d.' % (id['id'], lineNumber))
        raise SyntaxError
    #else:
        # generar cuadruplo de asignacion de arreglo

def p_assign_struct(p):
    '''
    assign_struct : T_ID T_COLON T_ID T_ASSIGN value
    '''
    instance_id, attribute_id, value = p[1], p[3], p[5]
    attribute = structManager.getInstanceAttribute(instance_id, attribute_id)
    if attribute is None:
        print('Semantic Error: attribute "%s" is not defined in for struct instance "%s" in line #%d.' % (attribute_id, instance_id, lineNumber))
        raise SyntaxError
    if attribute['type'] == 'FLOAT' and value['type'] == 'INT':
        quadList.insertAssign(value['id'], attribute['memID'])
    elif attribute['type'] != value['type']:
        print('Semantic Error: attribute "%s" is type %s, but you are trying to assign a value of type %s in line #%d.' % (attribute_id, attribute['type'], value['type'], lineNumber))
        raise SyntaxError
    else:
        quadList.insertAssign(value['id'], attribute['memID'])

def p_id_array(p):
    '''
    id : T_ID T_ARR_START e T_ARR_END
    '''
    symbol = currentSymbolTable.lookup(p[1])
    if symbol is None:
        print('Semantic Error: undeclared array with ID "%s" in line #%d.' % (p[1], lineNumber))
        raise SyntaxError
    elif not symbol['type'].endswith('[]'):
        print('Semantic Error: variable with ID "%s" must be an array in line #%d.' % (p[1], lineNumber))
        raise SyntaxError
    else:
        p[0] = { 'type': symbol['type'], 'id': symbol['memID'] }

def p_id(p):
    'id : T_ID'
    symbol = currentSymbolTable.lookup(p[1])
    if symbol is None:
        print('Semantic Error: undeclared variable with ID "%s" in line #%d.' % (p[1], lineNumber))
        raise SyntaxError
    else:
        p[0] = { 'type': symbol['type'], 'id': symbol['memID'] }

def p_array(p):
    'array : value T_COMMA array'
    value, array = p[1], p[3]
    if value['type'] != array['type']:
        print('Semantic Error: type mismatch in array declaration between %s and %s values in line #%d.' % (value['type'], array['type'], lineNumber))
        raise SyntaxError
    p[0] = value

def p_array_value(p):
    'array : value'
    p[0] = p[1]

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

def p_load(p):
    'load : T_LOAD T_EXP_START T_STRING_CONST T_COMMA T_ID T_EXP_END'

def p_concat_const(p):
    'concat : T_STRING_CONST'
    type = 'STRING'
    memID = constantTable.lookup(p[1], type)
    if memID:
        p[0] = { 'type': type, 'id': memID }
    else:
        memID = constants.generateStringID()
        constantTable.insert(p[1], type, memID)
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
    memID = temps.generateBooleanID()
    quadList.insertOperation(op, exp['id'], expression['id'], memID)
    p[0] = { 'type': 'BOOLEAN', 'id': memID }

def p_expresion(p):
    'expression : exp'
    p[0] = p[1]

def p_exp_op(p):
    # Expresiones de comparacion (<, >, !=, ==)
    'exp : e T_OPCOMP exp'
    e, op, exp = p[1], p[2], p[3]
    memID = temps.generateBooleanID()
    quadList.insertOperation(op, e['id'], exp['id'], memID)
    p[0] = { 'type': 'BOOLEAN', 'id': memID }

def p_exp(p):
    'exp : e'
    p[0] = p[1]

def p_e_op(p):
    'e : term T_OPARIT e'
    term, op, e = p[1], p[2], p[3]
    if term['type'] == 'FLOAT' or e['type'] == 'FLOAT':
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
        print('Semantic Error: arithmetic factors must be of numeric type (int or float) in line #%d.' % (lineNumber))
        raise SyntaxError
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
    p[0] = { 'type': type, 'id': memID }

def p_factor_float(p):
    'factor : T_FLOAT_CONST'
    float_const = p[1]
    type = 'FLOAT'
    memID = constantTable.lookup(float_const, type)
    if not memID:
        memID = constants.generateFloatID()
        constantTable.insert(float_const, type, memID)
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
    p[0] = { 'type': type, 'id': memID }

def p_factor_struct(p):
    '''
    factor : T_ID T_COLON T_ID
    '''
    instance_id, attribute_id = p[1], p[3]
    attribute = structManager.getInstanceAttribute(instance_id, attribute_id)
    if attribute['type'] == 'STRING':
        print('Semantic Error: attribute "%s" of type %s cannot be used in this type of expression, in line #%d.' % (attribute_id, attribute['type'], lineNumber))
        raise SyntaxError
    p[0] = { 'type': attribute['type'], 'id': attribute['memID'] }

def p_factor_id(p):
    'factor : id'
    id = p[1]
    if id['type'] == 'INT[]':
        p[0] = { 'type': 'INT', 'id': id['id'] }
    elif id['type'] == 'FLOAT[]':
        p[0] = { 'type': 'FLOAT', 'id': id['id'] }
    elif id['type'] == 'BOOLEAN[]':
        p[0] = { 'type': 'BOOLEAN', 'id': id['id'] }
    elif id['type'].startswith('STRING'):
        print('Semantic Error: variable with ID "%s" and type %s cannot be used in this type of expression, in line #%d.' % (id['id'], id['type'], lineNumber))
        raise SyntaxError
    else:
        p[0] = id

def p_error(p):
    if p:
        global lineNumber
        print('Syntax error at token "%s" in line #%d.' % (p.value, lineNumber))
    else:
        print('Syntax error at EOF.')

parser = yacc.yacc()

import sys
if len(sys.argv) < 2:
    file_name = raw_input('Nombre del archivo de entrada: ')
else:
    file_name = sys.argv[1]

with open(file_name) as file_obj:
    parser.parse(file_obj.read())

# Ejecutar cuadruplos
for quad in quadList.quadruples:
    if quad[0] == 9:
        print(constantTable.address_value.get(quad[1]))
    elif quad[0] == 13:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 + value2
    elif quad[0] == 14:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 - value2
    elif quad[0] == 15:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 / value2
    elif quad[0] == 16:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 * value2
    # elif quad[0] == 17:
    #     # TODO .
    elif quad[0] == 18:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 and value2
    elif quad[0] == 19:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 or value2
    elif quad[0] == 20:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 < value2
    elif quad[0] == 21:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 > value2
    elif quad[0] == 22:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 == value2
    elif quad[0] == 23:
        value1, value2 = constantTable.address_value.get(quad[1]), constantTable.address_value.get(quad[2])
        constantTable.address_value[quad[3]] = value1 != value2
    elif quad[0] == 25:
        value = constantTable.address_value.get(quad[1])
        if value is None:
            print constantTable.address_value
            print('Error: undefined variable %d.' % (quad[1]))
            raise Exception
        constantTable.address_value[quad[3]] = value
print constantTable.address_value
print structManager.structs
print structManager.instances
