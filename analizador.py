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
    'T_CHAR_ARR',
    'T_STRING_ARR'
]

reserved = {
    'if': 'T_IF',
    'else': 'T_ELSE',
    'program': 'T_PROGRAM',
    'int': 'T_INT',
    'float': 'T_FLOAT',
    'string': 'T_STRING',
    'char': 'T_CHAR',
    'boolean': 'T_BOOLEAN',
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
    'main': 'T_MAIN'
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
t_T_CHAR_ARR = r'char\[\]'
t_T_STRING_ARR = r'string\[\]'
t_T_BOOLEAN_ARR = r'boolean\[\]'

t_ignore = ' \t\n\r'

def t_T_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

import ply.yacc as yacc

class SymbolTable:
    def __init__(self):
        self.symbols = dict()
        self.children = list()
        self.parent = None

    def insert(self, id, type):
        self.symbols[id] = type

    def lookup(self, id):
        return self.symbols.get(id)

    def addChild(self, child):
        self.children.append(child)

    def setParent(self, parent):
        self.parent = parent

currentSymbolTable = SymbolTable()

class QuadrupleList:
    def __init__(self):
        self.quadruples = list()
        self.tempCounter = 0

    def insertOperation(self, op, arg1, arg2):
        tempID = 't' + str(self.tempCounter)
        self.tempCounter += 1
        self.quadruples.append((op, arg1, arg2, tempID))
        return tempID

    def insertAssign(self, val, dest):
        self.quadruples.append(('=', val, None, dest))

    def insertJump(self, jump, destination=None):
        # genera cuadruplo vacio para despues actualizarlo con la informacion que necesita
        self.quadruples.append((jump, None, None, destination))
        return len(self.quadruples) - 1

    def updateJump(self, index, expression, destination=None):
        if destination is None :
            destination = len(self.quadruples)
        jump = (self.quadruples[index][0], expression, None, destination)
        self.quadruples[index] = jump

    def getListSize(self):
        return len(self.quadruples)

    def printQuadruples(self):
        index = 0
        for quad in self.quadruples:
            print('#%d, %s, %s, %s, %s\n' % (index, quad[0], quad[1], quad[2], quad[3]))
            index += 1

quadList = QuadrupleList()

def p_programa(p):
    '''
    programa : T_PROGRAM T_ID T_STOP functions T_MAIN block
             | T_PROGRAM T_ID T_STOP T_MAIN block
    '''
    print('Program syntax parsed correctly')
    print 'Global scope symbols:\n', currentSymbolTable.symbols
    print 'Quadruples:\n', quadList.printQuadruples()

def p_functions(p):
    '''
    functions : func functions
              | func
    '''

def p_func(p):
    '''
    func : T_FUNCTION T_ID T_EXP_START parameters T_EXP_END block
         | T_FUNCTION T_ID T_EXP_START T_EXP_END block
    '''
    type = 'FUNCTION'
    id = p[2]
    if currentSymbolTable.lookup(id) == type:
        print "Error de funcion duplicada: ", id
        raise SyntaxError
    else:
        currentSymbolTable.insert(id, type)

def p_parameters(p):
    '''
    parameters : param T_COMMA parameters
               | param
    '''

def p_param(p):
    '''
    param : type T_ID
    '''

def p_var_declare_arr(p):
    'var_declare : type T_ARR_START T_ARR_END var_ids'
    type = p[1]
    for id in p[4]:
        if currentSymbolTable.lookup(id) == type:
            print "Error de variable duplicada: ", type , " ", id
            raise SyntaxError
        else:
            currentSymbolTable.insert(id, p[1] + '[]')

def p_var_declare(p):
    'var_declare : type var_ids'
    type = p[1]
    for id in p[2]:
        if currentSymbolTable.lookup(id) == type:
            print "Error de variable duplicada: ", type , " ", id
            raise SyntaxError
        else:
            currentSymbolTable.insert(id, p[1])

def p_type(p):
    '''
    type : T_BOOLEAN
         | T_CHAR
         | T_STRING
         | T_INT
         | T_FLOAT
         | T_BOOLEAN_ARR
         | T_CHAR_ARR
         | T_STRING_ARR
         | T_INT_ARR
         | T_FLOAT_ARR
    '''
    p[0] = p[1].upper()

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
    p[0] = {'start':p[1], 'end':p[3]}

def p_block_start(p):
    '''
    block_start : T_BLOCK_START
    '''
    p[0] = quadList.getListSize()

def p_block_end(p):
    '''
    block_end : T_BLOCK_END
    '''
    p[0] = quadList.getListSize()

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
         | condition
         | write
         | var_declare
    '''

def p_condition(p):
    '''
    condition : T_IF T_EXP_START expression exp_end block else_if else
              | T_IF T_EXP_START expression exp_end block else_if
              | T_IF T_EXP_START expression exp_end block
    '''
    expression, exp_end = p[3], p[4]
    quadList.updateJump(exp_end, expression['id'])

def p_condition_else(p):
    '''
    condition : T_IF T_EXP_START expression exp_end block else
    '''
    expression, exp_end, block = p[3], p[4], p[5]
    quadList.updateJump(exp_end, expression['id'], block['end'] + 1)

def p_exp_end(p):
    '''
    exp_end : T_EXP_END
    '''
    # generar un cuadruplo de 'GotoF', y regresa el indice del cuadruplo
    p[0] = quadList.insertJump('GotoF')

def p_else_if(p):
    '''
    else_if : T_ELSE T_IF T_EXP_START expression T_EXP_END block
    '''

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
    p[0] = quadList.insertJump('Goto')

def p_while(p):
    # while (expresion) { };
    '''
    while : while_token T_EXP_START expression exp_end block
    '''
    while_token, expression, exp_end = p[1], p[3], p[4]
    quadList.insertJump('Goto', while_token)
    quadList.updateJump(exp_end, expression['id'])

def p_while_token(p):
    'while_token : T_WHILE'
    # regresar el siguiente cuadruplo, que sera igual al inicio de la expresion del while
    p[0] = quadList.getListSize()

def p_for(p):
    # for each item X in myArray { };
    '''
    for : T_FOR T_EXP_START assign T_STOP expression T_STOP assign T_EXP_END block
    '''

def p_do_while(p):
    # do { } while ();
    '''
    do_while : T_DO block T_WHILE T_EXP_START expression T_EXP_END
    '''

def p_assign_simple(p):
    'assign : id T_ASSIGN value'
    id, value = p[1], p[3]
    if id[0] == 'FLOAT' and value['type'] == 'INT':
        print 'Advertencia: casting de int a float implicito al asignar a la variable ', p[1][1]
        quadList.insertAssign(value['id'], id[1])
    elif id[0] != value['type']:
        print 'Error semántico. La variable ', id[1], ' es de tipo ', id[0], ', pero se está intentando asignar un tipo ', value['type']
        raise SyntaxError
    else:
        quadList.insertAssign(value['id'], id[1])

def p_assign_array(p):
    'assign : id T_ASSIGN T_ARR_START array T_ARR_END'
    id, array = p[1], p[4]
    if id[0] != array['type'] + '[]':
        print 'Error semántico. La variable ', id[1], ' es de tipo ', id[0], ', pero se está intentando asignar un arreglo de tipo ', array['type']
        raise SyntaxError
    #else:
        # generar cuadruplo de asignacion de arreglo

def p_assign_array_empty(p):
    'assign : id T_ASSIGN T_ARR_START T_ARR_END'
    id = p[1]
    if not id[0].endswith('[]'):
        print 'Error semántico. La variable ', id[1], ' debe ser de tipo arreglo'
        print currentSymbolTable.symbols
        raise SyntaxError
    #else:
        # generar cuadruplo de asignacion de arreglo

def p_id_array(p):
    '''
    id : T_ID T_ARR_START e T_ARR_END
    '''
    type = currentSymbolTable.lookup(p[1])
    if type is None:
        print 'Error semántico. No se declaro el arregló con ID ', p[1]
        raise SyntaxError
    elif not type.endswith('[]'):
        print 'Error semántico. La variable ', p[1], ' debe ser de tipo arreglo'
        raise SyntaxError
    else:
        p[0] = (type, p[1], 'ARRAY')

def p_id(p):
    'id : T_ID'
    type = currentSymbolTable.lookup(p[1])
    if type is None:
        print 'Error semántico. No se declaro la variable con ID ', p[1]
        print currentSymbolTable.symbols
        raise SyntaxError
    else:
        p[0] = (type, p[1], 'VAR')

def p_array(p):
    'array : value T_COMMA array'
    value, array = p[1], p[3]
    if value['type'] != array['type']:
        print 'Error semántico. Los arreglos deben ser de un solo tipo.'
        print 'Tipos que no encajan: ', value['type'], ' -> ', array['type']
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
    p[0] = { 'type': 'STRING', 'id': p[1] }

def p_write(p):
    'write : T_PRINT T_EXP_START concat T_EXP_END'

def p_concat(p):
    '''
    concat : T_STRING_CONST T_CONCAT concat
           | expression T_CONCAT concat
           | T_STRING_CONST
           | expression
    '''

def p_expression_op(p):
    # Expresiones && y ||
    'expression : exp T_OPREL expression'
    exp, op, expression = p[1], p[2], p[3]
    tempID = quadList.insertOperation(op, exp['id'], expression['id'])
    p[0] = { 'type': 'BOOLEAN', 'id': tempID }

def p_expresion(p):
    'expression : exp'
    p[0] = p[1]

def p_exp_op(p):
    # Expresiones de comparacion (<, >, !=, ==)
    'exp : e T_OPCOMP exp'
    e, op, exp = p[1], p[2], p[3]
    tempID = quadList.insertOperation(op, e['id'], exp['id'])
    p[0] = { 'type': 'BOOLEAN', 'id': tempID }

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
    tempID = quadList.insertOperation(op, term['id'], e['id'])
    p[0] = { 'type': type, 'id': tempID }

def p_e(p):
    'e : term'
    p[0] = p[1]

def p_term_op(p):
    'term : factor T_OPFACT term'
    factor, op, term = p[1], p[2], p[3]
    if (factor['type'] != 'INT' and factor['type'] != 'FLOAT') or (term['type'] != 'INT' and term['type'] != 'FLOAT'):
        print 'Error semántico. Los factores aritméticos deben ser de tipo int o float.'
        raise SyntaxError
    elif factor['type'] == 'FLOAT' or term['type'] == 'FLOAT':
        type = 'FLOAT'
    else:
        type = term['type']
    tempID = quadList.insertOperation(op, factor['id'], term['id'])
    p[0] = { 'type': type, 'id': tempID }

def p_term(p):
    'term : factor'
    p[0] = p[1]

def p_factor(p):
    'factor : T_EXP_START expression T_EXP_END'
    p[0] = p[2]

def p_factor_int(p):
    'factor : T_INT_CONST'
    p[0] = { 'type': 'INT', 'id': p[1] }

def p_factor_float(p):
    'factor : T_FLOAT_CONST'
    p[0] = { 'type': 'FLOAT', 'id': p[1] }

def p_factor_boolean(p):
    '''
    factor : T_TRUE
           | T_FALSE
    '''
    p[0] = { 'type': 'BOOLEAN', 'id': p[1] }

def p_factor_id(p):
    'factor : id'
    if p[1][0] == 'INT[]':
        p[0] = { 'type': 'INT', 'id': p[1][1] }
    elif p[1][0] == 'FLOAT[]':
        p[0] = { 'type': 'FLOAT', 'id': p[1][1] }
    elif p[1][0] == 'BOOLEAN[]':
        p[0] = { 'type': 'BOOLEAN', 'id': p[1][1] }
    elif p[1][0].startswith('CHAR') or p[1][0].startswith('STRING'):
        print 'Error semántico. La variable ', p[1][1], ' de tipo ', p[1][0], ' no puede ser usada en este contexto.'
        raise SyntaxError
    else:
        p[0] = { 'type': p[1][0], 'id': p[1][1] }

def p_error(p):
    print 'Error de sintaxis!'
    print p

parser = yacc.yacc()

import sys
if len(sys.argv) < 2:
    file_name = raw_input('Nombre del archivo de entrada: ')
else:
    file_name = sys.argv[1]

with open(file_name) as file_obj:
    parser.parse(file_obj.read())
