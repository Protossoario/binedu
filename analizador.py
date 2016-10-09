# -*- coding: utf-8 -*-

import ply.lex as lex

tokens = [
    'T_STOP',
    'T_COLON',
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
    'T_FLOAT',
    'T_INT',
    'T_STRING',
    'OPARIT',
    'OPFACT',
    'OPCOMP',
    'OPREL'
]

reserved = {
    'if': 'IF',
    'else': 'ELSE',
    'var': 'VAR',
    'program': 'PROGRAM',
    'int': 'INT',
    'float': 'FLOAT',
    'string': 'STRING',
    'char': 'CHAR',
    'boolean': 'BOOLEAN',
    'print': 'PRINT',
    'for': 'FOR',
    'function': 'FUNCTION',
    'print': 'PRINT',
    'graph': 'GRAPH',
    'load': 'LOAD',
    'input': 'INPUT',
    'while': 'WHILE',
    'do': 'DO',
    'true': 'TRUE',
    'false': 'FALSE'
}

tokens += reserved.values()

T_STOP = r'\;'
T_COLON = r'\:'
T_COMMA = r'\,'
T_ASSIGN = r'\='
T_BLOCK_START = r'\{'
T_BLOCK_END = r'\}'
T_CONCAT = r'\.'
T_EXP_START = r'\('
T_EXP_END = r'\)'
T_ARR_START = r'\['
T_ARR_END = r'\]'

T_FLOAT = r'[0-9]+\.[0-9]+f?'
T_INT = r'[0-9]+'
T_STRING = r'\".*\"'
OPARIT = r'[+-]'
OPFACT = r'[*/]'
OPCOMP = r'[><]|!=|=='
OPREL = r'&&|\|\|'

ignore = ' \t\n\r'

def T_ID(t):
    r'[a-zA-Z][a-zA-Z0-9_]*'
    if t.value in reserved:
        t.type = reserved[t.value]
    return t

def error(t):
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

def p_programa(p):
    '''
    programa : PROGRAM T_ID T_STOP prog block
    '''
    print('Program syntax parsed correctly')
    print "Global scope symbols: ", currentSymbolTable.symbols

def p_prog(p):
    '''
    prog : vars prog
         | functions prog
         |
    '''

def p_functions(p):
    '''
    functions : func functions
              | func
    '''

def p_func(p):
    '''
    func : FUNCTION T_ID T_EXP_START parameters T_EXP_END vars block
         | FUNCTION T_ID T_EXP_START parameters T_EXP_END block
         | FUNCTION T_ID T_EXP_START T_EXP_END vars block
         | FUNCTION T_ID T_EXP_START T_EXP_END block
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

def p_vars(p):
    '''
    vars : type var_ids STOP vars
         | type var_ids STOP
    '''
    type = p[1]
    for id in p[2]:
        if currentSymbolTable.lookup(id) == type:
            print "Error de variable duplicada: ", type , " ", id
            raise SyntaxError
        else:
            currentSymbolTable.insert(id, p[1])

def p_type(p):
    '''
    type : BOOLEAN
         | CHAR
         | STRING
         | INT
         | FLOAT
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
    block : T_BLOCK_START process T_BLOCK_END
    '''

def p_process(p):
    '''
    process : proc process
            | proc
            |
    '''

def p_proc(p):
    '''
    proc : while
         | do_while
         | for
         | asignacion
         | condition
         | write
    '''

def p_while(p):
    # while (expresion) { };
    '''
    while : WHILE T_EXP_START expresion T_EXP_END block
    '''

def p_for(p):
    # for each item X in myArray { };
    '''
    for : FOR T_EXP_START asignacion T_STOP expresion T_STOP asignacion T_EXP_END block
        | FOR T_EXP_START the_var T_STOP expresion T_STOP asignacion T_EXP_END block
    '''

def p_the_var(p):
    '''
    the_var : VAR T_ID T_COLON type
    '''

def p_do_while(p):
    # do { } while (expresion);
    '''
    do_while : DO block WHILE T_EXP_START expresion T_EXP_END T_STOP
    '''

def p_asignacion(p):
    '''
    asignacion : id T_ASSIGN value T_STOP
               | id T_ASSIGN T_ARR_START array T_ARR_END T_STOP
               | id T_ARR_START expresion T_ARR_END T_STOP
    '''
    if p[1][0] == "ARRAY":
        print "Array assignment ", p[1][1]
    else:
        print "Variable assignment ", p[1][1]

def p_id_array(p):
    '''
    id : T_ID T_ARR_START e T_ARR_END
    '''
    p[0] = ("ARRAY", p[1])

def p_id(p):
    'id : T_ID'
    p[0] = ("VAR", p[1])

def p_array(p):
    '''
    array : value T_COMMA array
          | value
    '''

def p_value(p):
    '''
    value : expresion
          | T_STRING
          | TRUE
          | FALSE
    '''

def p_condition(p):
    '''
    condition : IF T_EXP_START expresion T_EXP_END block else_if else
              | IF T_EXP_START expresion T_EXP_END block else_if
              | IF T_EXP_START expresion T_EXP_END block else
              | IF T_EXP_START expresion T_EXP_END block
    '''

def p_else_if(p):
    '''
    else_if : ELSE IF T_EXP_START expresion T_EXP_END block else_if
            | ELSE IF T_EXP_START expresion T_EXP_END block
    '''

def p_else(p):
    '''
    else : ELSE block
    '''

def p_write(p):
    'write : PRINT T_EXP_START concatenacion T_EXP_END'

def p_concatenacion(p):
    '''
    concatenacion : T_STRING T_CONCAT concatenacion
                  | expresion T_CONCAT concatenacion
                  | T_STRING
                  | expresion
    '''

def p_expresion(p):
    # Expresiones && y ||
    '''
    expresion : exp OPREL exp
              | exp
    '''

def p_exp(p):
    # Expresiones de comparacion (<, >, !=, ==)
    '''
    exp : e OPCOMP exp
        | e
    '''

def p_e(p):
    # Expresion aritmeticas
    '''
    e : termino OPARIT e
      | termino
    '''

def p_termino(p):
    '''
    termino : factor OPFACT termino
            | factor
    '''

def p_factor(p):
    '''
    factor : T_EXP_START expresion T_EXP_END
           | OPARIT T_INT
           | OPARIT T_FLOAT
           | T_INT CONST
           | T_FLOAT
           | id
    '''

def p_error(p):
    print 'Error de sintaxis!'
    print p

parser = yacc.yacc()

data = '''
program MyProgram;

function myFunc(int A, string B, boolean C)
    int i;
{
    test = A + 2;
    print(test);
    while (C) {
        myArray[0] = myArray[1] + 2;
    };
    myArray = [ 3, 4, "test" ];
    for (i = 0; i < A; i = i + 1) {
        print(i);
    };
    A = myArray[0] + 1;
}

float X;
float Y;
int A, B;
{
    print("Hello");
x=[];
    B = 7;
    A = B;
    X = 9;
    Y = A[3 / 1];

    print(X);

    if (A < B && 1 < 2) {
        print("Oops!");
    };

    if (X > Y) {
        print("Value of X = " . X);
    }
    else {
        Y = 2 + 2 * X;
    };
}
'''

lexer.input(data)

result = parser.parse(lexer=lexer)
print(result)
