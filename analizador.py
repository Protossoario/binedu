# -*- coding: utf-8 -*-

import ply.lex as lex

tokens = [
    'STOP',
    'COLON',
    'COMMA',
    'ASSIGN',
    'BLOCK_START',
    'BLOCK_END',
    'CONCAT',
    'EXP_START',
    'EXP_END',
    'ID',
    'CTEF',
    'CTEI',
    'CTESTR',
    'OPARIT',
    'OPFACT',
    'OPCOMP',
    'ARR_START',
    'ARR_END',
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

t_STOP = r'\;'
t_COLON = r'\:'
t_COMMA = r'\,'
t_ASSIGN = r'\='
t_BLOCK_START = r'\{'
t_BLOCK_END = r'\}'
t_CONCAT = r'\.'
t_EXP_START = r'\('
t_EXP_END = r'\)'
t_ARR_START = r'\['
t_ARR_END = r'\]'

t_CTEF = r'[0-9]+\.[0-9]+f?'
t_CTEI = r'[0-9]+'
t_CTESTR = r'\".*\"'
t_OPARIT = r'[+-]'
t_OPFACT = r'[*/]'
t_OPCOMP = r'[><]|!=|=='
t_OPREL = r'&&|\|\|'

t_ignore = ' \t\n\r'

def t_ID(t):
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

def p_programa(p):
    '''
    programa : PROGRAM ID STOP functions vars bloque
             | PROGRAM ID STOP vars bloque
             | PROGRAM ID STOP functions bloque
             | PROGRAM ID STOP bloque
    '''
    print('Program syntax parsed correctly')
    print "Global scope symbols: ", currentSymbolTable.symbols

def p_functions(p):
    '''
    functions : func functions
              | func
    '''

def p_func(p):
    '''
    func : FUNCTION ID EXP_START parameters EXP_END vars bloque
         | FUNCTION ID EXP_START parameters EXP_END bloque
         | FUNCTION ID EXP_START EXP_END vars bloque
         | FUNCTION ID EXP_START EXP_END bloque
    '''

def p_parameters(p):
    '''
    parameters : param COMMA parameters
               | param
    '''

def p_param(p):
    '''
    param : type ID
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
    type : INT
         | FLOAT
         | STRING
         | BOOLEAN
    '''
    p[0] = p[1].upper()

def p_var_ids(p):
    '''
    var_ids : ID COMMA var_ids
            | ID
    '''
    if len(p) < 4:
        p[0] = [ p[1] ]
    else:
        p[3].append(p[1])
        p[0] = p[3]

def p_bloque(p):
    '''
    bloque : BLOCK_START estatutos BLOCK_END
           | BLOCK_START BLOCK_END
    '''

def p_estatutos(p):
    '''
    estatutos : estatuto STOP estatutos
              | estatuto STOP
    '''

def p_estatuto(p):
    '''
    estatuto : while
             | do_while
             | for
             | asignacion
             | condicion
             | escritura
    '''

def p_for(p):
    # for each item X in myArray { }
    '''
    for : FOR EXP_START asignacion STOP expresion STOP asignacion EXP_END bloque
    '''

def p_while(p):
    # while (expresion) { }
    '''
    while : WHILE EXP_START expresion EXP_END bloque
    '''

def p_do_while(p):
    # do { } while (expresion);
    '''
    do_while : DO bloque WHILE EXP_START expresion EXP_END
    '''

def p_asignacion(p):
    '''
    asignacion : id ASSIGN value
               | id ASSIGN ARR_START array ARR_END
    '''
    #if p[1][0] == "ARRAY":
    #    print "Asignacion de arreglo ", p[1][1]
    #else:
    #    print "Asignacion de variable ", p[1][1]

def p_id_array(p):
    '''
    id : ID ARR_START e ARR_END
    '''
    p[0] = ("ARRAY", p[1])

def p_id(p):
    'id : ID'
    p[0] = ("VAR", p[1])

def p_array(p):
    '''
    array : value COMMA array
          | value
    '''

def p_value(p):
    '''
    value : expresion
          | CTESTR
          | TRUE
          | FALSE
    '''

def p_condicion(p):
    '''
    condicion : IF EXP_START expresion EXP_END bloque ELSE bloque
              | IF EXP_START expresion EXP_END bloque
    '''

def p_escritura(p):
    'escritura : PRINT EXP_START concatenacion EXP_END'

def p_concatenacion(p):
    '''
    concatenacion : CTESTR CONCAT concatenacion
                  | expresion CONCAT concatenacion
                  | CTESTR
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
    factor : EXP_START expresion EXP_END
           | OPARIT CTEI
           | OPARIT CTEF
           | CTEI
           | CTEF
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
