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
    'T_FLOAT_CONST',
    'T_INT_CONST',
    'T_STRING_CONST',
    'T_OPARIT',
    'T_OPFACT',
    'T_OPCOMP',
    'T_OPREL'
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
t_T_COLON = r'\:'
t_T_COMMA = r'\,'
t_T_ASSIGN = r'\='
t_T_BLOCK_START = r'\{'
t_T_BLOCK_END = r'\}'
t_T_CONCAT = r'\.'
t_T_EXP_START = r'\('
t_T_EXP_END = r'\)'
t_T_ARR_START = r'\['
t_T_ARR_END = r'\]'

t_T_FLOAT_CONST = r'[0-9]+\.[0-9]+f?'
t_T_INT_CONST = r'[0-9]+'
t_T_STRING_CONST = r'\".*\"'
t_T_OPARIT = r'[+-]'
t_T_OPFACT = r'[*/]'
t_T_OPCOMP = r'[><]|!=|=='
t_T_OPREL = r'&&|\|\|'

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

def p_programa(p):
    '''
    programa : T_PROGRAM T_ID T_STOP functions T_MAIN block
             | T_PROGRAM T_ID T_STOP T_MAIN block
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

def p_var_declare(p):
    '''
    var_declare : type var_ids
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
    type : T_BOOLEAN
         | T_CHAR
         | T_STRING
         | T_INT
         | T_FLOAT
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
    process : proc T_STOP process
            | proc T_STOP
    '''

def p_proc(p):
    '''
    proc : while
         | do_while
         | for
         | asignacion
         | condition
         | write
         | var_declare
    '''

def p_while(p):
    # while (expresion) { };
    '''
    while : T_WHILE T_EXP_START expression T_EXP_END block
    '''

def p_for(p):
    # for each item X in myArray { };
    '''
    for : T_FOR T_EXP_START asignacion T_STOP expression T_STOP asignacion T_EXP_END block
    '''

def p_do_while(p):
    # do { } while ();
    '''
    do_while : T_DO block T_WHILE T_EXP_START expression T_EXP_END
    '''

def p_asignacion(p):
    '''
    asignacion : id T_ASSIGN value
               | id T_ASSIGN T_ARR_START array T_ARR_END
               | id T_ASSIGN T_ARR_START T_ARR_END
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
    value : expression
          | T_STRING_CONST
          | T_TRUE
          | T_FALSE
    '''

def p_condition(p):
    '''
    condition : T_IF T_EXP_START expression T_EXP_END block else_if else
              | T_IF T_EXP_START expression T_EXP_END block else_if
              | T_IF T_EXP_START expression T_EXP_END block else
              | T_IF T_EXP_START expression T_EXP_END block
    '''

def p_else_if(p):
    '''
    else_if : T_ELSE T_IF T_EXP_START expression T_EXP_END block
    '''

def p_else(p):
    '''
    else : T_ELSE block
    '''

def p_write(p):
    'write : T_PRINT T_EXP_START concat T_EXP_END'

def p_concat(p):
    '''
    concat : T_STRING_CONST T_CONCAT concat
                  | expression T_CONCAT concat
                  | T_STRING_CONST
                  | expression
    '''

def p_expression(p):
    # Expresiones && y ||
    '''
    expression : exp T_OPREL expression
               | exp
    '''

def p_exp(p):
    # Expresiones de comparacion (<, >, !=, ==)
    '''
    exp : e T_OPCOMP exp
        | e
    '''

def p_e(p):
    # Expresion aritmeticas
    '''
    e : term T_OPARIT e
      | term
    '''

def p_term(p):
    '''
    term : factor T_OPFACT term
            | factor
    '''

def p_factor(p):
    '''
    factor : T_EXP_START expression T_EXP_END
           | T_OPARIT T_INT_CONST
           | T_OPARIT T_FLOAT_CONST
           | T_INT_CONST
           | T_FLOAT_CONST
           | id
    '''

def p_error(p):
    print 'Error de sintaxis!'
    print p

parser = yacc.yacc()

data = '''
program MyProgram;

function myFunc(int A, string B, boolean C)
{
    int i;
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

main
{
    float X;
    float Y;
    int A, B;
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
