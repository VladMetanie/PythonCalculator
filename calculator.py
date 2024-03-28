import ply.lex as lex
import ply.yacc as yacc
import math

tokens = [
    'INT',
    'FLOAT',
    'NAME',
    'PLUS',
    'MINUS',
    'DIVIDE',
    'MULTIPLY',
    'EQUALS',
    'POWER',
    'RAD',
    'LOG',
    'SIN',
    'COS',
    'TG',
    'COT',
    'LPAREN',
    'RPAREN'
]

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='
t_POWER = r'\*\*'
t_LPAREN = r'\('
t_RPAREN = r'\)'

def t_RAD(t):
    r'rad'
    return t

def t_LOG(t):
    r'log'
    return t

def t_SIN(t):
    r'sin'
    return t

def t_COS(t):
    r'cos'
    return t

def t_TG(t):
    r'tg'
    return t

def t_COT(t):
    r'cot'
    return t

t_ignore = r' '

def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t

def t_error(t):
    print("Illegal characters!")
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (
    
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE'),
    ('right', 'POWER'),
    ('left', 'RAD', 'LOG', 'SIN', 'COS', 'TG', 'COT')
)

def p_calc(p):
    '''
    calc : expression
         | var_assign
         | empty
    '''
    print(run(p[1]))

def p_var_assign(p):
    '''
    var_assign : NAME EQUALS expression
    '''
    p[0] = ('=', p[1], p[3])

def p_expression(p):
    '''
    expression : expression MULTIPLY expression
               | expression DIVIDE expression
               | expression PLUS expression
               | expression MINUS expression
               | expression POWER expression
               | RAD LPAREN expression RPAREN
               | LOG LPAREN expression RPAREN
               | SIN LPAREN expression RPAREN
               | COS LPAREN expression RPAREN
               | TG LPAREN expression RPAREN
               | COT LPAREN expression RPAREN
    '''
    if len(p) == 5:
        if p[1] == 'log':
            p[0] = ('log', p[3])
        elif p[1] == 'rad':
            p[0] = ('rad', p[3])
        elif p[1] == 'sin':
            p[0] = ('sin', p[3])
        elif p[1] == 'cos':
            p[0] = ('cos', p[3])
        elif p[1] == 'tg':
            p[0] = ('tg', p[3])
        elif p[1] == 'cot':
            p[0] = ('cot', p[3])
    else:
        p[0] = (p[2], p[1], p[3])

def p_expression_int_float(p):
    '''
    expression : INT
               | FLOAT
    '''
    p[0] = p[1]

def p_expression_var(p):
    '''
    expression : NAME
    '''
    p[0] = ('var', p[1])

def p_expression_group(p):
    '''
    expression : LPAREN expression RPAREN
    '''
    p[0] = p[2]

def p_error(p):
    print("Syntax error found!")

def p_empty(p):
    '''
    empty :
    '''
    p[0] = None

parser = yacc.yacc()
env = {}

def run(p):
    global env
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        elif p[0] == '-':
            return run(p[1]) - run(p[2])
        elif p[0] == '*':
            return run(p[1]) * run(p[2])
        elif p[0] == '/':
            return run(p[1]) / run(p[2])
        elif p[0] == '**':
            return run(p[1]) ** run(p[2])
        elif p[0] == 'rad':
            return math.sqrt(run(p[1]))
        elif p[0] == 'log':
            return math.log(run(p[1]))
        elif p[0] == 'sin':
            return round(math.sin(run(p[1])*math.pi/180),4)
        elif p[0] == 'cos':
            return round(math.cos(run(p[1])*math.pi/180),4)
        elif p[0] == 'tg':
            return round(math.tan(run(p[1])*math.pi/180),4)
        elif p[0] == 'cot':
            return round(1 / math.tan((run(p[1])*math.pi/180)),4)
        elif p[0] == '=':
            env[p[1]] = run(p[2])
            return ''
        elif p[0] == 'var':
            if p[1] not in env:
                return 'Undeclared variable found!'
            else:
                return env[p[1]]
    else:
        return p

while True:
    try:
        s = input('>> ')
    except EOFError:
        break
    parser.parse(s)
