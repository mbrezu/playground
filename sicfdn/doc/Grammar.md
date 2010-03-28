
# Subset of JavaScript that I will implement in the first version

The only type for values is integer and functions. 'null' is an
allowed value. We allow function definitions with assignments. Values
assigned are expressions.

All functions are anonymous (to create a named function, assign the
function value to a variable).

The runtime contains a `print` function which can be used to print one
value.

The semicolon is required as a simple statement terminator.

Functions will be treated as lambdas in SICP, with an environment
evaluation model.

There is an `if` statement. Used with recursion it can be used to code
loops. There is to tail cail optimization.

There is no 'var' keyword, all variables are declared on first
assignment to them, and they are all local (belong to the environment
frame in which they were declared; variables declared in the 'root'
environment are the equivalent of globals).

# Grammar (EBNF)

    (* General Structure *)

    Program = StatementList

    StatementList = [Statement, { Statement }]

    Statement = FunctionCallStatement
              | AssignmentStatement
              | IfStatement
              | CompoundStatement
              | ReturnStatement

    (* Statements *)

    FunctionCallStatement = ExprFunctionCall, ';'

    AssignmentStatement = Identifier, '=', Expression, ';'

    IfStatement = 'if', '(', Expression ')', Statement, [ 'else', Statement ]

    CompoundStatement = '{', StatementList, '}'

    ReturnStatement = 'return', Expression, ';'

    (* Expressions *)

    Expression = ExprLogicalOr | FunctionExpression

    FunctionExpression =
        'function', '(', [Identifier, {',', Identifier}], ')', '{', StatementList, '}'

    ExprLogicalOr = ExprLogicalAnd, ['||', ExprLogicalAnd]

    ExprLogicalAnd = ExprRelational, ['&&', ExprRelational]

    ExprRelational = ExprSum, [('<' | '<=' | '==' | '!=' | '>=' | '>'), ExprSum]

    ExprSum = ExprMult, [('+' | '-'), ExprSum]

    ExprMult = ExprUnary, [('*' | '/'), ExprMult]

    ExprUnary = ExprParens | '-', ExprUnary | '!', ExprUnary | ExprTerminal

    ExprParens = '(', Expression, ')'
               | '(', FunctionExpression, ')', '(', [Expression, {',', Expression }], ')'

    ExprTerminal = Number | Identifier | ExprFunctionCall | 'null'

    ExprFunctionCall =
        ExprUnary, '(', [Expression, {',', Expression }], ')'

    (* Terminals *)

    Identifier = ? sequence of letters, digits, and underscores,
                 starting with a letter or underscore ?

    Number = ['-'], ? Sequence of digits ?, ['.', ? Sequence of digits ]

