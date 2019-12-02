{
module Parser where
import Lexer
import Grammar
}

%name parser
%tokentype { Token }
%error { parseError }

%token
    open                                    { TSym '(' }
    close                                   { TSym ')' }
    lambda                                  { TSym '\\' }
    point                                   { TSym '.' }
    var                                     { TVar $$ }

%%

Expression:
    Applying lambda var point Expression    { ApplyingLambda $1 (Lambda $3 $5) }
    |   lambda var point Expression         { Lambda $2 $4 }
    |   Applying                            { $1 }

Applying:
    Applying Atom                           { Applying $1 $2 }
    |   Atom                                { $1 }

Atom:
    open Expression close                   { $2 }
    |   var                                 { Variable $1 }

{
parseError _ = error "Parse error"
}