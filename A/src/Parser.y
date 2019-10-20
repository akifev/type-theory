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
    Space lambda var point Expression       { Lambda $1 $3 $5 }
    | lambda var point Expression           { Lambda [] $2 $4 }
    | Space                                 { Space (reverse $1) }

Space:
    Space Atom                              { $2 : $1 }
    |   Atom                                { $1 : [] }

Atom:
    open Expression close                   { Atom $2 }
    |   var                                 { Var $1 }

{
parseError _ = error "Parse error"
}