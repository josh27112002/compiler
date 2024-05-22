import compilerTools.Token;
/*hola*/
%%
%class Lexer
%type Token
%line
%column
%{
    private Token token(String lexeme, String lexicalComp, int line, int column){
        return new Token(lexeme, lexicalComp, line+1, column+1);
    }
%}
/* Variables básicas de comentarios y espacios */
TerminadorDeLinea = \r|\n|\r\n
EntradaDeCaracter = [^\r\n]
EspacioEnBlanco = {TerminadorDeLinea} | [ \t\f]
ComentarioTradicional = "/*"([^*]|(\*+[^*/]))*\*+"/"
FinDeLineaComentario = "//"[^\\n]* {TerminadorDeLinea}?
ComentarioDeDocumentacion = "/\\*\\*"([^*]|(\*+[^*/]))*\*+"/"

/* Comentario */
Comentario = {ComentarioTradicional} | {FinDeLineaComentario} | {ComentarioDeDocumentacion}

/* Identificador */
Letra = [A-Za-zÑñ_ÁÉÍÓÚáéíóúÜü]
Digito = [0-9]
Identificador = {Letra}({Letra}|{Digito})*
Numero = {Digito}+

/* Número */
Numero = 0 | [1-9][0-9]*
Real = {Numero}"."{Numero}
%%

/* Comentarios o espacios en blanco */
{Comentario}|{EspacioEnBlanco} { /*Ignorar*/ }

/* Número */
{Real} { return token(yytext(), "REAL", yyline, yycolumn); }
{Numero} { return token(yytext(), "NUMERO", yyline, yycolumn); }

/* Operadores */
"+" { return token(yytext(), "SUMA", yyline, yycolumn); }
"-" { return token(yytext(), "RESTA", yyline, yycolumn); }
"/" { return token(yytext(), "DIVISION", yyline, yycolumn); }
"*" { return token(yytext(), "MULTIPLICACION", yyline, yycolumn); }

/* Lógicos */
"==" { return token(yytext(), "IGUAL", yyline, yycolumn); }
"!=" { return token(yytext(), "DIFERENTE", yyline, yycolumn); }
">" { return token(yytext(), "MAYORQUE", yyline, yycolumn); }
"<" { return token(yytext(), "MENORQUE", yyline, yycolumn); }
">=" { return token(yytext(), "MAYORIGUALQUE", yyline, yycolumn); }
"<=" { return token(yytext(), "MENORIGUALQUE", yyline, yycolumn); }

"." { return token(yytext(), "PUNTO", yyline, yycolumn); }
"," { return token(yytext(), "COMA", yyline, yycolumn); }
":" { return token(yytext(), "DOSPUNTOS", yyline, yycolumn); }
";" { return token(yytext(), "PUNTOCOMA", yyline, yycolumn); }
"\'" { return token(yytext(), "COMILLASIMPLE", yyline, yycolumn); }
"\"" { return token(yytext(), "COMILLADOBLE", yyline, yycolumn); }
"=" { return token(yytext(), "ASIGNACION", yyline, yycolumn); }
"(" { return token(yytext(), "PARENTESISABIERTO", yyline, yycolumn); }
")" { return token(yytext(), "PARENTESISCERRADO", yyline, yycolumn); }
"{" { return token(yytext(), "LLAVEABIERTA", yyline, yycolumn); }
"}" { return token(yytext(), "LLAVECERRADA", yyline, yycolumn); }
"[" { return token(yytext(), "CORCHETEABIERTO", yyline, yycolumn); }
"]" { return token(yytext(), "CORCHETECERRADO", yyline, yycolumn); }
"++" { return token(yytext(), "INCREMENTO", yyline, yycolumn); }
"--" { return token(yytext(), "DECREMENTO", yyline, yycolumn); }

/* Palabras reservadas de varios lenguajes */
/* PL/SQL */
"DECLARE" | "BEGIN" | "END" | "EXCEPTION" | "LOOP" | "FOR" | "WHILE" | "IF" | "THEN" | "ELSE" | "ELSIF" | "EXIT" | "GOTO" | "NULL" | "WHEN" { return token(yytext(), "PLSQL_KEYWORD", yyline, yycolumn); }

/* T-SQL */
"SELECT" | "FROM" | "WHERE" | "INSERT" | "UPDATE" | "DELETE" | "MERGE" | "JOIN" | "INNER" | "OUTER" | "LEFT" | "RIGHT" | "FULL" | "CROSS" | "ON" | "GROUP" | "BY" | "ORDER" | "HAVING" | "TOP" | "DISTINCT" | "AS" | "INTO" | "VALUES" | "SET" | "EXEC" | "DECLARE" | "BEGIN" | "END" | "TRANSACTION" | "COMMIT" | "ROLLBACK" | "SAVEPOINT" | "LOCK" | "WAITFOR" | "WHILE" | "IF" | "ELSE" | "PRINT" { return token(yytext(), "TSQL_KEYWORD", yyline, yycolumn); }

/* C++ */
"alignas" | "alignof" | "and" | "and_eq" | "asm" | "atomic_cancel" | "atomic_commit" | "atomic_noexcept" | "auto" | "bitand" | "bitor" | "bool" | "break" | "case" | "catch" | "char" | "char8_t" | "char16_t" | "char32_t" | "class" | "compl" | "concept" | "const" | "consteval" | "constexpr" | "constinit" | "const_cast" | "continue" | "co_await" | "co_return" | "co_yield" | "decltype" | "default" | "delete" | "do" | "double" | "dynamic_cast" | "else" | "enum" | "explicit" | "export" | "extern" | "false" | "float" | "for" | "friend" | "goto" | "if" | "inline" | "int" | "long" | "mutable" | "namespace" | "new" | "noexcept" | "not" | "not_eq" | "nullptr" | "operator" | "or" | "or_eq" | "private" | "protected" | "public" | "register" | "reinterpret_cast" | "requires" | "return" | "short" | "signed" | "sizeof" | "static" | "static_assert" | "static_cast" | "struct" | "switch" | "synchronized" | "template" | "this" | "thread_local" | "throw" | "true" | "try" | "typedef" | "typeid" | "typename" | "union" | "unsigned" | "using" | "virtual" | "void" | "volatile" | "wchar_t" | "while" | "xor" | "xor_eq" { return token(yytext(), "CPP_KEYWORD", yyline, yycolumn); }

/* Pascal */
"and" | "array" | "begin" | "case" | "const" | "div" | "do" | "downto" | "else" | "end" | "file" | "for" | "function" | "goto" | "if" | "in" | "label" | "mod" | "nil" | "not" | "of" | "or" | "packed" | "procedure" | "program" | "record" | "repeat" | "set" | "then" | "to" | "type" | "until" | "var" | "while" | "with" { return token(yytext(), "PASCAL_KEYWORD", yyline, yycolumn); }

/* JavaScript */
"abstract" | "arguments" | "await" | "boolean" | "break" | "byte" | "case" | "catch" | "char" | "class" | "const" | "continue" | "debugger" | "default" | "delete" | "do" | "double" | "else" | "enum" | "eval" | "export" | "extends" | "false" | "final" | "finally" | "float" | "for" | "function" | "goto" | "if" | "implements" | "import" | "in" | "instanceof" | "int" | "interface" | "let" | "long" | "native" | "new" | "null" | "package" | "private" | "protected" | "public" | "return" | "short" | "static" | "super" | "switch" | "synchronized" | "this" | "throw" | "throws" | "transient" | "true" | "try" | "typeof" | "var" | "void" | "volatile" | "while" | "with" | "yield" { return token(yytext(), "JS_KEYWORD", yyline, yycolumn); }

/* HTML */
"<a>" | "<abbr>" | "<address>" | "<area>" | "<article>" | "<aside>" | "<audio>" | "<b>" | "<base>" | "<bdi>" | "<bdo>" | "<blockquote>" | "<body>" | "<br>" | "<button>" | "<canvas>" | "<caption>" | "<cite>" | "<code>" | "<col>" | "<colgroup>" | "<data>" | "<datalist>" | "<dd>" | "<del>" | "<details>" | "<dfn>" | "<dialog>" | "<div>" | "<dl>" | "<dt>" | "<em>" | "<embed>" | "<fieldset>" | "<figcaption>" | "<figure>" | "<footer>" | "<form>" | "<h1>" | "<h2>" | "<h3>" | "<h4>" | "<h5>" | "<h6>" | "<head>" | "<header>" | "<hgroup>" | "<hr>" | "<html>" | "<i>" | "<iframe>" | "<img>" | "<input>" | "<ins>" | "<kbd>" | "<label>" | "<legend>" | "<li>" | "<link>" | "<main>" | "<map>" | "<mark>" | "<menu>" | "<meta>" | "<meter>" | "<nav>" | "<noscript>" | "<object>" | "<ol>" | "<optgroup>" | "<option>" | "<output>" | "<p>" | "<param>" | "<picture>" | "<pre>" | "<progress>" | "<q>" | "<rp>" | "<rt>" | "<ruby>" | "<s>" | "<samp>" | "<script>" | "<section>" | "<select>" | "<small>" | "<source>" | "<span>" | "<strong>" | "<style>" | "<sub>" | "<summary>" | "<sup>" | "<table>" | "<tbody>" | "<td>" | "<template>" | "<textarea>" | "<tfoot>" | "<th>" | "<thead>" | "<time>" | "<title>" | "<tr>" | "<track>" | "<u>" | "<ul>" | "<var>" | "<video>" | "<wbr>" | "<!DOCTYPE html>" { return token(yytext(), "HTML_TAG", yyline, yycolumn); }

/* Python */
"False" | "None" | "True" | "and" | "as" | "assert" | "async" | "await" | "break" | "class" | "continue" | "def" | "del" | "elif" | "else" | "except" | "finally" | "for" | "from" | "global" | "if" | "import" | "in" | "is" | "lambda" | "nonlocal" | "not" | "or" | "pass" | "raise" | "return" | "try" | "while" | "with" | "yield" { return token(yytext(), "PYTHON_KEYWORD", yyline, yycolumn); }

/* IDs */
{Identificador} { return token(yytext(), "ID", yyline, yycolumn); }

/* Error */
. { return token(yytext(), "ERROR", yyline, yycolumn); }