#!/usr/bin/env python3
"""
Mini-entorno de compilación LL(1) (versión PyQt6).

- Lexer (tokenizador) según la gramática y notas:
    * IDs empiezan con letra: [A-Za-z][A-Za-z0-9]*
    * Strings: "Texto en string lit"
    * NUM: entero, decimal, notación científica (ej. 12.2, 123, 12e21, 13e-12)
    * Comentarios a ignorar: //[A-Za-z0-9]+// (y también //... por tolerancia)
- Parser: recursive-descent que implementa las producciones facilitadas (declaraciones + expresiones).
- Genera tabla de tokens y árbol de derivación (texto).
- GUI: editar/guardar código, mostrar tokens, errores léxicos, resultado del análisis sintáctico.

Implementación con PyQt6
"""

import re
import sys
from collections import namedtuple
from PyQt6.QtWidgets import (QApplication, QMainWindow, QWidget, QVBoxLayout, 
                           QHBoxLayout, QTextEdit, QSplitter, QPushButton, 
                           QTableWidget, QTableWidgetItem, QGroupBox,
                           QTabWidget, QLabel, QHeaderView, QMessageBox,
                           QFileDialog)
from PyQt6.QtCore import Qt
from PyQt6.QtGui import QColor, QTextCharFormat, QTextCursor, QFont, QSyntaxHighlighter

# ----------------------------
# Token structure
# ----------------------------
Token = namedtuple("Token", ["type", "lexeme", "line", "col"])

# ----------------------------
# LEXER
# ----------------------------
class LexerError(Exception):
    pass

class Lexer:
    def __init__(self):
        # Token definitions. Order matters: longer/multi-char tokens first.
        self.token_spec = [
            ("COMMENT_STRICT", r"//[A-Za-z0-9]+//"),  # strict comment style requested
            ("COMMENT", r"//[^\n]*"),                 # tolerant comment style
            ("NEWLINE", r"\n"),
            ("WHITESPACE", r"[ \t\r]+"),
            ("STRING", r'"([^"\\]|\\.)*"'),
            # Number: integer, decimal, optional exponent
            ("NUM", r"\d+(\.\d+)?([eE][+-]?\d+)?"),
            # Multi-char operators
            ("OR", r"\|\|"),
            ("AND", r"&&"),
            ("EQEQ", r"=="),
            ("NEQ", r"!="),
            ("LE", r"<="),
            ("GE", r">="),
            ("ARROW", r"->"),
            # Single-char operators / punctuation
            ("EQ", r"="),
            ("LT", r"<"),
            ("GT", r">"),
            ("PLUS", r"\+"),
            ("MINUS", r"-"),
            ("TIMES", r"\*"),
            ("DIV", r"/"),
            ("MOD", r"%"),
            ("NOT", r"!"),
            ("DOT", r"\."),
            ("COLON", r":"),
            ("SEMI", r";"),
            ("LBRACE", r"\{"),
            ("RBRACE", r"\}"),
            ("LPAREN", r"\("),
            ("RPAREN", r"\)"),
            ("LBRACK", r"\["),
            ("RBRACK", r"\]"),
            # ID must start with a letter
            ("ID", r"[A-Za-z][A-Za-z0-9]*"),
        ]

        # Keywords
        self.keywords = {
            "module":"MODULE",
            "import":"IMPORT",
            "as":"AS",
            "type":"TYPE",
            "struct":"STRUCT",
            "const":"CONST",
            "let":"LET",
            "fn":"FN",
            "int":"INT",
            "bool":"BOOL",
            "string":"STRING",
            "if":"IF",
            "else":"ELSE",
            "while":"WHILE",
            "return":"RETURN",
            "true":"TRUE",
            "false":"FALSE",
        }

        parts = []
        for name, pat in self.token_spec:
            parts.append("(?P<%s>%s)" % (name, pat))
        self.master_pat = re.compile("|".join(parts))

    def tokenize(self, code):
        pos = 0
        line = 1
        col = 1
        tokens = []
        m = None
        while pos < len(code):
            m = self.master_pat.match(code, pos)
            if not m:
                # Unknown char -> lexer error
                snippet = code[pos:pos+20].replace("\n","\\n")
                raise LexerError(f"Error léxico en línea {line}, columna {col}. Fragmento: '{snippet}'.")
            kind = m.lastgroup
            lexeme = m.group(kind)
            if kind == "NEWLINE":
                line += 1
                col = 1
            elif kind in ("WHITESPACE","COMMENT","COMMENT_STRICT"):
                # ignore, advance column by lexeme length
                # but keep track of newlines already handled
                col += len(lexeme)
            else:
                tok_type = self.keywords.get(lexeme, kind)
                token = Token(tok_type, lexeme, line, col)
                tokens.append(token)
                col += len(lexeme)
            pos = m.end()
        # Append EOF token
        tokens.append(Token("EOF","<<EOF>>", line, col))
        return tokens

# ----------------------------
# PARSER (recursive-descent) + Derivation Tree
# ----------------------------
class ParseError(Exception):
    def __init__(self, message, token, expected=None):
        super().__init__(message)
        self.token = token
        self.expected = expected or []

class Node:
    def __init__(self, name, children=None, token=None):
        self.name = name
        self.children = children if children is not None else []
        self.token = token  # for leaves
    def repr(self, indent=0):
        pad = "  "*indent
        if self.token:
            return f"{pad}{self.name}: {self.token.lexeme} (line {self.token.line})\n"
        s = f"{pad}{self.name}\n"
        for c in self.children:
            s += c.repr(indent+1)
        return s

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0
        self.current = tokens[0]

    def advance(self):
        self.pos += 1
        if self.pos < len(self.tokens):
            self.current = self.tokens[self.pos]
        else:
            self.current = Token("EOF","<<EOF>>", self.current.line, self.current.col)

    def expect(self, types):
        # types: single or list/set
        if isinstance(types, str):
            types = {types}
        else:
            types = set(types)
        if self.current.type in types:
            t = self.current
            self.advance()
            return t
        else:
            raise ParseError(f"Se esperaba {types}, encontrado {self.current.type}",
                             self.current, expected=list(types))

    # Entry: Program → ModuleDecl ImportList TopList EOF
    def parse(self):
        node = Node("Program")
        node.children.append(self.ModuleDecl())
        node.children.append(self.ImportList())
        node.children.append(self.TopList())
        # expect EOF
        node.children.append(Node("EOF", token=self.expect("EOF")))
        return node

    # ModuleDecl→ module QualID ';'
    def ModuleDecl(self):
        node = Node("ModuleDecl")
        node.children.append(Node("module", token=self.expect("MODULE")))
        node.children.append(self.QualID())
        node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # QualID → ID QualIDTail
    def QualID(self):
        node = Node("QualID")
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(self.QualIDTail())
        return node

    # QualIDTail → '.' ID QualIDTail | ɛ
    def QualIDTail(self):
        node = Node("QualIDTail")
        if self.current.type == "DOT":
            node.children.append(Node(".", token=self.expect("DOT")))
            node.children.append(Node("ID", token=self.expect("ID")))
            node.children.append(self.QualIDTail())
        else:
            node.children.append(Node("ε"))
        return node

    # ImportList → ImportDecl ImportList | ɛ
    def ImportList(self):
        node = Node("ImportList")
        if self.current.type == "IMPORT":
            node.children.append(self.ImportDecl())
            node.children.append(self.ImportList())
        else:
            node.children.append(Node("ε"))
        return node

    # ImportDecl → import QualID AsOpt ';'
    def ImportDecl(self):
        node = Node("ImportDecl")
        node.children.append(Node("import", token=self.expect("IMPORT")))
        node.children.append(self.QualID())
        node.children.append(self.AsOpt())
        node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # AsOpt→ as ID | ɛ
    def AsOpt(self):
        node = Node("AsOpt")
        if self.current.type == "AS":
            node.children.append(Node("as", token=self.expect("AS")))
            node.children.append(Node("ID", token=self.expect("ID")))
        else:
            node.children.append(Node("ε"))
        return node

    # TopList → TopDecl TopList | ɛ
    def TopList(self):
        node = Node("TopList")
        if self.current.type in ("TYPE","STRUCT","CONST","FN","LET"):
            node.children.append(self.TopDecl())
            node.children.append(self.TopList())
        else:
            node.children.append(Node("ε"))
        return node

    # TopDecl→ TypeDecl | StructDecl | ConstDecl | FunDecl | LetDecl
    def TopDecl(self):
        node = Node("TopDecl")
        if self.current.type == "TYPE":
            node.children.append(self.TypeDecl())
        elif self.current.type == "STRUCT":
            node.children.append(self.StructDecl())
        elif self.current.type == "CONST":
            node.children.append(self.ConstDecl())
        elif self.current.type == "FN":
            node.children.append(self.FunDecl())
        elif self.current.type == "LET":
            node.children.append(self.LetDecl())
        else:
            raise ParseError("TopDecl: producción inesperada", self.current, expected=["TYPE","STRUCT","CONST","FN","LET"])
        return node

    # TypeDecl→ type ID '=' Type ';'
    def TypeDecl(self):
        node = Node("TypeDecl")
        node.children.append(Node("type", token=self.expect("TYPE")))
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node("=", token=self.expect("EQ")))
        node.children.append(self.Type())
        node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # StructDecl → struct ID '{'FieldList '}';'
    def StructDecl(self):
        node = Node("StructDecl")
        node.children.append(Node("struct", token=self.expect("STRUCT")))
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node("{", token=self.expect("LBRACE")))
        node.children.append(self.FieldList())
        node.children.append(Node("}", token=self.expect("RBRACE")))
        node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # FieldList → Field FieldListTail | ɛ
    def FieldList(self):
        node = Node("FieldList")
        if self.current.type == "ID":
            node.children.append(self.Field())
            node.children.append(self.FieldListTail())
        else:
            node.children.append(Node("ε"))
        return node

    # FieldListTail → '.' Field FieldListTail | ɛ
    def FieldListTail(self):
        node = Node("FieldListTail")
        if self.current.type == "DOT":
            node.children.append(Node(".", token=self.expect("DOT")))
            node.children.append(self.Field())
            node.children.append(self.FieldListTail())
        else:
            node.children.append(Node("ε"))
        return node

    # Field → ID ':' Type
    def Field(self):
        node = Node("Field")
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node(":", token=self.expect("COLON")))
        node.children.append(self.Type())
        return node

    # ConstDecl→ const ID ':' Type '=' Expr ';'
    def ConstDecl(self):
        node = Node("ConstDecl")
        node.children.append(Node("const", token=self.expect("CONST")))
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node(":", token=self.expect("COLON")))
        node.children.append(self.Type())
        node.children.append(Node("=", token=self.expect("EQ")))
        node.children.append(self.Expr())
        node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # LetDecl → let ID ':' Type LetTail
    def LetDecl(self):
        node = Node("LetDecl")
        node.children.append(Node("let", token=self.expect("LET")))
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node(":", token=self.expect("COLON")))
        node.children.append(self.Type())
        node.children.append(self.LetTail())
        return node

    # LetTail→ '=' Expr ';' | ';'
    def LetTail(self):
        node = Node("LetTail")
        if self.current.type == "EQ":
            node.children.append(Node("=", token=self.expect("EQ")))
            node.children.append(self.Expr())
            node.children.append(Node(";", token=self.expect("SEMI")))
        elif self.current.type == "SEMI":
            node.children.append(Node(";", token=self.expect("SEMI")))
        else:
            raise ParseError("LetTail: se esperaba '=' o ';'", self.current, expected=["EQ","SEMI"])
        return node

    # FunDecl→ fn ID '('ParamListOpt ')' RetType Block
    def FunDecl(self):
        node = Node("FunDecl")
        node.children.append(Node("fn", token=self.expect("FN")))
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node("(", token=self.expect("LPAREN")))
        node.children.append(self.ParamListOpt())
        node.children.append(Node(")", token=self.expect("RPAREN")))
        node.children.append(self.RetType())
        node.children.append(self.Block())
        return node

    # ParamListOpt → ParamList | ɛ
    def ParamListOpt(self):
        node = Node("ParamListOpt")
        if self.current.type == "ID":
            node.children.append(self.ParamList())
        else:
            node.children.append(Node("ε"))
        return node

    # ParamList → Param ParamListTail
    def ParamList(self):
        node = Node("ParamList")
        node.children.append(self.Param())
        node.children.append(self.ParamListTail())
        return node

    # ParamListTail → '.' Param ParamListTail | ɛ
    def ParamListTail(self):
        node = Node("ParamListTail")
        if self.current.type == "DOT":
            node.children.append(Node(".", token=self.expect("DOT")))
            node.children.append(self.Param())
            node.children.append(self.ParamListTail())
        else:
            node.children.append(Node("ε"))
        return node

    # Param → ID ':' Type
    def Param(self):
        node = Node("Param")
        node.children.append(Node("ID", token=self.expect("ID")))
        node.children.append(Node(":", token=self.expect("COLON")))
        node.children.append(self.Type())
        return node

    # RetType → '->' Type | ɛ
    def RetType(self):
        node = Node("RetType")
        if self.current.type == "ARROW":
            node.children.append(Node("->", token=self.expect("ARROW")))
            node.children.append(self.Type())
        else:
            node.children.append(Node("ε"))
        return node

    # Type → SimpleType ArrOrFunType
    def Type(self):
        node = Node("Type")
        node.children.append(self.SimpleType())
        node.children.append(self.ArrOrFunType())
        return node

    # ArrOrFunType → '['']' ArrOrFunType | ɛ
    def ArrOrFunType(self):
        node = Node("ArrOrFunType")
        if self.current.type == "LBRACK":
            node.children.append(Node("[]", token=self.expect("LBRACK")))
            # expect RBRACK
            node.children.append(Node("]", token=self.expect("RBRACK")))
            node.children.append(self.ArrOrFunType())
        else:
            node.children.append(Node("ε"))
        return node

    # SimpleType → int | bool | string | QualID | '('Type ')'
    def SimpleType(self):
        node = Node("SimpleType")
        if self.current.type in ("INT","BOOL","STRING"):
            node.children.append(Node(self.current.type, token=self.expect(self.current.type)))
        elif self.current.type == "ID":
            node.children.append(self.QualID())
        elif self.current.type == "LPAREN":
            node.children.append(Node("(", token=self.expect("LPAREN")))
            node.children.append(self.Type())
            node.children.append(Node(")", token=self.expect("RPAREN")))
        else:
            raise ParseError("SimpleType: se esperaba tipo simple", self.current, expected=["INT","BOOL","STRING","ID","LPAREN"])
        return node

    # Block → '{'StmtList '}'
    def Block(self):
        node = Node("Block")
        node.children.append(Node("{", token=self.expect("LBRACE")))
        node.children.append(self.StmtList())
        node.children.append(Node("}", token=self.expect("RBRACE")))
        return node

    # StmtList → Stmt StmtList | ɛ
    def StmtList(self):
        node = Node("StmtList")
        if self.current.type in ("LBRACE","LET","IF","WHILE","RETURN","ID","NUM","STRING","TRUE","FALSE","LPAREN","NOT","MINUS"):
            node.children.append(self.Stmt())
            node.children.append(self.StmtList())
        else:
            node.children.append(Node("ε"))
        return node

    # Stmt → Block | LetDecl | ExprStmt | IfStmt | WhileStmt | ReturnStmt
    def Stmt(self):
        node = Node("Stmt")
        if self.current.type == "LBRACE":
            node.children.append(self.Block())
        elif self.current.type == "LET":
            node.children.append(self.LetDecl())
        elif self.current.type == "IF":
            node.children.append(self.IfStmt())
        elif self.current.type == "WHILE":
            node.children.append(self.WhileStmt())
        elif self.current.type == "RETURN":
            node.children.append(self.ReturnStmt())
        else:
            node.children.append(self.ExprStmt())
        return node

    # ExprStmt → Expr ';'
    def ExprStmt(self):
        node = Node("ExprStmt")
        node.children.append(self.Expr())
        node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # IfStmt → if '('Expr ')' Stmt ElseOpt
    def IfStmt(self):
        node = Node("IfStmt")
        node.children.append(Node("if", token=self.expect("IF")))
        node.children.append(Node("(", token=self.expect("LPAREN")))
        node.children.append(self.Expr())
        node.children.append(Node(")", token=self.expect("RPAREN")))
        node.children.append(self.Stmt())
        node.children.append(self.ElseOpt())
        return node

    # ElseOpt → else Stmt | ɛ
    def ElseOpt(self):
        node = Node("ElseOpt")
        if self.current.type == "ELSE":
            node.children.append(Node("else", token=self.expect("ELSE")))
            node.children.append(self.Stmt())
        else:
            node.children.append(Node("ε"))
        return node

    # WhileStmt → while '('Expr ')' Stmt
    def WhileStmt(self):
        node = Node("WhileStmt")
        node.children.append(Node("while", token=self.expect("WHILE")))
        node.children.append(Node("(", token=self.expect("LPAREN")))
        node.children.append(self.Expr())
        node.children.append(Node(")", token=self.expect("RPAREN")))
        node.children.append(self.Stmt())
        return node

    # ReturnStmt → return Expr ';' | return ';'
    def ReturnStmt(self):
        node = Node("ReturnStmt")
        node.children.append(Node("return", token=self.expect("RETURN")))
        if self.current.type == "SEMI":
            node.children.append(Node(";", token=self.expect("SEMI")))
        else:
            node.children.append(self.Expr())
            node.children.append(Node(";", token=self.expect("SEMI")))
        return node

    # --- EXPRESSIONS ---
    def Expr(self):
        # Expr → Assign
        node = Node("Expr")
        node.children.append(self.Assign())
        return node

    def Assign(self):
        # Assign→ Or AssignTail
        node = Node("Assign")
        node.children.append(self.Or())
        node.children.append(self.AssignTail())
        return node

    def AssignTail(self):
        # AssignTail → '=' Assign | ɛ
        node = Node("AssignTail")
        if self.current.type == "EQ":
            node.children.append(Node("=", token=self.expect("EQ")))
            node.children.append(self.Assign())
        else:
            node.children.append(Node("ε"))
        return node

    def Or(self):
        # Or → And OrTail
        node = Node("Or")
        node.children.append(self.And())
        node.children.append(self.OrTail())
        return node

    def OrTail(self):
        # OrTail→ '||' And OrTail | ɛ
        node = Node("OrTail")
        if self.current.type == "OR":
            node.children.append(Node("||", token=self.expect("OR")))
            node.children.append(self.And())
            node.children.append(self.OrTail())
        else:
            node.children.append(Node("ε"))
        return node

    def And(self):
        node = Node("And")
        node.children.append(self.Eq())
        node.children.append(self.AndTail())
        return node

    def AndTail(self):
        node = Node("AndTail")
        if self.current.type == "AND":
            node.children.append(Node("&&", token=self.expect("AND")))
            node.children.append(self.Eq())
            node.children.append(self.AndTail())
        else:
            node.children.append(Node("ε"))
        return node

    def Eq(self):
        node = Node("Eq")
        node.children.append(self.Rel())
        node.children.append(self.EqTail())
        return node

    def EqTail(self):
        node = Node("EqTail")
        if self.current.type == "EQEQ":
            node.children.append(Node("==", token=self.expect("EQEQ")))
            node.children.append(self.Rel())
            node.children.append(self.EqTail())
        elif self.current.type == "NEQ":
            node.children.append(Node("!=", token=self.expect("NEQ")))
            node.children.append(self.Rel())
            node.children.append(self.EqTail())
        else:
            node.children.append(Node("ε"))
        return node

    def Rel(self):
        node = Node("Rel")
        node.children.append(self.Add())
        node.children.append(self.RelTail())
        return node

    def RelTail(self):
        node = Node("RelTail")
        if self.current.type == "LT":
            node.children.append(Node("<", token=self.expect("LT")))
            node.children.append(self.Add())
            node.children.append(self.RelTail())
        elif self.current.type == "LE":
            node.children.append(Node("<=", token=self.expect("LE")))
            node.children.append(self.Add())
            node.children.append(self.RelTail())
        elif self.current.type == "GT":
            node.children.append(Node(">", token=self.expect("GT")))
            node.children.append(self.Add())
            node.children.append(self.RelTail())
        elif self.current.type == "GE":
            node.children.append(Node(">=", token=self.expect("GE")))
            node.children.append(self.Add())
            node.children.append(self.RelTail())
        else:
            node.children.append(Node("ε"))
        return node

    def Add(self):
        node = Node("Add")
        node.children.append(self.Mul())
        node.children.append(self.AddTail())
        return node

    def AddTail(self):
        node = Node("AddTail")
        if self.current.type == "PLUS":
            node.children.append(Node("+", token=self.expect("PLUS")))
            node.children.append(self.Mul())
            node.children.append(self.AddTail())
        elif self.current.type == "MINUS":
            node.children.append(Node("-", token=self.expect("MINUS")))
            node.children.append(self.Mul())
            node.children.append(self.AddTail())
        else:
            node.children.append(Node("ε"))
        return node

    def Mul(self):
        node = Node("Mul")
        node.children.append(self.Unary())
        node.children.append(self.MulTail())
        return node

    def MulTail(self):
        node = Node("MulTail")
        if self.current.type == "TIMES":
            node.children.append(Node("*", token=self.expect("TIMES")))
            node.children.append(self.Unary())
            node.children.append(self.MulTail())
        elif self.current.type == "DIV":
            node.children.append(Node("/", token=self.expect("DIV")))
            node.children.append(self.Unary())
            node.children.append(self.MulTail())
        elif self.current.type == "MOD":
            node.children.append(Node("%", token=self.expect("MOD")))
            node.children.append(self.Unary())
            node.children.append(self.MulTail())
        else:
            node.children.append(Node("ε"))
        return node

    def Unary(self):
        node = Node("Unary")
        if self.current.type == "NOT":
            node.children.append(Node("!", token=self.expect("NOT")))
            node.children.append(self.Unary())
        elif self.current.type == "MINUS":
            node.children.append(Node("-", token=self.expect("MINUS")))
            node.children.append(self.Unary())
        else:
            node.children.append(self.Postfix())
        return node

    def Postfix(self):
        node = Node("Postfix")
        node.children.append(self.Primary())
        node.children.append(self.PostfixTail())
        return node

    def PostfixTail(self):
        node = Node("PostfixTail")
        if self.current.type == "LPAREN":
            node.children.append(Node("(", token=self.expect("LPAREN")))
            node.children.append(self.ArgListOpt())
            node.children.append(Node(")", token=self.expect("RPAREN")))
            node.children.append(self.PostfixTail())
        elif self.current.type == "LBRACK":
            node.children.append(Node("[", token=self.expect("LBRACK")))
            node.children.append(self.Expr())
            node.children.append(Node("]", token=self.expect("RBRACK")))
            node.children.append(self.PostfixTail())
        elif self.current.type == "DOT":
            node.children.append(Node(".", token=self.expect("DOT")))
            node.children.append(Node("ID", token=self.expect("ID")))
            node.children.append(self.PostfixTail())
        else:
            node.children.append(Node("ε"))
        return node

    def Primary(self):
        node = Node("Primary")
        if self.current.type == "ID":
            node.children.append(Node("ID", token=self.expect("ID")))
        elif self.current.type == "NUM":
            node.children.append(Node("NUM", token=self.expect("NUM")))
        elif self.current.type == "STRING":
            node.children.append(Node("STRING", token=self.expect("STRING")))
        elif self.current.type == "TRUE":
            node.children.append(Node("true", token=self.expect("TRUE")))
        elif self.current.type == "FALSE":
            node.children.append(Node("false", token=self.expect("FALSE")))
        elif self.current.type == "LPAREN":
            node.children.append(Node("(", token=self.expect("LPAREN")))
            node.children.append(self.Expr())
            node.children.append(Node(")", token=self.expect("RPAREN")))
        else:
            raise ParseError("Primary: se esperaba ID/NUM/STRING/true/false/('Expr')", self.current,
                             expected=["ID","NUM","STRING","TRUE","FALSE","LPAREN"])
        return node

    def ArgListOpt(self):
        node = Node("ArgListOpt")
        if self.current.type in ("ID","NUM","STRING","TRUE","FALSE","LPAREN","NOT","MINUS"):
            node.children.append(self.ArgList())
        else:
            node.children.append(Node("ε"))
        return node

    def ArgList(self):
        node = Node("ArgList")
        node.children.append(self.Expr())
        node.children.append(self.ArgListTail())
        return node

    def ArgListTail(self):
        node = Node("ArgListTail")
        if self.current.type == "DOT":
            node.children.append(Node(".", token=self.expect("DOT")))
            node.children.append(self.Expr())
            node.children.append(self.ArgListTail())
        else:
            node.children.append(Node("ε"))
        return node

# ----------------------------
# Syntax Highlighter
# ----------------------------
class SyntaxHighlighter(QSyntaxHighlighter):
    """
    Resaltador de sintaxis para el editor de código.
    """
    
    def __init__(self, parent=None):
        super().__init__(parent)
        self.highlighting_rules = []
        
        # Formato para palabras clave
        keyword_format = QTextCharFormat()
        keyword_format.setForeground(QColor("#569CD6"))  # Azul para palabras clave
        keyword_format.setFontWeight(QFont.Weight.Bold)
        keywords = [
            "module", "import", "as", "type", "struct", "fn", "let", "const", 
            "if", "else", "while", "return", "int", "bool", "string", "true", "false"
        ]
        for keyword in keywords:
            pattern = fr'\b{keyword}\b'
            self.highlighting_rules.append((pattern, keyword_format))
        
        # Formato para operadores
        operator_format = QTextCharFormat()
        operator_format.setForeground(QColor("#D4D4D4"))  # Gris claro para operadores
        operators = [
            '=', '==', '!=', '<', '<=', '>', '>=', 
            '+', '-', '*', '/', '%', 
            '&&', '||', '!', 
            '->', ';', ',', '.', '(', ')', '{', '}', '[', ']'
        ]
        for op in operators:
            # Escapamos los caracteres especiales
            escaped_op = ''.join([f'\\{char}' if char in '()[]{}*.+^$?|' else char for char in op])
            pattern = f'{escaped_op}'
            self.highlighting_rules.append((pattern, operator_format))
        
        # Formato para literales numéricos
        number_format = QTextCharFormat()
        number_format.setForeground(QColor("#B5CEA8"))  # Verde para números
        self.highlighting_rules.append((r'\b\d+(\.\d+)?([eE][+-]?\d+)?\b', number_format))
        
        # Formato para literales de cadena
        string_format = QTextCharFormat()
        string_format.setForeground(QColor("#CE9178"))  # Naranja para cadenas
        self.highlighting_rules.append((r'"[^"]*"', string_format))
        
        # Formato para comentarios de línea
        comment_format = QTextCharFormat()
        comment_format.setForeground(QColor("#6A9955"))  # Verde oscuro para comentarios
        self.highlighting_rules.append((r'//.*', comment_format))
        
    def highlightBlock(self, text):
        """
        Aplica el resaltado de sintaxis al bloque de texto.
        """
        for pattern, format in self.highlighting_rules:
            expression = re.compile(pattern)
            for match in expression.finditer(text):
                start = match.start()
                length = match.end() - start
                self.setFormat(start, length, format)

# ----------------------------
# GUI with PyQt6
# ----------------------------
class CompilerGUI(QMainWindow):
    def __init__(self):
        super().__init__()
        
        self.lexer = Lexer()
        
        self.setWindowTitle("Mini Compilador LL(1) - PyQt6")
        self.setGeometry(100, 100, 1100, 700)
        
        # Layout principal
        central_widget = QWidget()
        self.setCentralWidget(central_widget)
        main_layout = QVBoxLayout(central_widget)
        
        # Splitter para dividir la pantalla
        splitter = QSplitter(Qt.Orientation.Horizontal)
        main_layout.addWidget(splitter)
        
        # Panel izquierdo (editor)
        left_panel = QWidget()
        left_layout = QVBoxLayout(left_panel)
        
        # Barra de herramientas
        toolbar = QWidget()
        toolbar_layout = QHBoxLayout(toolbar)
        toolbar_layout.setContentsMargins(0, 0, 0, 0)
        
        self.btn_open = QPushButton("Abrir")
        self.btn_save = QPushButton("Guardar")
        self.btn_lex = QPushButton("Analizar Léxico")
        self.btn_parse = QPushButton("Analizar Sintáctico")
        
        self.btn_open.clicked.connect(self.open_file)
        self.btn_save.clicked.connect(self.save_file)
        self.btn_lex.clicked.connect(self.run_lex)
        self.btn_parse.clicked.connect(self.run_parse)
        
        toolbar_layout.addWidget(self.btn_open)
        toolbar_layout.addWidget(self.btn_save)
        toolbar_layout.addWidget(self.btn_lex)
        toolbar_layout.addWidget(self.btn_parse)
        toolbar_layout.addStretch(1)
        
        left_layout.addWidget(toolbar)
        
        # Editor de código
        self.editor = QTextEdit()
        self.editor.setFont(QFont("Consolas", 11))
        left_layout.addWidget(self.editor)
        
        # Aplicar resaltador de sintaxis
        self.highlighter = SyntaxHighlighter(self.editor.document())
        
        # Panel derecho (resultados)
        right_panel = QWidget()
        right_layout = QVBoxLayout(right_panel)
        
        # Pestañas para resultados
        self.tabs = QTabWidget()
        right_layout.addWidget(self.tabs)
        
        # Pestaña de tokens
        tokens_tab = QWidget()
        tokens_layout = QVBoxLayout(tokens_tab)
        self.tokens_table = QTableWidget(0, 4)
        self.tokens_table.setHorizontalHeaderLabels(["Tipo", "Lexema", "Línea", "Columna"])
        header = self.tokens_table.horizontalHeader()
        header.setSectionResizeMode(QHeaderView.ResizeMode.Stretch)
        tokens_layout.addWidget(self.tokens_table)
        self.tabs.addTab(tokens_tab, "Tokens")
        
        # Pestaña de errores
        errors_tab = QWidget()
        errors_layout = QVBoxLayout(errors_tab)
        self.text_errors = QTextEdit()
        self.text_errors.setReadOnly(True)
        errors_layout.addWidget(self.text_errors)
        self.tabs.addTab(errors_tab, "Errores")
        
        # Pestaña de árbol de sintaxis
        parse_tab = QWidget()
        parse_layout = QVBoxLayout(parse_tab)
        self.text_parse = QTextEdit()
        self.text_parse.setReadOnly(True)
        self.text_parse.setFont(QFont("Consolas", 10))
        parse_layout.addWidget(self.text_parse)
        self.tabs.addTab(parse_tab, "Sintaxis / Árbol")
        
        # Añadir paneles al splitter
        splitter.addWidget(left_panel)
        splitter.addWidget(right_panel)
        splitter.setSizes([600, 500])
        
        # Barra de estado
        self.status_bar = self.statusBar()
        self.status_bar.showMessage("Listo")
        
        # Cargar ejemplo
        self.load_example_code()
    
    def load_example_code(self):
        """Carga un código de ejemplo en el editor."""
        sample = """module ejemplo;
import lib as l;
type X = int;
struct Persona { id:int.name:string };
const PI : int = 3;
let x : int = 10;
fn suma(a:int.b:int) -> int {
    if (a > b && true) {
        return a + b;
    } else {
        return 0;
    }
}
"""
        self.editor.setText(sample)
    
    def open_file(self):
        """Abre un archivo para editar."""
        file_path, _ = QFileDialog.getOpenFileName(
            self,
            "Abrir archivo",
            "",
            "Archivos de texto (*.txt);;Archivos de código fuente (*.src *.my);;Todos los archivos (*.*)"
        )
        
        if file_path:
            try:
                with open(file_path, "r", encoding="utf-8") as file:
                    content = file.read()
                    self.editor.setText(content)
                    self.status_bar.showMessage(f"Abierto: {file_path}")
            except Exception as e:
                QMessageBox.critical(self, "Error al abrir el archivo", str(e))
    
    def save_file(self):
        """Guarda el contenido del editor en un archivo."""
        file_path, _ = QFileDialog.getSaveFileName(
            self,
            "Guardar archivo",
            "",
            "Archivos de texto (*.txt);;Archivos de código fuente (*.src *.my);;Todos los archivos (*.*)"
        )
        
        if file_path:
            try:
                with open(file_path, "w", encoding="utf-8") as file:
                    file.write(self.editor.toPlainText())
                    self.status_bar.showMessage(f"Guardado: {file_path}")
            except Exception as e:
                QMessageBox.critical(self, "Error al guardar el archivo", str(e))
    
    def run_lex(self):
        """Ejecuta el análisis léxico."""
        code = self.editor.toPlainText()
        try:
            tokens = self.lexer.tokenize(code)
        except LexerError as e:
            self.show_tokens([])
            self.text_errors.clear()
            self.text_errors.setTextColor(QColor("red"))
            self.text_errors.insertPlainText(str(e))
            self.status_bar.showMessage("Error léxico")
            QMessageBox.critical(self, "Error Léxico", str(e))
            return
        
        # Mostrar tokens
        self.show_tokens(tokens)
        self.text_errors.clear()
        self.text_parse.clear()
        self.status_bar.showMessage(f"Tokens generados: {len(tokens)}")
    
    def show_tokens(self, tokens):
        """Muestra los tokens en la tabla."""
        self.tokens_table.setRowCount(0)
        for token in tokens:
            row_position = self.tokens_table.rowCount()
            self.tokens_table.insertRow(row_position)
            self.tokens_table.setItem(row_position, 0, QTableWidgetItem(token.type))
            self.tokens_table.setItem(row_position, 1, QTableWidgetItem(token.lexeme))
            self.tokens_table.setItem(row_position, 2, QTableWidgetItem(str(token.line)))
            self.tokens_table.setItem(row_position, 3, QTableWidgetItem(str(token.col)))
    
    def run_parse(self):
        """Ejecuta el análisis sintáctico."""
        code = self.editor.toPlainText()
        try:
            tokens = self.lexer.tokenize(code)
        except LexerError as e:
            self.text_errors.clear()
            self.text_errors.setTextColor(QColor("red"))
            self.text_errors.insertPlainText("ERROR LÉXICO:\n" + str(e))
            self.status_bar.showMessage("Error léxico")
            QMessageBox.critical(self, "Error Léxico", str(e))
            return
        
        # Si el análisis léxico es correcto, pasamos al sintáctico
        parser = Parser(tokens)
        try:
            tree = parser.parse()
        except ParseError as pe:
            self.text_errors.clear()
            self.text_errors.setTextColor(QColor("red"))
            msg = f"ERROR SINTÁCTICO:\nCerca del token '{pe.token.lexeme}' (tipo {pe.token.type}) en línea {pe.token.line}, columna {pe.token.col}\n"
            if pe.expected:
                msg += "Se esperaba: " + ", ".join(pe.expected) + "\n"
            msg += str(pe)
            self.text_errors.insertPlainText(msg)
            self.text_parse.clear()
            self.status_bar.showMessage("Error sintáctico")
            QMessageBox.critical(self, "Error Sintáctico", msg)
            return
        
        # Si el análisis sintáctico es correcto
        self.text_errors.clear()
        self.text_parse.clear()
        self.text_parse.insertPlainText(tree.repr())
        self.status_bar.showMessage("Análisis sintáctico exitoso")
        QMessageBox.information(self, "Éxito", "El código pasó el análisis léxico y sintáctico correctamente.")

# ----------------------------
# Run app
# ----------------------------
def main():
    app = QApplication(sys.argv)
    window = CompilerGUI()
    window.show()
    sys.exit(app.exec())

if __name__ == "__main__":
    main()