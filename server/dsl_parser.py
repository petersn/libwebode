"""
DSL Parser
"""

import string
import pprint

identifier_start_chars = set(string.ascii_letters + "_$")
identifier_chars       = set(string.ascii_letters + string.digits + "_'")
def valid_identifier(s):
    return s and s[0] in identifier_start_chars and all(c in identifier_chars for c in s)

whitespace = set(" \t\n")
symbol_characters = set("()[]{}.,?=+-*/%:<>!;|&~^")
length_two_symbols = {
    "<=", ">=", "==", "!=",
    "+=", "-=", "*=", "/=", "%=",
    "::", ":=", "..",
    "<-", "->", "||", "&&",
}
keywords = {
    "fn", "as", "for", "in", "if", "else", "return", "native", "javascript",
    "var", "dyn", "const", "int", "float", "array",
    "plot", "title", "trace", "trace2d", "layout", "simoptions",
}
escape_table = {
    "\\": "\\",
    "n": "\n",
    "t": "\t",
    "0": "\0",
    '"': '"',
}

# Text colors.
COLOR_RED    = "\033[91m"
COLOR_GREEN  = "\033[92m"
COLOR_YELLOW = "\033[93m"
COLOR_BLUE   = "\033[94m"
COLOR_PURPLE = "\033[95m"
COLOR_TEAL   = "\033[96m"
COLOR_END    = "\033[0m"

class StreamPos:
    def __init__(self, line_number=1, column_number=1):
        # Ugh, what a convention. Too bad I can't fix it.
        self.line_number = line_number
        self.column_number = column_number

    def copy(self):
        return StreamPos(self.line_number, self.column_number)

class Token:
    kind_to_color = {
        "str": COLOR_GREEN,
        "int": COLOR_YELLOW,
        "float": COLOR_YELLOW,
        "keyword": COLOR_PURPLE,
        "var": COLOR_BLUE,
        "compvar": COLOR_TEAL,
        "javascript": COLOR_RED,
        #"symbol": ...,
        "end_of_program": COLOR_RED,
    }

    def __init__(self, kind, contents, stream_pos):
        assert isinstance(kind, str)
        assert isinstance(stream_pos, StreamPos)
        self.kind = kind
        self.contents = contents
        self.stream_pos = stream_pos.copy()

    def __eq__(self, template):
        assert isinstance(template, tuple) and len(template) == 2
        return (self.kind, self.contents) == template

    def __ne__(self, template):
        return not (self == template)

    def __repr__(self):
        s = self.contents
        if self.kind == "str" or not isinstance(s, str):
            s = repr(s)
        return s

    def __str__(self):
        s = repr(self)
        if self.kind in self.kind_to_color:
            s = self.kind_to_color[self.kind] + s + COLOR_END
        return s

def is_thing(s, thing):
    try:
        thing(s)
        return True
    except ValueError:
        return False

class SourcePositionError(Exception):
    def __init__(self, message, stream_pos):
        super().__init__(message)
        self.stream_pos = stream_pos.copy()

class LexError(SourcePositionError):
    NAME = "Lexing"

class ParseError(SourcePositionError):
    NAME = "Parse"

class Lexer:
    def __init__(self, text):
        self.text = text + "\xff"
        self.cursor = 0
        self.stream_pos = StreamPos()

    def peek(self):
        return self.text[self.cursor]

    def advance(self, l=1):
        for _ in range(l):
            if self.test("\n"):
                self.stream_pos.line_number += 1
                self.stream_pos.column_number = 1
            else:
                self.stream_pos.column_number += 1
            self.cursor += 1
            assert self.cursor <= len(self.text)

    def test(self, following):
        return self.text[self.cursor : self.cursor+len(following)] == following

    def match(self, *possibilities):
        for possibility in possibilities:
            if self.test(possibility):
                self.advance(len(possibility))
                return True
        return False

    def lex(self):
        tokens = []
        while self.cursor < len(self.text):
            if self.peek() in whitespace:
                self.advance()
            elif self.match("//"):
                while self.cursor < len(self.text) and not self.match("\n"):
                    self.advance()
            elif self.match("/*"):
                counter = 1
                while counter and self.cursor < len(self.text):
                    if self.match("/*"):
                        counter += 1
                    elif self.match("*/"):
                        counter -= 1
                    else:
                        self.advance()
            elif self.peek() in identifier_start_chars:
                identifier_start_index = self.cursor
                self.advance()
                while self.peek() in identifier_chars:
                    self.advance()
                if self.peek() == "$":
                    raise LexError("$ cannot appear in the middle of an identifier", self.stream_pos)
                span = self.text[identifier_start_index:self.cursor]
                kind = "var"
                if span[0] == "$":
                    kind = "compvar"
                elif span in keywords:
                    kind = "keyword"
                # Check if this is the javascript keyword, and if so parse a lexum of ECMAScript.
                if span == "javascript":
                    while self.peek() in whitespace:
                        self.advance()
                    if not self.match("{"):
                        raise LexError("javascript keyword must be followed by {", self.stream_pos)
                    js_start = self.cursor
                    brace_count = 1
                    while brace_count:
                        # BUG: We don't properly check if we're in a string right now.
                        if self.peek() == "{":
                            brace_count += 1
                        elif self.peek() == "}":
                            brace_count -= 1
                        self.advance()
                    js_span = self.text[js_start:self.cursor]
                    tokens.append(Token("javascript", span, self.stream_pos))
                else:
                    tokens.append(Token(kind, span, self.stream_pos))
            elif self.text[self.cursor : self.cursor+2] in length_two_symbols:
                tokens.append(Token("symbol", self.text[self.cursor : self.cursor+2], self.stream_pos))
                self.advance(2)
            elif self.peek() in symbol_characters:
                tokens.append(Token("symbol", self.peek(), self.stream_pos))
                self.advance()
            elif self.peek() in string.digits:
                number_start_index = self.cursor
                self.advance()
                # Accept digits before the decimal point.
                while self.peek() in string.digits:
                    self.advance()
                self.match(".")
                # Accept digits after the decimal point.
                while self.peek() in string.digits:
                    self.advance()
                # Accept scientific notation.
                if self.match("e", "E"):
                    if self.peek() in string.digits + "+-" :
                        self.advance()
                    else:
                        raise LexError("Scientific notation exponent missing after e", self.stream_pos)
                    while self.peek() in string.digits:
                        self.advance()
                span = self.text[number_start_index:self.cursor]
                if all(c in string.digits for c in span):
                    tokens.append(Token("int", int(span), self.stream_pos))
                else:
                    tokens.append(Token("float", float(span), self.stream_pos))
            elif self.match('"'):
                accumulated = []
                while True:
                    if self.cursor == len(self.text):
                        raise LexError("Unterminated string", self.stream_pos)
                    elif self.match('"'):
                        break
                    else:
                        accumulated.append(self.peek())
                        self.advance()
                tokens.append(Token("str", "".join(accumulated), self.stream_pos))
            elif self.match("\xff"):
                break
            else:
                raise LexError("Unexpected character: %r" % self.peek(), self.stream_pos)
        tokens.append(Token("end_of_program", "END", self.stream_pos))
        return tokens

# Fields mean: is_left_associative, precedence
binary_operators = {
    "^":   (False, 9),
    "*":   (True,  7),
    "/":   (True,  7),
    "%":   (True,  7),
    "+":   (True,  6),
    "-":   (True,  6),
    "..":  (False, 5),
    "<=":  (True,  4),
    ">=":  (True,  4),
    "<":   (True,  4),
    ">":   (True,  4),
    "==":  (True,  3),
    "!=":  (True,  3),
    "&&":  (True,  1),
    "||":  (True,  1),
    "=":   (False, 0),
    "+=":  (False, 0),
    "-=":  (False, 0),
    "*=":  (False, 0),
    "/=":  (False, 0),
    "%=":  (False, 0),
    "<-":  (False, 0),
    "~":   (False, 0),
}

unary_operators = {
    "-": 8,
    "+": 8,
    "!": 2,
}

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.cursor = 0
        self.last_seen_line_number = 0

    def peek(self, offset=0):
        return self.tokens[self.cursor + offset]

    def advance(self, amount=1):
        self.cursor += amount

    def get_token(self):
        token = self.peek()
        self.advance()
        return token

    def parse_error(self, message):
        return ParseError(message, self.peek().stream_pos)

    def expect(self, x, message=None):
        if message is None:
            message = "Expected: " + x[1]
        if self.peek() != x:
            raise self.parse_error(message)
        self.advance()
        return True # Analogously to match, we return True.

    def match(self, x):
        if self.peek() == x:
            self.advance()
            return True
        return False

    def parse_repeated_until(self, f, ending):
        values = []
        while self.peek() != ending:
            values.append(f())
        self.advance()
        return values

    def parse_comma_separated_until(self, f, ending):
        values = []
        while self.peek() != ending:
            values.append(f())
            if (not self.match(("symbol", ","))) and self.peek() != ending:
                raise self.parse_error("Comma expected")
        self.advance()
        return values

    def parse_var(self):
        if self.peek().kind != "var":
            raise self.parse_error("Expected identifier")
        return self.get_token().contents

    def parse_compvar(self):
        if self.peek().kind != "compvar":
            raise self.parse_error("Expected $identifier")
        return self.get_token().contents

    def parse_expression_atom(self):
        if self.peek().kind in {"int", "float", "str"}:
            token = self.get_token()
            return "lit", token.kind, token.contents
        if self.peek().kind in {"var", "compvar"}:
            return self.peek().kind, self.get_token().contents
        raise self.parse_error("Expected expression")

    def parse_expression_postfixes(self, e):
        while True:
            if self.match(("symbol", "(")):
                e = "call", e, self.parse_comma_separated_until(self.parse_expr, ("symbol", ")"))
            elif self.match(("symbol", "[")):
                index = self.parse_expr()
                self.expect(("symbol", "]"))
                e = "binary-op", "[]", e, index
            elif self.match(("symbol", ".")):
                e = "dot", e, self.parse_var()
            else:
                break
        return e

    def parse_expr(self, min_precedence=0):
        def is_operator(token, table):
            return (token.kind == "symbol" or token.kind == "keyword") and token.contents in table

        if self.match(("symbol", "(")):
            e = self.parse_expr()
            self.expect(("symbol", ")"))
            e = self.parse_expression_postfixes(e)
        elif is_operator(self.peek(), unary_operators):
            operator = self.get_token()
            e = self.parse_expr(unary_operators[operator.contents])
            e = "unary-op", operator.contents, e
        else:
            e = self.parse_expression_postfixes(self.parse_expression_atom())

        while True:
            if not is_operator(self.peek(), binary_operators):
                break
            operator = self.peek()
            op_left_assoc, op_precedence = binary_operators[operator.contents]
            if op_precedence < min_precedence:
                break
            self.advance()
            rhs = self.parse_expr(op_precedence + op_left_assoc)
            e = "binary-op", operator.contents, e, rhs

        return e

    def parse_fn_arg(self):
        name = self.parse_compvar()
        ty = self.parse_type() if self.match(("symbol", ":")) else None
        return name, ty

    def parse_type(self):
        if self.match(("symbol", "(")):
            # TODO: Maybe (T,) should be the length-1 tuple, and (T) should just be T?
            return "tuple", self.parse_comma_separated_until(self.parse_type, ("symbol", ")"))
        if self.match(("symbol", "[")):
            ty = self.parse_type()
            self.expect(("symbol", "]"))
            return "list", ty
        for type_keyword in ("var", "dyn", "const", "int", "float"):
            if self.match(("keyword", type_keyword)):
                return type_keyword
        raise self.parse_error("Expected type")

    def parse_block(self):
        self.expect(("symbol", "{"))
        return self.parse_repeated_until(self.parse_statement, ("symbol", "}"))

    def parse_jsonlike_number(self):
        multiplier = +1
        for symbol, sign in {"+": +1, "-": -1}.items():
            if self.match(("symbol", symbol)):
                multiplier = sign
                break
        token = self.get_token()
        if token.kind not in ("int", "float"):
            raise self.parse_error("Invalid json-like data")
        return multiplier * token.contents

    def parse_jsonlike_dict_entry(self):
        if self.peek().kind == "str":
            key = self.get_token().contents
        else:
            key = self.parse_var()
        self.expect(("symbol", ":"))
        value = self.parse_jsonlike_data()
        return key, value

    def parse_jsonlike_data(self):
        if self.match(("symbol", "[")):
            return self.parse_comma_separated_until(self.parse_jsonlike_data, ("symbol", "]"))
        if self.match(("symbol", "{")):
            return dict(self.parse_comma_separated_until(self.parse_jsonlike_dict_entry, ("symbol", "}")))
        if self.peek().kind == "str":
            return self.get_token().contents
        if self.peek().kind == "var" and self.peek().contents in ("true", "false", "null"):
            return {"true": True, "false": False, "null": None}[self.get_token().contents]
        return self.parse_jsonlike_number()

    def parse_statement(self):
        if self.peek().stream_pos.line_number != self.last_seen_line_number:
            ln = self.peek().stream_pos.line_number
            self.last_seen_line_number = ln
            return "line", ln
        if self.match(("keyword", "plot")):
            # Check for complex plots.
            if self.peek() == ("symbol", "{"):
                body = self.parse_block()
                return "complex-plot", body
            return "simple-plot", self.parse_comma_separated_until(self.parse_expr, ("symbol", ";"))
        if self.match(("keyword", "trace")):
            return "trace", self.parse_expr(), self.parse_jsonlike_data()
        if self.match(("keyword", "trace2d")):
            return "trace2d", self.parse_expr(), self.parse_expr(), self.parse_jsonlike_data()
        if self.match(("keyword", "layout")):
            return "layout", self.parse_jsonlike_data()
        if self.match(("keyword", "title")):
            expr = self.parse_expr()
            self.expect(("symbol", ";"))
            return "title", expr
        if self.match(("keyword", "simoptions")):
            return "simoptions", self.parse_jsonlike_data()
        if self.match(("keyword", "fn")):
            function_name = self.parse_var()
            self.expect(("symbol", "("))
            fn_args = self.parse_comma_separated_until(self.parse_fn_arg, ("symbol", ")"))
            return_type = self.parse_type() if self.match(("symbol", "->")) else None
            body = self.parse_block()
            return "fn", {
                "name": function_name,
                "args": fn_args,
                "return_type": return_type,
                "body": body,
            }
        if self.peek().kind == "compvar" and self.peek(+1) in [("symbol", ":"), ("symbol", ":=")]:
            var_name = self.parse_compvar()
            type_ascription = None
            if self.match(("symbol", ":")):
                type_ascription = self.parse_type()
                self.expect(("symbol", "="))
            else:
                self.expect(("symbol", ":="))
            initializer     = self.parse_expr()
            self.expect(("symbol", ";"))
            return "let", {
                "name": var_name,
                "type": type_ascription,
                "initializer": initializer,
            }
        if self.match(("keyword", "if")):
            elif_clauses = []
            else_clause = None
            def get_clause():
                cond = self.parse_expr()
                body = self.parse_block()
                elif_clauses.append((cond, body))
            get_clause()
            # Parse all elif clauses that follow.
            while self.peek() == ("keyword", "else") and self.peek(+1) == ("keyword", "if"):
                self.advance(2)
                get_clause()
            if self.match(("keyword", "else")):
                else_clause = self.parse_block()
            # TODO: Handle else and elif here.
            return "if", {
                "elifs": elif_clauses,
                "else": else_clause,
            }
        if self.match(("keyword", "return")):
            if self.match(("symbol", ";")):
                return "return", None
            return_value = self.parse_expr()
            self.expect(("symbol", ";"))
            return "return", return_value
        if self.match(("keyword", "for")):
            var_name = self.parse_compvar()
            self.expect(("keyword", "in"))
            iterator = self.parse_expr()
            body = self.parse_block()
            return "for", {
                "name": var_name,
                "iterator": iterator,
                "body": body,
            }
        if self.match(("keyword", "array")):
            declaration = self.parse_expr()
            print(declaration)
            if declaration[:2] != ("binary-op", "[]"):
                raise self.parse_error("array declarations must be like: array var[num];")
            self.expect(("symbol", ";"))
            _, _, var_expr, count_expr = declaration
            return "array", var_expr, count_expr
        if self.peek().kind == "javascript":
            return "javascript", self.get_token()

        e = self.parse_expr()
        self.expect(("symbol", ";"))
        return "expr", e

    def parse_program(self):
        return self.parse_repeated_until(self.parse_statement, ("end_of_program", "END"))

def parse(program_text):
    tokens = Lexer(program_text).lex()
    return Parser(tokens).parse_program()

if __name__ == "__main__":
    src = """
freq ~ Slider(1, 2);
x ~ Uniform(-1, 1);
x'' <- -freq * x;

plot x;
simtime 10;
"""
    tokens = Lexer(src).lex()
    print(" ".join(map(str, tokens)))
    #print(" ".join([str(tok.stream_pos.line_number) for tok in tokens]))
    p = Parser(tokens)
    prog = p.parse_program()
    print(prog)
