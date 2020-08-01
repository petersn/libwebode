"""
libwebode exploration server
"""

import json
import tornado
import tornado.ioloop
import tornado.web
import dsl_parser

class AllowCORS:
    def set_default_headers(self):
        self.set_header("Access-Control-Allow-Origin", "*")
        self.set_header("Access-Control-Allow-Headers", "x-requested-with, content-type")
        self.set_header("Access-Control-Allow-Methods", "POST, GET, OPTIONS")

    def options(self):
        self.set_status(204)
        self.finish()

class CompileHandler(AllowCORS, tornado.web.RequestHandler):
    def post(self):
        payload = json.loads(self.request.body)
        try:
            tokens = dsl_parser.lex(payload["code"])
        except dsl_parser.LexError as lex_error:
            self.write(json.dumps({
                "error": True,
                "line_number": lex_error.line_number,
                "column_number": lex_error.column_number,
                "message": lex_error.args[0],
            }))
            return
        print(tokens)
        self.write(json.dumps({
            "error": False,
        }))

def make_app():
    return tornado.web.Application([
        ("/compile", CompileHandler),
    ])

if __name__ == "__main__":
    app = make_app()
    app.listen(50505)
    print("Launching server.")
    tornado.ioloop.IOLoop.current().start()
