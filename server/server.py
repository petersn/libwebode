"""
libwebode exploration server
"""

import json
import traceback
import tornado
import tornado.ioloop
import tornado.web
import dsl_parser
import dsl

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
        ctx = dsl.Context()
        try:
            ast = dsl_parser.parse(payload["code"])
            ctx.execute(None, ast)
            js = ctx.codegen_js()
        except dsl_parser.SourcePositionError as error:
            print("%s Error: %s" % (error.NAME, error.args[0]))
            self.write(json.dumps({
                "error": True,
                "line_number": error.stream_pos.line_number,
                "column_number": error.stream_pos.column_number,
                "message": "%s Error: %s" % (error.NAME, error.args[0]),
                "print_output": "\n".join(ctx.print_output),
            }))
            return
        except Exception as error:
            print("Unhandled error:", error)
            traceback.print_exc()
            self.write(json.dumps({
                "error": True,
                "line_number": -1,
                "column_number": -1,
                "message": "Compiler bug: %s" % (error,),
                "print_output": "\n".join(ctx.print_output),
            }))
            return
        print(js)
        self.write(json.dumps({
            "error": False,
            "js": js,
            "print_output": "\n".join(ctx.print_output),
        }))

class SaveHandler(AllowCORS, tornado.web.RequestHandler):
    def post(self):
        payload = json.loads(self.request.body)
        code = payload["code"]
        with open("system.webode", "w") as f:
            f.write(code)
        self.write(json.dumps({"error": False}))

class ReloadHandler(AllowCORS, tornado.web.RequestHandler):
    def post(self):
        try:
            with open("system.webode", "r") as f:
                code = f.read()
        except FileNotFoundError:
            self.write(json.dumps({"error": True}))
        else:
            self.write(json.dumps({"error": False, "code": code}))

def make_app():
    return tornado.web.Application([
        ("/compile", CompileHandler),
        ("/save", SaveHandler),
        ("/reload", ReloadHandler),
    ])

if __name__ == "__main__":
    app = make_app()
    app.listen(50505)
    print("Launching server.")
    tornado.ioloop.IOLoop.current().start()
