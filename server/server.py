"""
libwebode exploration server
"""

import json
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
        try:
            ast = dsl_parser.parse(payload["code"])
            ctx = dsl.Context()
            ctx.execute(None, ast)
            js = ctx.codegen_js()
        except dsl_parser.SourcePositionError as error:
            print("%s Error: %s" % (error.NAME, error.args[0]))
            self.write(json.dumps({
                "error": True,
                "line_number": error.stream_pos.line_number,
                "column_number": error.stream_pos.column_number,
                "message": "%s Error: %s" % (error.NAME, error.args[0]),
            }))
            return
        except Exception as error:
            print("Unhandled error:", error)
            self.write(json.dumps({
                "error": True,
                "line_number": -1,
                "column_number": -1,
                "message": "Compiler bug: %s" % (error,),
            }))
            raise
            #return
        print(js)
        self.write(json.dumps({
            "error": False,
            "js": js,
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
