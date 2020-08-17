"""
libwebode exploration server
"""

import os
import json
import string
import traceback
import tornado
import tornado.ioloop
import tornado.web
import tornado.websocket
import dsl_parser
import dsl

last_compilation = None

# We have to be at least a tiny bit careful here about security.
def get_fs_path(url_param):
    requested_path = url_param if url_param is not None else "system.webode"
    allowed_characters = set(string.printable) - set("\r\n\t\x0b\x0c")
    requested_path = "".join(c for c in requested_path if c in allowed_characters)
    # We now try to make sure that the path cannot go outside of files/
    while ".." in requested_path:
        requested_path = requested_path.replace("..", "")
    sandbox = "files/"
    path = os.path.join(sandbox, requested_path)
    print("Requested path", repr(url_param), "mapped to", repr(path))
    return path

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
        global last_compilation
        payload = json.loads(self.request.body)
        print("Compilation parameters:", payload["compilation_parameters"])
        ctx = dsl.Context(payload["compilation_parameters"])
        try:
            ast = dsl_parser.parse(payload["code"])
            ctx.execute(None, ast)
            js = ctx.codegen()
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
            error_message = traceback.format_exc()
            print(error_message)
            self.write(json.dumps({
                "error": True,
                "line_number": -1,
                "column_number": -1,
                "message": "Compiler bug:\n%s" % (error_message,),
                "print_output": "\n".join(ctx.print_output),
            }))
            return
        #print(js)
        last_compilation = ctx
        self.write(json.dumps({
            "error": False,
            "js": js,
            "print_output": "\n".join(ctx.print_output),
        }))

class RemoteComputationHandler(tornado.websocket.WebSocketHandler):
    def check_origin(self, origin):
        return True

    def open(self):
        print("Opened WebSocket connection:", self)

    def on_message(self, message):
        print("============= Got request:", message)
        request = json.loads(message)
        if last_compilation is None:
            self.write_message(json.dumps({"errorMessage": "BUG BUG BUG: No compilation whatosever!"}))
            return
        elif request["compilationId"] != last_compilation.compilation_id:
            self.write_message(json.dumps({
                "requestId": request["requestId"],
                "errorMessage": "Got request for compilation id %i, when %i is current. Try recompiling?" % (
                    request["compilationId"], last_compilation.compilation_id,
                ),
            }))
            return

        ctx = last_compilation
        try:
            results = ctx.native_backend_fulfill_request(request)
        except dsl_parser.SourcePositionError as error:
            self.write_message(json.dumps({
                "requestId": request["requestId"],
                "errorMessage": str(error.args[0]),
            }))
            return
        except Exception as error:
            print("Unhandled error:", error)
            error_message = traceback.format_exc()
            print(error_message)
            self.write_message(json.dumps({
                "requestId": request["requestId"],
                "errorMessage": "Backend bug:\n%s" % (error_message,),
            }))
            return
        results["requestId"] = request["requestId"]
        results["errorMessage"] = None
        response = json.dumps(results)
        print("Generated", len(response), "bytes of response")
        self.write_message(response)

    def on_close(self):
        print("Closed WebSocket connection:", self)

class SaveHandler(AllowCORS, tornado.web.RequestHandler):
    def post(self):
        fs_path = get_fs_path(self.get_argument("path", None))
        payload = json.loads(self.request.body)
        code = payload["code"]
        try:
            with open(fs_path, "w") as f:
                f.write(code)
        except FileNotFoundError:
            self.write(json.dumps({"error": True}))
        else:
            self.write(json.dumps({"error": False}))

class ReloadHandler(AllowCORS, tornado.web.RequestHandler):
    def post(self):
        fs_path = get_fs_path(self.get_argument("path", None))
        try:
            with open(fs_path, "r") as f:
                code = f.read()
        except FileNotFoundError:
            self.write(json.dumps({"error": True}))
        else:
            self.write(json.dumps({"error": False, "code": code}))

def make_app():
    return tornado.web.Application([
        ("/compile", CompileHandler),
        ("/computation", RemoteComputationHandler),
        ("/save", SaveHandler),
        ("/reload", ReloadHandler),
    ])

if __name__ == "__main__":
    app = make_app()
    app.listen(50505)
    print("Launching server.")
    tornado.ioloop.IOLoop.current().start()
