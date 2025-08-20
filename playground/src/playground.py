import base64
import json
import os
import subprocess
import tempfile

def read_body(ev):
    body = base64.b64decode(ev["body"]) if ev["isBase64Encoded"] else ev["body"]
    return json.loads(body)

def write_source(tmp_dir, source):
    source_path = os.path.join(tmp_dir, "program.hiss")

    with open(source_path, "w") as f:
        f.write(source)

    return source_path

def ok(message):
    return {
        "statusCode": 200,
        "body": message,
        "isBase64Encoded": False
    }

def error(result, message, code=400):
    result["message"] = message

    return {
        "statusCode": code,
        "body": result,
        "isBase64Encoded": False
    }

def run_cmd(cmd):
    prelude = f"Running command {' '.join(cmd)}\n"
    result = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=10)
    return (result.returncode, prelude + result.stdout.decode("utf-8"))

def do_assemble(tmp_dir):
    cmd = ["./hissc", os.path.join(tmp_dir, "program.hiss"), "-o", os.path.join(tmp_dir, "program.hissa"), "--asm"]
    return run_cmd(cmd)

def do_compile(tmp_dir):
    cmd = ["./hissc", os.path.join(tmp_dir, "program.hiss"), "-o", os.path.join(tmp_dir, "program.hissc")]
    return run_cmd(cmd)

def do_interp(tmp_dir):
    cmd = ["./hissvm", os.path.join(tmp_dir, "program.hissc")]
    return run_cmd(cmd)

def handle(ev, ctx):
    request = read_body(ev)
    source = request["source"]

    with tempfile.TemporaryDirectory() as tmp_dir:
        result = {"stdout": ""}

        try:
            write_source(tmp_dir, source)

            assemble_rc, assemble_stdout = do_assemble(tmp_dir)
            result["stdout"] = assemble_stdout
            if assemble_rc != 0:
                return error(result, f"hissc exited with return code {assemble_rc}")

            with open(os.path.join(tmp_dir, "program.hissa"), "r") as assembly_file:
                result["assembly"] = assembly_file.read()

            compile_rc, compile_stdout = do_compile(tmp_dir)
            result["stdout"] += compile_stdout 
            if compile_rc != 0:
                return error(result, f"hissc exited with return code {compile_rc}")

            interp_rc, interp_stdout = do_interp(tmp_dir)
            result["stdout"] += interp_stdout
            if interp_rc != 0:
                return error(result, f"hissvm exited with return code {interp_rc}")

            return ok(result)
        except Exception as e:
            err = f"encountered unexpected error: {e}"
            result["stdout"] += err
            return error(result, err, code=500)