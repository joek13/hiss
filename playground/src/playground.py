import os

def handle(ev, ctx):
    output = os.system("./hissc")

    return {
        "statusCode": 200,
        "message": str(output)
    }