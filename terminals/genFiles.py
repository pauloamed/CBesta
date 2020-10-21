import sys

tokens = []
input_str = sys.stdin.read()
for line in input_str.split('\n'):
    line = line.strip()
    if line == "": continue

    lineTokens = line.split(' ')
    hasArg = (len(lineTokens) == 4)
    tokens.append((lineTokens[0], hasArg))

annot = "{}Token :: Parsec [Token] st Token"
header = "{}Token = tokenPrim show update_pos get_token where"
firstLine = ["get_token ({} p)  = Just ({} p) ", "get_token ({} p x) = Just ({} p x)"]
secondLine = "get_token _  = Nothing"

for token, hasArg in tokens:
    funcName = token[0].lower() + token[1:]
    print(annot.format(funcName))
    print(header.format(funcName))
    print("  " + firstLine[hasArg].format(token, token))
    print("  " + secondLine)
    print("\n")
