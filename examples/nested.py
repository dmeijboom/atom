result = ""
depth = 0
totalIterations = 0

for x in range(1, 6):
    result = result + "Layer" + str(x) + ": "
    depth = depth + 1

    for y in range(1, 4):
        if x % 2 == 0:
            if y % 2 == 1:
                for z in range(1, 3):
                    totalIterations = totalIterations + 1

                    if z == 1:
                        if x > 2:
                            if y == 1:
                                result = result + "[" + str(x) + "," + str(y) + "," + str(z) + "]"

                                for a in range(0, 2):
                                    if a == 0:
                                        if x + y + z > 5:
                                            if totalIterations % 3 == 0:
                                                result = result + "★"

                                                for b in range(0, 2):
                                                    if b == 1:
                                                        if x * y > 4:
                                                            result = result + "♦"
                                                        else:
                                                            if z % 2 == 1:
                                                                result = result + "♠"
                                                            else:
                                                                result = result + "♥"
                                            else:
                                                if totalIterations % 2 == 0:
                                                    result = result + "▲"
                                                else:
                                                    result = result + "▼"
                                        else:
                                            result = result + "◆"
                                    else:
                                        result = result + "○"
                            else:
                                if y == 3:
                                    result = result + "{" + str(x) + "," + str(y) + "}"
                                else:
                                    result = result + "(" + str(x) + "," + str(y) + ")"
                        else:
                            result = result + "~" + str(x) + "~"
                    else:
                        if x == 4:
                            if y + z > 3:
                                result = result + "MAX"
                            else:
                                result = result + "mid"
                        else:
                            result = result + "min"
            else:
                if y == 2:
                    result = result + "EVEN-EVEN"
                else:
                    result = result + "SKIP"
        else:
            if y == 1:
                for w in range(0, 2):
                    totalIterations = totalIterations + 1

                    if w == 0:
                        if x == 1:
                            result = result + "FIRST"
                        else:
                            if x == 3:
                                result = result + "THIRD"
                            else:
                                result = result + "FIFTH"
                    else:
                        result = result + "-" + str(x) + "w" + str(w)
            else:
                if y == 2:
                    if x > 3:
                        result = result + "BIG"
                    else:
                        result = result + "small"
                else:
                    result = result + "end"

        result = result + " | "

    result = result + ", "
    depth = depth - 1

print([result, depth, totalIterations])
