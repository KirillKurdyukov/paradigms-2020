"use strict";

const Expression = function (type, fun, ...args) {
    this.type = type;
    this.fun = fun;
    this.args = args;
};

Expression.prototype.evaluate = function (x, y, z) {
    return this.fun.apply(this, this.args.map(i => i.evaluate(x, y, z)));
};

Expression.prototype.toString = function () {
    return this.args.join(" ") + " " + this.type;
};

Expression.prototype.prefix = function () {
    return "(" + [this.type, ...this.args.map(i => i.prefix())].join(" ") + ")";
};

const makeOf = function (func, type) {
    const res = function (...val) {
        Expression.call(this, type, func, ...val)
    };
    res.prototype = Object.create(Expression.prototype);
    return res;
};

const Const = function (x) {
    this.x = x;
};

Const.prototype.evaluate = function () {
    return this.x;
};

Const.prototype.toString = function () {
    return this.x.toString();
};

Const.prototype.prefix = function () {
    return this.x.toString();
};

const Negate = makeOf((a => -a), "negate");

const ArcTan = makeOf((a => Math.atan(a)), "atan");

const Exp = makeOf((a => Math.exp(a)), "exp");

const Variable = function (name) {
    this.name = name;
};

Variable.prototype.evaluate = function () {
    return arguments["xyz".indexOf(this.name)]
};

Variable.prototype.toString = function () {
    return this.name;
};

Variable.prototype.prefix = function () {
    return this.name;
};

const Add = makeOf(((a, b) => a + b), "+");

const Subtract = makeOf(((a, b) => a - b), "-");

const Min3 = makeOf(((a, b, c) => Math.min(a, Math.min(b, c))), "min3");

const Max5 = makeOf(((a, b, c, d, e) => Math.max(a, Math.max(b, Math.max(c, Math.max(d, e))))), "max5");

const Divide = makeOf(((a, b) => a / b), "/");

const Multiply = makeOf(((a, b) => a * b), "*");

const parsePrefix = function (string) {
    const len = string.length;
    let pos = 0;
    let balance = 0;
    let isLastArgSimpleOne = false;
    let openBracket;
    const skipWhitespaces = function () {
        while (pos < len && string[pos] === ' ') {
            ++pos;
            isLastArgSimpleOne = false;
        }
    };
    const errorCheck = function (exp, error, poss) {
        if (!exp) {
            throw new Error(error + "\n" + string + "\n" + getError(poss));
        }
    };
    const isDigit = function (pos) {
        return string[pos] >= '0' && string[pos] <= '9';
    };

    const isVariable = function (pos) {
        return string[pos] === 'x' || string[pos] === 'y' || string[pos] === 'z';
    };

    const getError = function (pos) {
        let string = "";
        for (let i = 0; i < pos; i++) {
            string += " ";
        }
        string += "^";
        return string;
    };

    const operations = {
        "+": [Add, 2],
        "-": [Subtract, 2],
        "/": [Divide, 2],
        "*": [Multiply, 2],
        "exp": [Exp, 1],
        "atan": [ArcTan, 1],
        "negate": [Negate, 1],
    };

    const getOperation = function () {
        for (let name in operations) {
            if (name.length + pos < len && name === string.substr(pos, name.length)) {
                pos += name.length;
                return operations[name];
            }
        }
        return undefined;
    };

    const parsing = function () {
        errorCheck(pos < len, "End of string is not expected", 0);
        if (isDigit(pos) || (string[pos] === '-' && pos + 1 < len && isDigit(pos + 1))) {
            errorCheck(isLastArgSimpleOne === false, "Expected whitespace between arguments on pos " + pos, pos);
            isLastArgSimpleOne = true;
            return new Const(getNumber());
        }
        if (isVariable(pos)) {
            errorCheck(isLastArgSimpleOne === false, "Expected whitespace between arguments on pos " + pos, pos);
            isLastArgSimpleOne = true;
            return new Variable(string[pos++]);
        }
        if (string[pos] === '(') {
            openBracket = pos;
            pos++;
            balance++;
            skipWhitespaces();
            isLastArgSimpleOne = false;
            let args = [];
            let fun = undefined;
            let haveOperation = false;
            while (string[pos] !== ')' && pos < len) {
                let ArgOrFun = parsing();
                if (ArgOrFun === undefined && haveOperation) {
                    throw new Error("Arguments are after operation on pos " + pos)
                }
                if (ArgOrFun === undefined) {
                    haveOperation = true;
                    fun = getOperation();
                    errorCheck(fun !== undefined, "Expected operation on pos " + pos, pos);
                } else {
                    args.push(ArgOrFun);
                }
                skipWhitespaces();
            }
            errorCheck(fun !== undefined && fun !== null, "Expected operation before arguments on pos: " + (openBracket + 1), openBracket + 1);

            errorCheck(string[pos] === ')', "Expected close bracket on pos " + pos, pos);

            errorCheck(args.length === fun[1] , "Wrong number of arguments in operation on pos " + pos, pos);
            isLastArgSimpleOne = false;
            ++pos;
            --balance;
            errorCheck(balance >= 0, "Bracket's balance is less than 0 on pos " + pos, pos);
            return new fun[0](...args);
        }
        return undefined;
    };

    const getNumber = function () {
        let start = pos;
        if (string[pos] === '-') {
            pos++;
        }
        while (isDigit(pos)) {
            ++pos;
        }
        return parseInt(string.substring(start, pos));
    };
    skipWhitespaces();
    const result = parsing();
    skipWhitespaces();
    errorCheck(balance === 0, "Bracket's balance isn't 0 at the end", pos);
    errorCheck(len === pos, "Expression didn't parse completely", len);
    return result;
};
