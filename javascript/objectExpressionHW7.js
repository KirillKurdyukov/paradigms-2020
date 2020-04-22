"use strict";

const Expression = function(type, fun, ...args) {
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
    return "(" +  [this.type, ...this.args.map(i=>i.prefix())].join(" ") +  ")";
};

const makeOf = function (func, type) {
    const res = function (...val) {
        Expression.call(this, type, func, ...val)
    }
    res.prototype = Object.create(Expression.prototype);
    return res;
};

const Const = function(x) {
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

const Variable = function(name) {
    this.name = name;
    this.toString = () => this.name;
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