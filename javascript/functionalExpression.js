"use strict";

const cnst = value => () => value;
const variable = function (a) {
    const argName = "xyz".indexOf(a);
    return function () {
        return arguments[argName];
    }
};
const binary = fun => (a, b) => (...arg) => fun(a(...arg), b(...arg));
const unary = fun => a => (...arg) => fun(a(...arg));
const cube = unary(a => a * a * a);
const negate = unary(a => -a);
const cuberoot = unary(a => Math.cbrt(a));
const parSe = input => (input.trim() === "x" || input.trim() === "y" || input.trim() === "z") ? variable(input.trim()) : cnst(+input);
const add = binary((a, b) => a + b);
const subtract = binary((a, b) => a - b);
const multiply = binary((a, b) => a * b);
const divide = binary((a, b) => a / b);
const operationsBinary = {
    "+": add,
    "-": subtract,
    "/": divide,
    "*": multiply,
};
const operationsUnary = {
    "negate": negate,
    "cube" : cube,
    "cuberoot" : cuberoot,
};
const parse = function (s) {
    let expression = [];
    let a = s.split(" ").filter(x => x !== '');
    for (let i = 0; i < a.length; i++) {
        if (a[i] in operationsBinary) {
            let first = expression.pop();
            let second = expression.pop();
            expression.push(operationsBinary[a[i]](second, first));
        } else if (a[i] in operationsUnary) {
            let first = expression.pop();
            expression.push(operationsUnary[a[i]](first));
        } else
            expression.push(parSe(a[i]));
    }
    return function (...arg) {
        return expression[0](...arg);
    }
};