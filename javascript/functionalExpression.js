"use strict";

const cnst = value => () => value;
const variable = function (a) {
    const argName = "xyz".indexOf(a);
    return function () {
        return arguments[argName];
    }
};
const binary = fun => (a, b) => (x, y, z) => fun(a(x, y, z), b(x, y, z));
const unary = fun => a => (x, y, z) => fun(a(x, y, z));
const cube = unary(a => a * a * a);
const negate = unary(a => -a);
const cuberoot = unary(a => Math.cbrt(a));
const parSe = input => (input.trim() === "x" || input.trim() === "y" || input.trim() === "z") ? variable(input.trim()) : cnst(+input);
const add = binary((a, b) => a + b);
const subtract = binary((a, b) => a - b);
const multiply = binary((a, b) => a * b);
const divide = binary((a, b) => a / b);
const operations = {
    "+": add,
    "-": subtract,
    "/": divide,
    "*": multiply,
};
const parse = function (s) {
    let expression = [];
    let a = s.split(" ").filter(x => x !== '');
    for (let i = 0; i < a.length; i++) {
        if (a[i] in operations) {
            let first = expression.pop();
            let second = expression.pop();
            expression.push(operations[a[i]](second, first));
        } else
            expression.push(parSe(a[i]));
    }
    return function (x) {
        return expression[0](x);
    }
};