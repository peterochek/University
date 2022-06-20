'use strict'

const letters = new Map([
    ['x', 0],
    ['y', 1],
    ['z', 2],
])

const fold =
    (func) =>
        (...inner) =>
            (...outer) =>
                func(...inner.map((operation) => operation(...outer)))

const cnst = (x) => () => x
const variable =
    (letter) =>
        (...args) =>
            args[letters.get(letter)]
const add = fold((...args) => args.reduce((_, now) => _ + now, 0))
const subtract = fold((x, y) => x - y)
const multiply = fold((...args) => args.reduce((_, now) => _ * now, 1))
const divide = fold((x, y) => x / y)
const negate = fold((x) => -x)
const abs = fold((x) => Math.abs(x))
const iff = fold((x, y, z) => (x >= 0 ? y : z))

const pi = cnst(Math.PI)
const e = cnst(Math.E)

const consts = new Map([
    ['pi', pi],
    ['e', e],
])

const operators = new Map([
    ['+', [add, 2]],
    ['-', [subtract, 2]],
    ['*', [multiply, 2]],
    ['/', [divide, 2]],
    ['negate', [negate, 1]],
    ['abs', [abs, 1]],
    ['iff', [iff, 3]],
])

let line = []

const process = (token) => {
    if (letters.has(token)) {
        return variable(token)
    } else if (operators.has(token)) {
        const [operator, num] = operators.get(token)
        return operator(...line.splice(-num))
    } else if (consts.has(token)) {
        return consts.get(token)
    } else {
        return cnst(Number(token))
    }
}

function parse(string) {
    string
        .split(' ')
        .filter((x) => x.length > 0)
        .forEach((token) => line.push(process(token)))
    return line.pop()
}

let expression = add(
    subtract(
        multiply(variable('x'), variable('x')),
        multiply(cnst(2), variable('x'))
    ),
    cnst(1)
)

const N = 10

Array.from(Array(N), (_, index) => index + 1).forEach((i) =>
    console.log(expression(i, 0, 0))
)
