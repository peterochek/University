"use strict";

const letters = new Map([
    ["x", 0],
    ["y", 1],
    ["z", 2],
]);

class Const {
    constructor(value) {
        this.value = value;
    }

    evaluate() {
        return Number(this.value);
    }

    toString() {
        return String(this.value);
    }

    diff() {
        return new Const(0);
    }
}

const E = new Const(Math.E);

class Variable {
    constructor(name) {
        this.name = name;
    }

    evaluate(...args) {
        // :NOTE: Эффективность
        return args[letters.get(this.name)];
    }

    toString() {
        return this.name;
    }

    diff(letter) {
        // :NOTE: new Const(1)
        return this.name === letter ? new Const(1) : new Const(0);
    }
}

function factory(func, string, diff) {
    return class {
        static arity = func.length;
        constructor(...args) {
            this.values = args;
        }

        // :NOTE: -> Прототип
        evaluate(...args) {
            return func(...this.values.map((x) => x.evaluate(...args)));
        }

        toString() {
            return (
                this.values.map((x) => x.toString()).join(" ") + " " + string
            );
        }

        diff(letter) {
            return diff.call(
                this,
                this.values.map((x) => x.diff(letter))
            );
        }
    };
}

const Add = factory(
    (x, y) => x + y,
    "+",
    function (diff) {
        return new Add(...diff);
    }
);

const Subtract = factory(
    (x, y) => x - y,
    "-",
    function (diff) {
        return new Subtract(...diff);
    }
);

const Multiply = factory(
    (x, y) => x * y,
    "*",
    // (x, y, dx, dy) =>
    function (diff) {
        return new Add(
            new Multiply(diff[0], this.values[1]),
            new Multiply(this.values[0], diff[1])
        );
    }
);

const Divide = factory(
    (x, y) => x / y,
    "/",
    function (diff) {
        return new Divide(
            new Subtract(
                new Multiply(diff[0], this.values[1]),
                new Multiply(this.values[0], diff[1])
            ),
            new Multiply(this.values[1], this.values[1])
        );
    }
);

const Negate = factory(
    (x) => -x,
    "negate",
    function (diff) {
        return new Negate(diff[0]);
    }
);

const Log = factory(
    (x, y) => Math.log(Math.abs(y)) / Math.log(Math.abs(x)),
    "log",
    function (diff) {
        return new Divide(
            new Subtract(
                new Divide(
                    new Multiply(new Log(E, this.values[0]), diff[1]),
                    this.values[1]
                ),
                new Divide(
                    new Multiply(new Log(E, this.values[1]), diff[0]),
                    this.values[0]
                )
            ),
            new Multiply(new Log(E, this.values[0]), new Log(E, this.values[0]))
        );
    }
);

const Pow = factory(
    (x, y) => Math.pow(x, y),
    "pow",
    function (diff) {
        return new Multiply(
            new Pow(this.values[0], new Subtract(this.values[1], new Const(1))),
            new Add(
                new Multiply(this.values[1], diff[0]),
                new Multiply(
                    new Multiply(this.values[0], new Log(E, this.values[0])),
                    diff[1]
                )
            )
        );
    }
);

const operators = new Map([
    ["+", Add],
    ["-", Subtract],
    ["*", Multiply],
    ["/", Divide],
    ["negate", Negate],
    ["pow", Pow],
    ["log", Log],
]);

function parse(string) {
    let line = [];
    string
        .split(" ")
        .filter((x) => x.length > 0)
        .forEach((token) =>
            line.push(
                ((token) => {
                    if (letters.has(token)) {
                        return new Variable(token);
                    } else if (operators.has(token)) {
                        const operator = operators.get(token);
                        return new operator(...line.splice(-operator.arity));
                    } else {
                        return new Const(parseInt(token));
                    }
                })(token)
            )
        );

    return line.pop();
}
