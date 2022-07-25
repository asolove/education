let source = `
(begin
    (define (fib n)
        (define (fib-iter a b k)
            (if (= k n)
                b
                (fib-iter b (+ a b) (+ k 1))))
        (fib-iter 0 1 1))
    (fib 10))
`

let parseTree = [
    "begin",
    ["define", ["fib", "n"], 
        ["define", ["fib-iter", "a", "b", "k"],
            ["if", ["=", "k", "n"],
                    "b",
                    ["fib-iter", "b", ["+", "a", "b"], ["+", "k", 1]]]],
        ["fib-iter", 0, 1, 1]],
    ["fib", 10]
];

let evaluate = (expr, env) => {
    console.log(`Evaluating ${JSON.stringify(expr)}`);
    switch(exprType(expr)) {
        case "boolean": return expr === "true";
        case "number": return expr;
        case "identifier": return lookup(expr, env);
        case "if": return evalIf(expr, env);
        case "begin": return evalBegin(expr, env);
        case "define": return evalDefine(expr, env);
        case "apply": return evalApply(expr, env);
        case "native-op": return evalNativeOp(expr, env);
    }
}

let nativeOps = {
    "+": function(a, b) { return a + b; },
    "=": function(a, b) { return a === b }
}


let exprType = (expr) => {
    if (typeof(expr) === "number") return "number";
    if (expr === "true" || expr === "false") return "boolean"
    if (typeof(expr) === "string") return "identifier";

    if (!(expr instanceof Array)) {
        console.log(JSON.stringify(expr));
        debugger;
        throw new Error("invalid expression type.")
    }
    
    switch (expr[0]) {
        case "if": return "if";
        case "begin": return "begin";
        case "define": return "define";
    }

    if (Object.keys(nativeOps).includes(expr[0])) {
        return "native-op";
    }
    return "apply";
}

// Expression evaluation
let evalIf = (ifExpr, env) => {
    let condition = evaluate(ifExpr[1], env);
    return evaluate(ifExpr[condition ? 2 : 3], env);
}

let evalBegin = (beginExpr, env) => {
    let exprs = (beginExpr).slice(1);
    let val = false;
    for(let child of exprs) {
        val = evaluate(child, env);
    }
    console.log(`Begin is about to return: ${val}`);
    return val;
}

let evalDefine = (defineExpr, env) => {
    // (define (name ...argNames) ...body)
    let [, [name, ...argNames], ...body] = defineExpr;
    let proc = {env, body, argNames};
    set(env, name, proc);
    return proc;
}

let evalApply = (applyExpr, env) => {
    // (identifierOrExpr ...argExprs)
    let [procExpr, ...argExprs] = applyExpr;
    let {env: staticEnv, argNames, body} = evaluate(procExpr, env);

    let argVals = argExprs.map(expr => evaluate(expr, env));

    let newEnv = extend(staticEnv);
    argVals.forEach((val, i) => {
        set(newEnv, argNames[i], val);
    });
    console.log(`evalApply: about to evaluate the body of ${procExpr}: ${JSON.stringify(body)}`)
    console.log(`  in the context of ${JSON.stringify(newEnv)}`);
    return evalBegin(["begin", ...body], newEnv);
}

let evalNativeOp = (nativeOpExpr, env) => {
    // (nativeOpName ...argExprs)
    let [nativeOpName, ...argExprs] = nativeOpExpr;
    let argVals = argExprs.map(expr => evaluate(expr, env));

    // TODO: cases where we have to translate JS<>lang vals
    return nativeOps[nativeOpName](...argVals);
}

// Environments
let set = (env, name, value) => {
    console.log(`Setting ${name} to ${JSON.stringify(value)} in ${inspectEnv(env)}}`);
    env.values.set(name, value);
}

let extend = (env) => {
    return { values: new Map(), parent: env };
}

let baseEnv = () => ({ parent: undefined, values: new Map() });

let lookup = (name, env) => {
    let currentEnv = env;
    while(currentEnv) {
        let val = currentEnv.values.get(name);
        if (val !== undefined) return val;
        currentEnv = currentEnv.parent;
    }
    throw new Error(`lookup failed at ${name} in ${inspectEnv(env)}`);
}

let inspectEnv = (env) => {
    return `Environment ${Array.from(env.values.entries())}. Parent: ${env.parent && ("\n" + inspectEnv(env.parent))}`;
}


// Sample program

console.log(evaluate(parseTree, baseEnv()));