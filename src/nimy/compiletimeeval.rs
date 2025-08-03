use crate::nimy::{
    cpunit::CompilationUnit,
    scope::InnerScope,
    trees::{NodeKind, ParseNode},
    values::{NimValue, parse_literal},
};

/// Evaluate a Nim expression
pub fn evaluate_compile_time_expression(
    node: &ParseNode,
    cpunit: &CompilationUnit,
    scope: &InnerScope,
) -> NimValue {
    // println!("Evaluating compile-time expression:\n{}", node.dbg_str());
    match node.kind {
        NodeKind::Identifier => {
            let identifier_name = node.to_str();
            if let Some(value) = parse_literal(&identifier_name) {
                return value;
            }
            NimValue::Unknown
        }
        NodeKind::IntegerLiteral => NimValue::Int(node.to_str().parse().unwrap_or(0)),
        NodeKind::InfixExpression => {
            let lhs = evaluate_compile_time_expression(&node.child(0).unwrap(), cpunit, scope);
            let rhs = evaluate_compile_time_expression(&node.child(2).unwrap(), cpunit, scope);
            let operator = node.child(1).unwrap().to_str();
            evaluate_infix(&lhs, &rhs, &operator)
        }
        NodeKind::Call => {
            let identifier = node.child(0).expect("at least one child").to_str();
            let args: Vec<NimValue> = node
                .get_by_kind(NodeKind::ArgumentList)
                .expect("argument list")
                .children()
                .filter(|c| c.kind != NodeKind::LParen && c.kind != NodeKind::RParen)
                .map(|node| evaluate_compile_time_expression(&node, cpunit, scope))
                .collect();
            evaluate_function_call(&identifier, &args, cpunit, scope)
        }
        _ => NimValue::Unknown,
    }
}

pub fn evaluate_function_call(
    fn_name: &str,
    _args: &[NimValue],
    _cpunit: &CompilationUnit,
    _scope: &InnerScope,
) -> NimValue {
    // Check for well-known function names.
    if fn_name == "defined" {
        return NimValue::Boolean(false);
    }

    NimValue::Unknown
}

pub fn evaluate_infix(lhs: &NimValue, rhs: &NimValue, operator: &str) -> NimValue {
    if matches!(lhs, NimValue::Unknown) || matches!(rhs, NimValue::Unknown) {
        return NimValue::Unknown;
    }
    // MARK: TODO typecheck
    match (lhs, rhs, operator) {
        (NimValue::Int(l), NimValue::Int(r), "+") => NimValue::Int(l + r),
        (NimValue::Float(l), NimValue::Float(r), "+") => NimValue::Float(l + r),
        (_, _, "==") => NimValue::Boolean(lhs == rhs),
        (_, _, "!=") => NimValue::Boolean(lhs != rhs),
        _ => NimValue::Nil,
    }
}
