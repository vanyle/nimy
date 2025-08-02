use crate::nimy::{
    cpunit::CompilationUnit,
    scope::InnerScope,
    trees::{NodeKind, ParseNode},
    values::{NimValue, parse_literal},
};

/// Evaluate a Nim expression
pub fn evaluate_compile_time_expression(
    node: &ParseNode,
    _cpunit: &CompilationUnit,
    _scope: &InnerScope,
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
            let lhs = evaluate_compile_time_expression(&node.child(0).unwrap(), _cpunit, _scope);
            let rhs = evaluate_compile_time_expression(&node.child(2).unwrap(), _cpunit, _scope);
            let operator = node.child(1).unwrap().to_str();
            evaluate_infix(&lhs, &rhs, &operator)
        }
        _ => NimValue::Unknown,
    }
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
