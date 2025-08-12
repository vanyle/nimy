use std::rc::Rc;

use crate::nimy::{
    cpunit::CompilationUnit,
    generics::{self, GenericProc},
    namedprocs::NimProc,
    scope::InnerScope,
    symbols::Symbol,
    trees::{self, NodeKind},
    typeparser::parse_type_expression,
    types,
};

pub fn handle_proc_declaration(
    node: &trees::ParseNode,
    scope: &mut InnerScope,
    cpunit: &CompilationUnit,
) {
    let maybe_exported = node.child(1).expect("at least 2 children in proc decl");
    let is_exported = node.extract_by_kind(NodeKind::ExportedSymbol).is_some();

    let proc_name_sym_node = maybe_exported
        .extract_by_kind(NodeKind::Identifier)
        .unwrap_or(maybe_exported);

    let generic_params = node.get_by_kind(NodeKind::GenericParameterList);
    let generic_arguments = if let Some(generic_params) = generic_params {
        generics::parse_generic_param_list(generic_params, |n| {
            parse_type_expression(&n, cpunit, scope, &[])
        })
    } else {
        vec![]
    };

    let parameters = node.get_by_kind(NodeKind::ParameterDeclarationList);
    let parameters = match parameters {
        None => vec![],
        Some(parameters_node) => parameters_node
            .children()
            .filter(|n| n.kind == NodeKind::ParameterDeclaration)
            .filter_map(|param_decl_node| {
                // for now, extract just the types of the argument, not the names.
                param_decl_node.get_by_kind(NodeKind::TypeExpression)
            })
            .map(|type_expr_nodes| {
                parse_type_expression(&type_expr_nodes, cpunit, scope, &generic_arguments)
                    .unwrap_or(Rc::new(types::to_undefined_type(&type_expr_nodes)))
            })
            .collect::<Vec<_>>(),
    };

    let return_type_node = node
        .children()
        .skip_while(|n| n.kind != NodeKind::Colon)
        .nth(1);
    let return_type =
        return_type_node.and_then(|node| parse_type_expression(&node, cpunit, scope, &[]));

    let base_proc = Rc::new(NimProc {
        sym: Symbol::from_node(&proc_name_sym_node, is_exported),
        nimtype: types::NimProcType {
            arguments: parameters,
            return_type,
        },
    });

    if generic_arguments.len() > 0 {
        scope.generic_procs.push(Rc::new(GenericProc {
            underlying_proc: base_proc,
            generics: generic_arguments,
        }));
    } else {
        scope.procs.push(base_proc);
    }
}
