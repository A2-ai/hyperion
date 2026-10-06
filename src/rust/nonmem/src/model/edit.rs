//! R entry points for model edits. Each takes the model's current source
//! text, applies one edit through pharos, and returns the re-parsed model.
//! The R wrappers carry the edit-session attributes (refs, unsaved-edit
//! count, copy origin) across.

use std::collections::HashMap;

use extendr_api::Result;
use extendr_api::prelude::*;
use nmparser::{
    Change, CodeRecord, FilterKind, Model, NewRow, NewTheta, OptionEdit, ParamRef, RandomKind,
    RefKind, RowUpdate, ThetaUpdate, resolve_placeholders,
};

use crate::model::model_to_robj;
use crate::utils::get_strict_comment_type;
use hyperion_core::{ResultExt, extendr_err};

fn parse_source(source: &str) -> Result<Model> {
    Model::parse("model", source).map_to_extendr_err("Failed to parse the model")
}

fn code_record(record: &str) -> Result<CodeRecord> {
    match record {
        "PK" => Ok(CodeRecord::Pk),
        "ERROR" => Ok(CodeRecord::Error),
        "DES" => Ok(CodeRecord::Des),
        other => Err(extendr_err!("Unsupported code record `{other}`.")),
    }
}

fn parse_refs(
    names: Vec<String>,
    kinds: Vec<String>,
    indices: Vec<i32>,
    cols: Vec<i32>,
) -> Result<HashMap<String, ParamRef>> {
    names
        .into_iter()
        .zip(kinds)
        .zip(indices)
        .zip(cols)
        .map(|(((name, kind), index), col)| {
            let kind = match kind.as_str() {
                "theta" => RefKind::Theta,
                "omega" => RefKind::Omega,
                "sigma" => RefKind::Sigma,
                other => return Err(extendr_err!("Unknown ref kind `{other}`.")),
            };
            Ok((
                name,
                ParamRef {
                    kind,
                    index: index as usize,
                    col: (col > 0).then_some(col as usize),
                },
            ))
        })
        .collect()
}

fn edit_err(e: anyhow::Error) -> Error {
    extendr_err!("{e}")
}

fn random_kind(kind: &str) -> Result<RandomKind> {
    match kind {
        "omega" => Ok(RandomKind::Omega),
        "sigma" => Ok(RandomKind::Sigma),
        other => Err(extendr_err!("Unknown parameter kind `{other}`.")),
    }
}

/// `action` is "keep", "set" or "remove"; "set" needs `value`.
fn change<T>(action: &str, value: Option<T>) -> Result<Change<T>> {
    match (action, value) {
        ("keep", _) => Ok(Change::Keep),
        ("remove", _) => Ok(Change::Remove),
        ("set", Some(v)) => Ok(Change::Set(v)),
        _ => Err(extendr_err!("Invalid change `{action}`.")),
    }
}

/// Record options from R: `actions` is "value", "flag" or "remove" per option.
fn option_edits(
    names: Vec<String>,
    actions: Vec<String>,
    values: Vec<String>,
) -> Result<Vec<(String, OptionEdit)>> {
    names
        .into_iter()
        .zip(actions)
        .zip(values)
        .map(|((name, action), value)| {
            let edit = match action.as_str() {
                "value" => OptionEdit::Value(value),
                "flag" => OptionEdit::Flag,
                "remove" => OptionEdit::Remove,
                other => return Err(extendr_err!("Invalid option action `{other}`.")),
            };
            Ok((name, edit))
        })
        .collect()
}

fn to_robj(mut model: Model, path: &str) -> Result<Robj> {
    model_to_robj(&mut model, path)
}

/// Append a THETA row (internal)
///
/// @return list(model = <hyperion_nonmem_model>, index = <new THETA number>)
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn edit_add_theta_impl(
    source: &str,
    path: &str,
    init: f64,
    #[extendr(default = "NULL")] lower: Option<f64>,
    #[extendr(default = "NULL")] upper: Option<f64>,
    #[extendr(default = "FALSE")] fix: bool,
    #[extendr(default = "NULL")] comment: Option<String>,
    #[extendr(default = "NULL")] index: Option<i32>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let theta = NewTheta {
        init,
        lower,
        upper,
        fix,
        comment,
    };
    let (mut edited, index) = model
        .add_theta(&theta, index.map(|i| i as usize), get_strict_comment_type())
        .map_err(edit_err)?;
    let robj = model_to_robj(&mut edited, path)?;
    Ok(list!(model = robj, index = index as i32).into())
}

/// Edit a code record (internal)
///
/// With `lhs` (or `mu_of`), `append` is added to the end of that statement's
/// right-hand side (or the end of the `within` call), and the statement's
/// comment is changed per `comment_action` ("keep", "set" or "remove").
/// Without, each element of `append` is added as a new statement.
///
/// @return list(model = <hyperion_nonmem_model>, used = <ref names used>)
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn edit_code_impl(
    source: &str,
    path: &str,
    record: &str,
    append: Vec<String>,
    #[extendr(default = "NULL")] lhs: Option<String>,
    #[extendr(default = "NULL")] mu_of: Option<String>,
    #[extendr(default = "NULL")] within: Option<String>,
    ref_names: Vec<String>,
    ref_kinds: Vec<String>,
    ref_indices: Vec<i32>,
    ref_cols: Vec<i32>,
    #[extendr(default = "'keep'")] comment_action: &str,
    #[extendr(default = "NULL")] comment: Option<String>,
    #[extendr(default = "NULL")] replace: Option<String>,
) -> Result<Robj> {
    let comment = change(comment_action, comment)?;
    let record = code_record(record)?;
    let refs = parse_refs(ref_names, ref_kinds, ref_indices, ref_cols)?;
    let mut model = parse_source(source)?;

    let mut used: Vec<String> = vec![];
    let mut resolve = |text: &str| -> Result<String> {
        let (r, u) = resolve_placeholders(text, &refs).map_err(edit_err)?;
        for name in u {
            if !used.contains(&name) {
                used.push(name);
            }
        }
        Ok(r)
    };
    let resolved = append
        .iter()
        .map(|t| resolve(t))
        .collect::<Result<Vec<_>>>()?;
    let replace = replace.map(|t| resolve(&t)).transpose()?;

    let target = match (lhs, mu_of) {
        (Some(l), None) => Some(l),
        (None, Some(p)) => Some(model.mu_of(&p).map_err(edit_err)?),
        (None, None) => None,
        (Some(_), Some(_)) => return Err(extendr_err!("Give `lhs` or `mu_of`, not both.")),
    };

    match target {
        Some(lhs) => {
            if let Some(text) = &replace {
                if !resolved.is_empty() || within.is_some() {
                    return Err(extendr_err!(
                        "`replace` swaps the whole right-hand side; don't combine it with `append` or `within`."
                    ));
                }
                model = model
                    .replace_statement(record, &lhs, text)
                    .map_err(edit_err)?;
            }
            match resolved.as_slice() {
                [] if within.is_some() => {
                    return Err(extendr_err!("`within` needs `append`."));
                }
                [] => {}
                [text] => {
                    model = model
                        .append_to_statement(record, &lhs, within.as_deref(), text)
                        .map_err(edit_err)?;
                }
                _ => {
                    return Err(extendr_err!(
                        "`append` must be a single string when adding to an existing statement."
                    ));
                }
            }
            model = model
                .set_statement_comment(record, &lhs, &comment)
                .map_err(edit_err)?;
        }
        None => {
            if within.is_some() {
                return Err(extendr_err!("`within` needs a target statement."));
            }
            if comment != Change::Keep {
                return Err(extendr_err!("`comment` needs a target statement."));
            }
            if replace.is_some() {
                return Err(extendr_err!("`replace` needs a target statement."));
            }
            for line in &resolved {
                model = model.add_statement(record, line).map_err(edit_err)?;
            }
        }
    }

    let robj = model_to_robj(&mut model, path)?;
    Ok(list!(model = robj, used = used).into())
}

/// Append a diagonal OMEGA or SIGMA row (internal)
///
/// @return list(model = <hyperion_nonmem_model>, index = <new ETA/EPS number>)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_add_random_impl(
    source: &str,
    path: &str,
    kind: &str,
    init: f64,
    #[extendr(default = "FALSE")] fix: bool,
    #[extendr(default = "NULL")] comment: Option<String>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let row = NewRow { init, fix, comment };
    let (edited, index) = model
        .add_random(random_kind(kind)?, &row, get_strict_comment_type())
        .map_err(edit_err)?;
    Ok(list!(model = to_robj(edited, path)?, index = index as i32).into())
}

/// Remove a THETA row (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_remove_theta_impl(source: &str, path: &str, index: i32) -> Result<Robj> {
    let model = parse_source(source)?;
    to_robj(model.remove_theta(index as usize).map_err(edit_err)?, path)
}

/// Remove a diagonal OMEGA or SIGMA row (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_remove_random_impl(source: &str, path: &str, kind: &str, index: i32) -> Result<Robj> {
    let model = parse_source(source)?;
    let edited = model
        .remove_random(random_kind(kind)?, index as usize)
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Update one THETA (internal)
///
/// `*_action` is "keep", "set" or "remove".
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn edit_update_theta_impl(
    source: &str,
    path: &str,
    index: i32,
    init: Option<f64>,
    lower_action: &str,
    lower: Option<f64>,
    upper_action: &str,
    upper: Option<f64>,
    fix: Option<bool>,
    comment_action: &str,
    comment: Option<String>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let update = ThetaUpdate {
        init,
        lower: change(lower_action, lower)?,
        upper: change(upper_action, upper)?,
        fix,
        comment: change(comment_action, comment)?,
    };
    let edited = model
        .update_theta(index as usize, &update, get_strict_comment_type())
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Update one diagonal OMEGA or SIGMA (internal)
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn edit_update_random_impl(
    source: &str,
    path: &str,
    kind: &str,
    index: i32,
    init: Option<f64>,
    fix: Option<bool>,
    comment_action: &str,
    comment: Option<String>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let update = RowUpdate {
        init,
        fix,
        comment: change(comment_action, comment)?,
    };
    let edited = model
        .update_random(
            random_kind(kind)?,
            index as usize,
            &update,
            get_strict_comment_type(),
        )
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Make an OMEGA block (internal)
///
/// `init` and `comment` are lower-triangle; NA keeps the existing element.
/// @keywords internal
/// @noRd
#[extendr]
#[allow(clippy::too_many_arguments)]
pub fn edit_add_omega_block_impl(
    source: &str,
    path: &str,
    first: i32,
    size: i32,
    init: Doubles,
    comment: Strings,
    fix: bool,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let init: Vec<Option<f64>> = init.iter().map(Option::<f64>::from).collect();
    let comment: Vec<Option<String>> = comment
        .iter()
        .map(|c| if c.is_na() { None } else { Some(c.to_string()) })
        .collect();
    let edited = model
        .add_omega_block(
            first as usize,
            size as usize,
            &init,
            &comment,
            fix,
            get_strict_comment_type(),
        )
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Change `$EST` options (internal)
///
/// `actions` is "value", "flag" or "remove" per option.
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_update_est_impl(
    source: &str,
    path: &str,
    index: i32,
    names: Vec<String>,
    actions: Vec<String>,
    values: Vec<String>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let edits = option_edits(names, actions, values)?;
    let edited = model.update_est(index as usize, &edits).map_err(edit_err)?;
    to_robj(edited, path)
}

/// Change `$SUBROUTINES` (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_update_subroutines_impl(
    source: &str,
    path: &str,
    advan: Option<i32>,
    trans_action: &str,
    trans: Option<i32>,
    tol_action: &str,
    tol: Option<i32>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let opt = |action: &str, v: Option<i32>| -> Option<Option<u32>> {
        match action {
            "set" => Some(v.map(|n| n as u32)),
            "remove" => Some(None),
            _ => None,
        }
    };
    let edited = model
        .update_subroutines(
            advan.map(|n| n as u32),
            opt(trans_action, trans),
            opt(tol_action, tol),
        )
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Remove `$COV` (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_remove_cov_impl(source: &str, path: &str) -> Result<Robj> {
    let edited = parse_source(source)?.remove_cov().map_err(edit_err)?;
    to_robj(edited, path)
}

/// Set the `$DATA` path (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_update_data_impl(source: &str, path: &str, data_path: &str) -> Result<Robj> {
    let edited = parse_source(source)?
        .update_data(data_path)
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Append lines to `$MODEL` (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_update_model_record_impl(
    source: &str,
    path: &str,
    append: Vec<String>,
) -> Result<Robj> {
    let edited = parse_source(source)?
        .update_model_record(&append)
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Add `$TABLE` columns (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_update_table_impl(
    source: &str,
    path: &str,
    file: Option<String>,
    index: Option<i32>,
    append: Vec<String>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let i = model
        .table_index(file.as_deref(), index.map(|i| i as usize))
        .map_err(edit_err)?;
    let edited = model.update_table(i, &append).map_err(edit_err)?;
    to_robj(edited, path)
}

/// Rename a variable (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_rename_variable_impl(source: &str, path: &str, from: &str, to: &str) -> Result<Robj> {
    let edited = parse_source(source)?
        .rename_variable(from, to)
        .map_err(edit_err)?;
    to_robj(edited, path)
}

/// Add or remove `$DATA` IGNORE/ACCEPT conditions (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_data_filter_impl(
    source: &str,
    path: &str,
    kind: &str,
    action: &str,
    conditions: Vec<String>,
) -> Result<Robj> {
    let kind = match kind {
        "ignore" => FilterKind::Ignore,
        "accept" => FilterKind::Accept,
        other => return Err(extendr_err!("Unknown filter kind `{other}`.")),
    };
    let mut model = parse_source(source)?;
    for cond in &conditions {
        model = match action {
            "add" => model.add_data_filter(kind, cond),
            "remove" => model.remove_data_filter(kind, cond),
            other => return Err(extendr_err!("Unknown filter action `{other}`.")),
        }
        .map_err(edit_err)?;
    }
    to_robj(model, path)
}

/// Add a `$EST` record (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_add_est_impl(
    source: &str,
    path: &str,
    names: Vec<String>,
    actions: Vec<String>,
    values: Vec<String>,
) -> Result<Robj> {
    let model = parse_source(source)?;
    let options = option_edits(names, actions, values)?;
    to_robj(model.add_est(&options).map_err(edit_err)?, path)
}

/// Remove a `$EST` record (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_remove_est_impl(source: &str, path: &str, index: i32) -> Result<Robj> {
    let model = parse_source(source)?;
    to_robj(model.remove_est(index as usize).map_err(edit_err)?, path)
}

/// Names assigned in a code record, upper case (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_assigned_names_impl(source: &str, record: &str) -> Result<Vec<String>> {
    let model = parse_source(source)?;
    model.assigned_names(code_record(record)?).map_err(edit_err)
}

/// `$MODEL` compartments without a `DADT` in `$DES` (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_missing_dadt_impl(source: &str) -> Result<Vec<i32>> {
    let model = parse_source(source)?;
    Ok(model.missing_dadt().into_iter().map(|n| n as i32).collect())
}

/// Number of `$EST` records (internal)
/// @keywords internal
/// @noRd
#[extendr]
pub fn edit_est_count_impl(source: &str) -> Result<i32> {
    Ok(parse_source(source)?.estimations.len() as i32)
}

extendr_module! {
    mod edit;
    fn edit_add_theta_impl;
    fn edit_code_impl;
    fn edit_add_random_impl;
    fn edit_remove_theta_impl;
    fn edit_remove_random_impl;
    fn edit_update_theta_impl;
    fn edit_update_random_impl;
    fn edit_add_omega_block_impl;
    fn edit_update_est_impl;
    fn edit_update_subroutines_impl;
    fn edit_remove_cov_impl;
    fn edit_update_data_impl;
    fn edit_update_model_record_impl;
    fn edit_update_table_impl;
    fn edit_rename_variable_impl;
    fn edit_assigned_names_impl;
    fn edit_data_filter_impl;
    fn edit_add_est_impl;
    fn edit_remove_est_impl;
    fn edit_missing_dadt_impl;
    fn edit_est_count_impl;
}
