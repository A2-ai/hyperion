use std::collections::BTreeMap;

use extendr_api::Result;
use extendr_api::prelude::*;
use extendr_api::serializer::to_robj;
use serde::Serialize;

use nmparser::{
    Model, OmegaSigmaEntry, OmegaSigmaParam, ParameterOrdering, ParsedOmegaComment,
    ParsedRaneffComment, ParsedSigmaComment, ParsedThetaComment, Transform, Type1Theta,
};

use crate::model::parameters::compare_param_names;
use crate::model::robj_to_model;
use hyperion_core::ResultExt;

#[derive(Debug, Clone, Default, Serialize)]
pub struct ThetaCommentInfo {
    pub name: Option<String>,
    pub unit: Option<String>,
    pub parameterization: Option<String>,
}

impl From<&ParsedThetaComment> for ThetaCommentInfo {
    fn from(parsed: &ParsedThetaComment) -> Self {
        match parsed {
            ParsedThetaComment::Type1(Type1Theta::WithUnit {
                parameter,
                unit,
                parametrization,
            }) => ThetaCommentInfo {
                name: Some(parameter.clone()),
                unit: Some(unit.clone()),
                parameterization: parametrization.as_deref().and_then(map_parameterization),
            },
            ParsedThetaComment::Type1(Type1Theta::Covariate { parameter }) => ThetaCommentInfo {
                name: Some(parameter.clone()),
                unit: None,
                parameterization: None,
            },
            ParsedThetaComment::Type1(Type1Theta::Type {
                typ,
                parameterization,
            }) => ThetaCommentInfo {
                name: Some(typ.clone()),
                unit: None,
                parameterization: map_parameterization(parameterization),
            },
            ParsedThetaComment::Type2(t) => ThetaCommentInfo {
                name: Some(t.name.clone()),
                unit: t.unit.clone(),
                parameterization: t.parameterization.as_ref().map(ToString::to_string),
            },
        }
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct OmegaCommentInfo {
    pub name: Option<String>,
    pub raw_name: Option<String>,
    pub associated_theta: Vec<String>,
    pub parameterization: Option<String>,
}

impl From<&ParsedOmegaComment> for OmegaCommentInfo {
    fn from(parsed: &ParsedOmegaComment) -> Self {
        match parsed {
            ParsedOmegaComment::Type1(o) => OmegaCommentInfo {
                name: parsed.name(),
                raw_name: Some(o.name.clone()),
                associated_theta: vec![o.theta_name.clone()],
                parameterization: map_parameterization(&o.parameterization),
            },
            ParsedOmegaComment::Type2(o) => OmegaCommentInfo {
                name: parsed.name(),
                raw_name: Some(o.name.clone()),
                associated_theta: o.raw_theta_refs.clone(),
                parameterization: o.parameterization.as_ref().map(ToString::to_string),
            },
        }
    }
}

#[derive(Debug, Clone, Default, Serialize)]
pub struct SigmaCommentInfo {
    pub name: Option<String>,
    pub unit: Option<String>,
    pub parameterization: Option<String>,
}

impl From<&ParsedSigmaComment> for SigmaCommentInfo {
    fn from(parsed: &ParsedSigmaComment) -> Self {
        match parsed {
            ParsedSigmaComment::Type1(s) => SigmaCommentInfo {
                name: Some(s.name.clone()),
                unit: None,
                parameterization: s.parameterization.as_deref().and_then(map_parameterization),
            },
            ParsedSigmaComment::Type2(s) => SigmaCommentInfo {
                name: Some(s.name.clone()),
                unit: s.unit.clone(),
                parameterization: s.parameterization.as_ref().map(ToString::to_string),
            },
        }
    }
}

#[derive(Debug, Clone, Serialize)]
pub struct ModelCommentInfo {
    pub thetas: Vec<(String, ThetaCommentInfo)>,
    pub omegas: Vec<(String, OmegaCommentInfo)>,
    pub sigmas: Vec<(String, SigmaCommentInfo)>,
}

fn build_theta_info(parsed: Option<&ParsedThetaComment>) -> ThetaCommentInfo {
    parsed.map(ThetaCommentInfo::from).unwrap_or_default()
}

fn build_omega_info(param: &OmegaSigmaParam) -> OmegaCommentInfo {
    let inner = match param.parsed_comment.as_ref() {
        Some(ParsedRaneffComment::Omega(o)) => Some(o),
        Some(ParsedRaneffComment::Sigma(_)) => {
            panic!("pharos invariant: omega block parameter has Sigma parsed comment")
        }
        None => None,
    };

    inner.map(OmegaCommentInfo::from).unwrap_or_default()
}

fn build_sigma_info(param: &OmegaSigmaParam) -> SigmaCommentInfo {
    let inner = match param.parsed_comment.as_ref() {
        Some(ParsedRaneffComment::Sigma(s)) => Some(s),
        Some(ParsedRaneffComment::Omega(_)) => {
            panic!("pharos invariant: sigma block parameter has Omega parsed comment")
        }
        None => None,
    };

    inner.map(SigmaCommentInfo::from).unwrap_or_default()
}

fn sort_entries<T>(entries: BTreeMap<String, T>) -> Vec<(String, T)> {
    let mut out: Vec<(String, T)> = entries.into_iter().collect();
    out.sort_by(|a, b| compare_param_names(&a.0, &b.0));
    out
}

/// Walk a model's parameters and produce per-parameter comment info.
pub fn build(model: &Model) -> anyhow::Result<ModelCommentInfo> {
    let mut thetas: BTreeMap<String, ThetaCommentInfo> = BTreeMap::new();
    for (i, theta) in model.thetas.iter().enumerate() {
        let key = format!("THETA{}", i + 1);
        thetas.insert(key, build_theta_info(theta.parsed_comment.as_ref()));
    }

    let omega_entries: Vec<OmegaSigmaEntry> =
        model.get_omega_parameters(ParameterOrdering::RowMajor)?;
    let mut omegas: BTreeMap<String, OmegaCommentInfo> = BTreeMap::new();
    for entry in omega_entries {
        let info = build_omega_info(&entry.parameter);
        omegas.insert(entry.param_name, info);
    }

    let sigma_entries: Vec<OmegaSigmaEntry> =
        model.get_sigma_parameters(ParameterOrdering::RowMajor)?;
    let mut sigmas: BTreeMap<String, SigmaCommentInfo> = BTreeMap::new();
    for entry in sigma_entries {
        let info = build_sigma_info(&entry.parameter);
        sigmas.insert(entry.param_name, info);
    }

    Ok(ModelCommentInfo {
        thetas: sort_entries(thetas),
        omegas: sort_entries(omegas),
        sigmas: sort_entries(sigmas),
    })
}

/// Build per-parameter comment info from a model object (internal)
///
/// @param model hyperion_nonmem_model object from read_model()
///
/// @return list with `thetas`, `omegas`, `sigmas` entries; each is a list of
///   length-2 lists `(coordinate, info)` in numeric coordinate order.
/// @keywords internal
#[extendr]
pub fn get_model_comment_info(model: Robj) -> Result<Robj> {
    let model = robj_to_model(&model)?;
    let info = build(&model).map_to_extendr_err("Failed to build comment info")?;
    to_robj(&info).map_to_extendr_err("Failed to serialize comment info")
}

/// Canonicalize a parameterization alias to its PascalCase form.
///
/// @param raw Parameterization alias (e.g. `"EXP"`, `"lognormal"`, `"PROP"`).
/// @return Canonical name (`"LogNormal"`, `"Proportional"`, ...) or `NA_character_`
///   if `raw` is not a recognized alias.
/// @keywords internal
#[extendr]
pub fn map_parameterization(raw: &str) -> Option<String> {
    raw.parse::<Transform>().ok().map(|t| t.to_string())
}

extendr_module! {
    mod comment_info;

    fn get_model_comment_info;
    fn map_parameterization;
}
