use std::fmt;
use std::path::Path;

use extendr_api::Result;
use extendr_api::prelude::*;

use hyperion_core::{OptionExt, extendr_err};

use crate::utils::{find_output_file, from_config_relative, path_from_robj};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunStatus {
    Run,
    NotRun,
}

impl fmt::Display for RunStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            RunStatus::Run => "run",
            RunStatus::NotRun => "not_run",
        };
        f.write_str(value)
    }
}

pub fn determine_run_status(path: impl AsRef<Path>) -> Result<RunStatus> {
    let path = path.as_ref();
    let stem = path
        .file_stem()
        .ok_or_extendr_err("Could not determine model file stem")?
        .to_string_lossy()
        .to_string();
    let parent = path
        .parent()
        .ok_or_extendr_err("Could not determine model file parent directory")?;
    let run_dir = match path.extension().and_then(|e| e.to_str()) {
        Some("lst") => parent.to_path_buf(),
        _ => parent.join(&stem),
    };

    if !run_dir.exists() {
        return Ok(RunStatus::NotRun);
    }

    let ext_path = run_dir.join(format!("{}.ext", stem));
    let lst_path = run_dir.join(format!("{}.lst", stem));

    if ext_path.exists() && lst_path.exists() {
        Ok(RunStatus::Run)
    } else {
        Ok(RunStatus::NotRun)
    }
}

/// Determine run status for a model path, run directory, or model object.
///
/// @param input A hyperion_nonmem_model object, run directory, or model path.
/// @return "run" or "not_run"
///
/// Accepts .mod/.ctl/.lst paths, run directories, or a hyperion_nonmem_model object.
#[extendr]
pub fn get_run_status(input: Robj) -> Result<Robj> {
    let mut path = if input.inherits("hyperion_nonmem_model") {
        let source = input
            .get_attrib("model_source")
            .ok_or_extendr_err("Model object is missing model_source attribute")?;
        let source_str = source
            .as_str()
            .ok_or_extendr_err("model_source attribute must be a string")?;
        from_config_relative(source_str)?
    } else {
        path_from_robj(&input)?
    };

    if path.is_dir() {
        // Prefer lst in run directory; fall back to mod/ctl when present.
        if let Ok(p) = find_output_file(&path, "lst") {
            path = p;
        } else if let Ok(p) = find_output_file(&path, "mod") {
            path = p;
        } else if let Ok(p) = find_output_file(&path, "ctl") {
            path = p;
        } else {
            return Err(extendr_err!(
                "No run outputs found in directory: {}",
                path.display()
            ));
        }
    }

    let status = determine_run_status(&path)?;
    Ok(status.to_string().into_robj())
}

extendr_module! {
   mod run_status;

    fn get_run_status;
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    #[test]
    fn test_determine_run_status_run() {
        let temp_dir = TempDir::new().unwrap();
        let mod_file = temp_dir.path().join("run001.mod");
        fs::write(&mod_file, "test content").unwrap();

        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        let ext_file = run_dir.join("run001.ext");
        fs::write(&ext_file, "test content").unwrap();
        let lst_file = run_dir.join("run001.lst");
        fs::write(&lst_file, "test content").unwrap();

        let status = determine_run_status(&mod_file).unwrap();
        assert_eq!(status, RunStatus::Run);
    }

    #[test]
    fn test_determine_run_status_not_run() {
        let temp_dir = TempDir::new().unwrap();
        let mod_file = temp_dir.path().join("run001.mod");
        fs::write(&mod_file, "test content").unwrap();

        let status = determine_run_status(&mod_file).unwrap();
        assert_eq!(status, RunStatus::NotRun);
    }

    #[test]
    fn test_determine_run_status_lst_file() {
        let temp_dir = TempDir::new().unwrap();
        let lst_file = temp_dir.path().join("run001.lst");
        fs::write(&lst_file, "test content").unwrap();
        let ext_file = temp_dir.path().join("run001.ext");
        fs::write(&ext_file, "test content").unwrap();

        // For .lst files, the run_dir is the parent directory itself
        // which exists, so status should be Run
        let status = determine_run_status(&lst_file).unwrap();
        assert_eq!(status, RunStatus::Run);
    }
}
