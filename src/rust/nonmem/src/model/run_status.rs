use std::fmt;
use std::path::{Path, PathBuf};

use extendr_api::Result;
use extendr_api::prelude::*;
use fs_err as fs;

use hyperion_core::{OptionExt, extendr_err};
use nonmem::Model;
use nonmem::output_files::ext::ExtReader;

use crate::utils::{find_output_file, path_from_robj, resolve_ext_path};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RunStatus {
    Run,
    Running,
    NotRun,
}

impl fmt::Display for RunStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            RunStatus::Run => "run",
            RunStatus::Running => "running",
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

    let ext_path = resolved_ext_for_run(path, &run_dir, &stem);
    let lst_path = run_dir.join(format!("{}.lst", stem));

    let ext_exists = ext_path.exists();
    let lst_exists = lst_path.exists();

    if !ext_exists && !lst_exists {
        return Ok(RunStatus::NotRun);
    }

    if ext_exists && lst_exists {
        let ext_reader = ExtReader::default().final_estimates_only();
        if let Ok(tables) = ext_reader.parse_file(&ext_path)
            && tables.iter().any(|t| !t.rows.is_empty())
        {
            return Ok(RunStatus::Run);
        }
    }

    Ok(RunStatus::Running)
}

/// Resolve the .ext path for a run, honoring `$EST FILE=` when the source
/// model can be parsed. Falls back to `{stem}.ext` in `run_dir` otherwise.
fn resolved_ext_for_run(input_path: &Path, run_dir: &Path, stem: &str) -> PathBuf {
    let default = run_dir.join(format!("{stem}.ext"));

    // Locate the source .mod/.ctl. For .mod/.ctl input use it directly; for
    // .lst input (which lives inside run_dir) the source is one level up.
    let source = match input_path.extension().and_then(|e| e.to_str()) {
        Some("mod") | Some("ctl") => input_path.to_path_buf(),
        _ => match run_dir.parent() {
            Some(p) => {
                let mod_candidate = p.join(format!("{stem}.mod"));
                if mod_candidate.exists() {
                    mod_candidate
                } else {
                    p.join(format!("{stem}.ctl"))
                }
            }
            None => return default,
        },
    };

    let Ok(content) = fs::read_to_string(&source) else {
        return default;
    };
    let Ok(model) = Model::parse(&content) else {
        return default;
    };
    resolve_ext_path(&model, run_dir, stem)
}

/// Determine run status for a model path, run directory, or model object.
///
/// @param input A hyperion_nonmem_model object, run directory, or model path.
/// @return "run", "running", or "not_run"
///
/// Accepts .mod/.ctl/.lst paths, run directories, or a hyperion_nonmem_model object.
#[extendr]
pub fn get_run_status(input: Robj) -> Result<Robj> {
    let mut path = path_from_robj(&input, false)?;

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
    use std::path::PathBuf;
    use tempfile::TempDir;

    fn test_data_dir() -> PathBuf {
        PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("test_data")
    }

    #[test]
    fn test_determine_run_status_run() {
        let lst_file = test_data_dir().join("run001/run001.lst");
        let status = determine_run_status(&lst_file).unwrap();
        assert_eq!(status, RunStatus::Run);
    }

    #[test]
    fn test_determine_run_status_running() {
        let lst_file = test_data_dir().join("run001-running/run001.lst");
        let status = determine_run_status(&lst_file).unwrap();
        assert_eq!(status, RunStatus::Running);
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
    fn test_determine_run_status_running_early() {
        // NONMEM creates .lst before .ext during early execution
        let temp_dir = TempDir::new().unwrap();
        let mod_file = temp_dir.path().join("run001.mod");
        fs::write(&mod_file, "test content").unwrap();

        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        fs::write(run_dir.join("run001.lst"), "lst content").unwrap();

        let status = determine_run_status(&mod_file).unwrap();
        assert_eq!(status, RunStatus::Running);
    }

    #[test]
    fn test_determine_run_status_not_run_empty_run_dir() {
        // Run directory exists but contains no output files
        let temp_dir = TempDir::new().unwrap();
        let mod_file = temp_dir.path().join("run001.mod");
        fs::write(&mod_file, "test content").unwrap();

        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();

        let status = determine_run_status(&mod_file).unwrap();
        assert_eq!(status, RunStatus::NotRun);
    }
}
