use std::fmt;
use std::path::Path;

use extendr_api::Result;
use extendr_api::prelude::*;
use fs_err as fs;

use hyperion_core::{OptionExt, extendr_err};

use crate::utils::{find_output_file, path_from_robj};

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

/// Determine the run status from on-disk outputs.
///
/// `Run` requires the `.lst` to contain NONMEM's "Stop Time:" marker, which is
/// written at run termination regardless of whether estimation or covariance
/// succeeded. `Running` means the `.lst` exists but the marker is absent.
/// `NotRun` means neither the run directory nor the `.lst` exists.
pub fn determine_run_status(run_dir: &Path, stem: &str) -> Result<RunStatus> {
    if !run_dir.exists() {
        return Ok(RunStatus::NotRun);
    }
    let lst_path = run_dir.join(format!("{stem}.lst"));
    if !lst_path.exists() {
        return Ok(RunStatus::NotRun);
    }
    if lst_indicates_completion(&lst_path) {
        return Ok(RunStatus::Run);
    }
    Ok(RunStatus::Running)
}

fn lst_indicates_completion(lst_path: &Path) -> bool {
    let Ok(content) = fs::read_to_string(lst_path) else {
        return false;
    };
    content
        .lines()
        .rev()
        .any(|line| line.trim_start().starts_with("Stop Time:"))
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

    let status = determine_run_status(&run_dir, &stem)?;
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
        let run_dir = test_data_dir().join("run001");
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::Run);
    }

    #[test]
    fn test_determine_run_status_running() {
        let run_dir = test_data_dir().join("run001-running");
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::Running);
    }

    #[test]
    fn test_determine_run_status_not_run() {
        // run_dir doesn't exist
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::NotRun);
    }

    #[test]
    fn test_determine_run_status_running_early() {
        // .lst exists without "Stop Time:" => still running
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        fs::write(run_dir.join("run001.lst"), "partial lst content\n").unwrap();
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::Running);
    }

    #[test]
    fn test_determine_run_status_not_run_empty_run_dir() {
        // run_dir exists but no .lst inside
        let temp_dir = TempDir::new().unwrap();
        let run_dir = temp_dir.path().join("run001");
        fs::create_dir(&run_dir).unwrap();
        let status = determine_run_status(&run_dir, "run001").unwrap();
        assert_eq!(status, RunStatus::NotRun);
    }
}
